{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Four where

--                             LIBS                            --
------------------------------------------------------------------------
import Data.Char ()
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Maybe as Maybe ( Maybe(Just, Nothing), fromJust )
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering), hSetEncoding, utf8, stdout)
import System.Random (getStdGen, Random (randomRs), randomRIO, randomIO)
import Control.Monad.State (lift, StateT)
import Control.Lens ( (^?), element, (^.))
import Control.Lens.Tuple ()
import Unicode.Char ()
import Text.Show.Unicode ()
import Control.Concurrent ( threadDelay )
import System.Console.ANSI ( setCursorPosition, clearScreen, setSGR, SGR (SetColor, Reset), ConsoleLayer (Background, Foreground), ColorIntensity (Vivid, Dull), Color (Blue, Red, Yellow) )
import Text.Read (readMaybe)




--                             TYPES/DATA                            --
------------------------------------------------------------------------

type Size = Int
data Disc = X | O | E deriving (Eq, Read, Show)
type Row = [Disc]
type Board = [[Disc]]
data Maybe a = Nothing | Just a
data Game = Game
  { _gPlayerScore   :: Int
  , _gComputerScore :: Int
  , _gPlayerOneBalance :: Double
  , _gPlayerTwoBalance :: Double
  } deriving Show




--                            PURE FUNCTIONS                         --
------------------------------------------------------------------------

createBoard :: Size -> Size -> Board
createBoard m n = replicate m (replicate n E)

switchPlayer :: Disc -> Disc
switchPlayer X = O
switchPlayer O = X

toDisc :: Char -> Disc
toDisc 'X' = X
toDisc 'O' = O

showPlayerName :: Disc -> String
showPlayerName X = "Player 1"
showPlayerName O = "Player 2"

playerOneInput :: IO () -> Int -> Row -> [Disc]
playerOneInput p n r = toList $ Seq.update n X (Seq.fromList r)

playerTwoInput :: IO () -> Int -> Row -> [Disc]
playerTwoInput p n r = toList $ Seq.update n O (Seq.fromList r)

-- takes a reverse board
updateBoard :: Disc -> Int -> Board -> Board
updateBoard p i [] = []
updateBoard p i (xs : xss) = case xs ^? element i of
    Maybe.Nothing -> xs : xss
    Maybe.Just E  -> if p == X then playerOneInput (showDisc p) i xs : xss else playerTwoInput (showDisc p) i xs : xss
    Maybe.Just X  -> xs : updateBoard p i xss
    Maybe.Just O  -> xs : updateBoard p i xss


-- HELPER FUNCTIONS --
----------------------
filterE :: [Disc] -> [Disc]
filterE y = filter (== E) y

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs

columnHasEmptySlot :: [[Disc]] -> Int -> Bool
columnHasEmptySlot b i = find E $ map (!!i) b

-- getEmptySlots :: Board -> [Int]
-- getEmptySlots b = 

getColumn :: [[b]] -> Int -> [b]
getColumn b i = map (!!i) b

getColumn2 (xs : xss) i = map (!!i) (head xs)

-- concatColumns = 

doesColumnExist :: [Disc] -> Bool
doesColumnExist c = find E c || find X c || find O c



--                            IO FUNCTIONS                          --
------------------------------------------------------------------------

showBoard :: Board -> IO ()
showBoard = mapM_ initRow

showDisc :: Disc -> IO ()
showDisc E = putChar 'E'
showDisc X = setSGR [SetColor Foreground  Vivid Blue] >> putChar 'X' >> setSGR [Reset]
showDisc O = setSGR [SetColor Foreground  Vivid Red] >> putChar 'O' >> setSGR [Reset]

-- showDisc :: Disc -> IO()
-- showDisc E =  putChar '⚪'
-- showDisc X =  putChar '🔴'
-- showDisc O =  putChar '🔵'

randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = getStdGen >>= return . (randomRs range)

initRow :: Row -> IO ()
initRow []  = putStrLn "There are no rows inserted"
initRow [x] = showDisc x >> putStr "\n"
initRow (x : xs) = showDisc x  >> putChar ' ' >> initRow xs

winColumnPlayer :: Disc -> Row -> Bool
winColumnPlayer p [] = False
winColumnPlayer p r = (filter (== p) (take 4 r)  == take 4 r) || winColumnPlayer p (tail r)

winOnStraightLine :: Disc -> Row -> Bool
winOnStraightLine p [] = False
winOnStraightLine p [x,y,z] = False
winOnStraightLine p r = (filter (== p) (take 4 r)  == take 4 r) || winOnStraightLine p (tail r)

-- winRowPlayer :: Disc -> Row -> Bool
-- winRowPlayer [] = False
-- winRowPlayer p = (filter (== p) (take 4 r)  == take 4 r) || winRowPlayer p (tail r)

leftdiagonals :: Board-> Board
leftdiagonals [] = []
leftdiagonals ([] : x) = x
leftdiagonals  x = zipWith (++) (map ( (: []). head) x ++ repeat [])([]:leftdiagonals (map tail x))

rightdiagonals :: Board -> Board
rightdiagonals x = leftdiagonals (reverse x)

--- this is the function to get all the other diagonals 

moreThanThree :: [[a]] -> [[a]]
moreThanThree x = filter (\x-> (length x) >3) x

diagonals ::  Board -> Board
diagonals x = moreThanThree (leftdiagonals x ++ rightdiagonals x)

winDiagonalsPlayerOne :: Board -> Bool
winDiagonalsPlayerOne b = winRowBoardPlayerOne ( diagonals b)


winRowBoardPlayerOne :: Board -> Bool
winRowBoardPlayerOne b = if filter (== True)( map (winRowPlayerOne) b) == [] then False else True

winRowPlayerOne :: Row -> Bool
winRowPlayerOne [] = False
winRowPlayerOne [x,y,z] = False
winRowPlayerOne r = if  filter (== X) (take 4 r)  == take 4 r then True else winRowPlayerOne (tail r)


winRowBoardPlayerTwo :: Board -> Bool
winRowBoardPlayerTwo b = if filter (== True)( map (winRowPlayerTwo) b) == [] then False else True

winDiagonalsPlayerTwo :: Board -> Bool
winDiagonalsPlayerTwo x = winRowBoardPlayerTwo ( diagonals x)

winRowPlayerTwo :: Row -> Bool
winRowPlayerTwo [] = False
winRowPlayerTwo [x,y,z] = False
winRowPlayerTwo r = if  filter (== O) (take 4 r)  == take 4 r then True else winRowPlayerTwo (tail r)

isDraw :: Board -> Bool
isDraw [] = True
isDraw (b:bs) =  if filterE b  /= [] then False else isDraw bs

getUserInput :: String -> IO Int
getUserInput s = do
  putStrLn s
  line <- getLine
  case readMaybe line of
    Maybe.Just x -> return x
    Maybe.Nothing -> putStrLn "You must enter a valid number. Please try again. \n" >> getUserInput s

columnIsValid :: Disc -> Board -> IO Int
columnIsValid p b = do
  putStr (showPlayerName p) >> putStr " choose input column: "
  i <- getUserInput ""
  if i < length (head b)
    then return i
    else putStrLn "You must enter a valid number. Please try again. \n" >> columnIsValid p b

--GAME PLAY --
----------------------------

checkWinner :: Int -> Int -> Disc -> Int -> Board -> IO ()
checkWinner m r p i rb = do
  case winOnStraightLine  p $ getColumn (updateBoard p i rb) i of
    True -> putStr (showPlayerName p) >> putStr " has won the game on Column! \n"
    False -> case winOnStraightLine p (concat $ updateBoard p i rb) of
      True -> putStr (showPlayerName p) >> putStr " has won the game on Row! \n"
      False -> case winDiagonalsPlayerOne (updateBoard p i rb) of
        True -> putStr (showPlayerName p) >> putStr " has won the game Diagonally! \n"
        False -> case winDiagonalsPlayerTwo (updateBoard p i rb) of
          True -> putStr (showPlayerName p) >> putStr " has won the game Diagonally! \n"
          False -> case isDraw (updateBoard  p i rb) of
            True -> putStr "The game is a draw"
            False -> case m == 1 of 
              True -> playGameP m r (switchPlayer p) $ reverse $ updateBoard p i rb
              False -> playGameC m r (switchPlayer p) $ reverse $ updateBoard p i rb

playGameP :: Int -> Int -> Disc -> Board -> IO ()
playGameP m r p b = do
    i <- columnIsValid p b
    threadDelay 2.0e5
    let rb = reverse b
    if columnHasEmptySlot rb i then showBoard $ reverse $ updateBoard p i rb
      else (do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Column is full, please choose another column"
        setSGR [Reset]
        let ri = reverse rb
        if p == X then playGameP m r p ri else playGameC m r p ri )
    checkWinner m r p i rb


playGameC :: Int -> Int -> Disc -> Board -> IO ()
playGameC m r p b = do
  threadDelay 1000000
  number <- computerMove r
  putStr "Computer made move in column:" >> print number
  let rb = reverse b
  showBoard $ reverse $ updateBoard p number rb
  let rbr2 = reverse $ updateBoard p number rb
  playGameP m r (switchPlayer p) rbr2

computerMove :: Int -> IO Int
computerMove r = randomRIO (0,r-1)

initializeGame :: IO ()
initializeGame = do
  hSetEncoding stdout utf8
  hSetBuffering stdin NoBuffering
  clearScreen
  setCursorPosition 4 25
  setSGR [SetColor Foreground  Vivid Blue]
  putStrLn "WELCOME TO 4 IN A ROW"
  setSGR [Reset]
  m <- getUserInput $ "Choose a game mode: \n"++ "1 -> Player vs Player \n" ++ "2 -> Player vs Computer"
  c <- getUserInput $ "Choose a board size column: \n" ++ "Columns: "
  r <- getUserInput "Rows: "
  showBoard $ createBoard c r
  playGameP m r X $ createBoard c r

main :: IO ()
main = do
  initializeGame 
  return ()



-- findEmptySlotInColumn b i = elemIndex E $ getSelectedColumn b i

--A
--Rafael
-- state monad $ transformer
-- win diagonal more general for both players

--Ivannick
-- Computer knows column full
-- Ansi moving cursor / the number they can pick seperate with line

-- Minimal size 4 x 4 -- Standard board vs custom
-- spacing between columns and lines
--font size for win
-- imrpove text

--B
--Ivannick
-- Computer knows column full
-- Ansi moving cursor / the number they can pick seperate with line
-- Minimal size 4 x 4 -- Standard board vs custom

--Rafael
-- spacing between columns and lines
--font size for win
-- imrpove text

--Rafael/Ivannick
-- state monad $ transformer
-- win diagonal more general for both players

--C
--Rafael
-- state monad $ transformer
-- win diagonal more general for both players

--Ivannick
-- Computer knows column full
-- Ansi moving cursor / the number they can pick seperate with line

-- Up for grabs
-- Minimal size 4 x 4 -- Standard board vs custom
-- spacing between columns and lines
--font size for win



