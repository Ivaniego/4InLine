{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumDecimals #-}

module Four where

--LIBS
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
import System.Console.ANSI ( setCursorPosition, clearScreen, setSGR, SGR (SetColor, Reset), ConsoleLayer (Background, Foreground), ColorIntensity (Vivid), Color (Blue, Red) )
import Text.Read (readMaybe)

--TYPES/DATA
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

-- PURE FUNCTIONS
createBoard :: Size -> Size -> Board
createBoard m n = replicate m (replicate n E)

-- switchPlayer :: Char -> Char
-- switchPlayer 'X' = 'O'
-- switchPlayer 'O' = 'X'
switchPlayer :: Disc -> Disc
switchPlayer X = O
switchPlayer O = X


showDisc :: Disc -> IO ()
showDisc E = putChar 'E'
showDisc X = setSGR [SetColor Foreground  Vivid Blue] >> putChar 'X' >> setSGR [Reset]
showDisc O = setSGR [SetColor Foreground  Vivid Red] >> putChar 'O' >> setSGR [Reset]
-- showDisc :: Disc -> Char
-- showDisc E =  '⚪'
-- showDisc X =  '🔴'
-- showDisc O =  '🔵'

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

-- IMPURE FUNCTIONS
randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = getStdGen >>= return . (randomRs range)

initRow :: Row -> IO ()
initRow []  = putStrLn "There are no rows inserted"
initRow [x] = (showDisc $ head [x]) >> putStr "\n"
initRow (x : xs) = (showDisc $ head [x]) >> putChar ' ' >> initRow xs

-- initRow :: Row -> IO ()
-- initRow []  = putStrLn "There are no rows inserted"
-- initRow [x] = setSGR [SetColor Foreground  Vivid Blue] >> putChar (showDisc $ head [x]) >> setSGR [Reset] >> putStr "\n"   
-- initRow (x : xs) = putChar (showDisc $ head [x]) >> putChar ' ' >> initRow xs
winColumnPlayer :: Disc -> Row -> Bool
winColumnPlayer p [] = False
winColumnPlayer p r = (filter (== p) (take 4 r)  == take 4 r) || winColumnPlayer p (tail r)

winRow2Player :: Disc -> Row -> Bool
winRow2Player p [] = False
winRow2Player p r = (filter (== p) (take 4 r)  == take 4 r) || winRow2Player p (tail r)

-- winRowPlayer :: Disc -> Row -> Bool
-- winRowPlayer [] = False
-- winRowPlayer p = (filter (== p) (take 4 r)  == take 4 r) || winRowPlayer p (tail r)

showBoard :: Board -> IO ()
showBoard = mapM_ initRow

isDraw :: Board -> Bool
isDraw [] = True
isDraw (b:bs) =  if filterE b  /= [] then False else isDraw bs


filterE :: [Disc] -> [Disc]
filterE y = filter (== E) y

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs

columnHasEmptySlot b i = find E $ map (!!i) b

getColumn b i = map (!!i) b
-- findEmptySlotInColumn b i = elemIndex E $ getSelectedColumn b i
doesColumnExist c = find E c || find X c || find O c

-- columnIsValid :: Int -> Board -> Bool
-- columnIsValid i b =  i < length (head b)




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
        playGameP m r p ri)
    if winColumnPlayer  p $ getColumn (updateBoard p i rb) i
      then putStr (showPlayerName p) >> putStr " has won the game on Column! \n"
      else if winRow2Player p (concat $ updateBoard p i rb)
        then putStr (showPlayerName p) >> putStr " has won the game on Row! \n"
        else if isDraw (updateBoard  p i rb)
          then putStr "The game is a draw"
        else if m == 1 then playGameP m r (switchPlayer p) $ reverse $ updateBoard p i rb else playGameC m r (switchPlayer p) $ reverse $ updateBoard ( p) i rb

playGameC :: Int -> Int -> Disc -> Board -> IO ()
playGameC m r p b = do
  threadDelay 1000000
  number <- randomRIO (0,r-1) :: IO Int
  putStr "Computer made move in column:" >> print number
  let rb = reverse b
  showBoard $ reverse $ updateBoard p number rb
  let rbr2 = reverse $ updateBoard p number rb
  playGameP m r (switchPlayer p) rbr2

main :: IO ()
main = do
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
  if r <= 0  then putStrLn "No negative numbers allowed"
  else showBoard $ createBoard c r
  playGameP m r X $ createBoard c r
  return ()

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

