{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}

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

showDisc :: Disc -> Char
showDisc E = 'E'
showDisc X = 'X'
showDisc O = 'O'
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

playerInput :: Char -> Int -> Row -> [Disc]
playerInput p n r = toList $ Seq.update n (toDisc p) (Seq.fromList r)

-- takes a reverse board
updateBoard :: Char -> Int -> Board -> Board
updateBoard p i [] = []
updateBoard p i (xs : xss) = case xs ^? element i of
    Maybe.Nothing -> xs : xss
    Maybe.Just E  -> playerInput p i xs : xss
    Maybe.Just X  -> xs : updateBoard 'X' i xss
    Maybe.Just O  -> xs : updateBoard 'O' i xss

-- IMPURE FUNCTIONS
randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = getStdGen >>= return . (randomRs range)

initRow :: Row -> IO ()
initRow []  = putStrLn "There are no rows inserted"
initRow [x] = putChar (showDisc $ head [x]) >> putStr "\n"
initRow (x : xs) = putChar (showDisc $ head [x]) >> putChar ' ' >> initRow xs

-- initRow :: Row -> IO ()
-- initRow []  = putStrLn "There are no rows inserted"
-- initRow [x] = setSGR [SetColor Foreground  Vivid Blue] >> putChar (showDisc $ head [x]) >> setSGR [Reset] >> putStr "\n"   
-- initRow (x : xs) = putChar (showDisc $ head [x]) >> putChar ' ' >> initRow xs
winRowPlayerOne :: Row -> Bool
winRowPlayerOne [] = False
winRowPlayerOne r = if  filter (== X) (take 4 r)  == take 4 r then True else winRowPlayerOne (tail r)

showBoard :: Board -> IO ()
showBoard = mapM_ initRow

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs
  
columnHasEmptySlot b i = find E $ map (!!i) b
-- findEmptySlotInColumn b i = elemIndex E $ getSelectedColumn b i

playGameP :: Int -> Int -> Disc -> Board -> IO ()
playGameP m r p b = do
    putStr (showPlayerName p) >> putStr " choose input column: "
    i :: Int <- readLn
    threadDelay 200000
    let rb = reverse b
    if columnHasEmptySlot rb i then showBoard $ reverse $ updateBoard (showDisc p) i rb
      else (do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Column is full, please choose another column"
        setSGR [Reset]
        let ri = reverse rb
        playGameP m r p ri)
    if m == 1 
      then playGameP m r (switchPlayer p) $ reverse $ updateBoard (showDisc p) i rb
      else playGameC m r (switchPlayer p) $ reverse $ updateBoard (showDisc p) i rb

playGameC :: Int -> Int -> Disc -> Board -> IO ()
playGameC m r p b = do
  threadDelay 1000000
  number <- randomRIO (0,r-1) :: IO Int
  putStr "Computer made move in column:" >> print number
  let rb = reverse b
  showBoard $ reverse $ updateBoard (showDisc p) number rb
  let rbr2 = reverse $ updateBoard (showDisc p) number rb
  playGameP m r (switchPlayer p) rbr2



main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetBuffering stdin NoBuffering
  clearScreen
  setCursorPosition 4 25
  setSGR [SetColor Foreground  Vivid Blue]
  putStrLn "WELCOME TO 4 IN A ROW"
  setSGR [Reset]
  putStrLn "Choose a game mode: "
  putStrLn "1 -> Player vs Player"
  putStrLn "2 -> Player vs Computer"
  m <- getUserInput
  if m <= 0  then putStrLn "No negative numbers allowed"
  else putStrLn "Choose a board size column: "
  putStrLn "Columns: "
  c <- getUserInput
  if c <= 0  then putStrLn "No negative numbers allowed"
  else putStrLn "Rows: "
  r <- getUserInput
  if r <= 0  then putStrLn "No negative numbers allowed"
  -- create board
  else showBoard $ createBoard c r
  playGameP m r X $ createBoard c r
  return ()

getUserInput :: IO Int
getUserInput = do
  line <- getLine
  case readMaybe line of
    Maybe.Just x -> return x
    Maybe.Nothing -> putStrLn "You must enter a number. Please try again." >> getUserInput

    -- https://stackoverflow.com/questions/15542328/checking-to-see-if-a-list-is-ordered-consecutively


  --TODO:
  -- finish IO with exception handling user input
  -- State Monad for keeping score individual player
  -- Diagonal board check

  -- Irfan Cabal, to use cabal in vs code.

  --EXTRA
  -- random (funny) computer expressions
  -- color code X and O || Uni code
  -- State Monad -> play with money (keep a balance add/substract)
  --             -> best of 3 sets
  -- Read/write from file to keep track High score of players in previous games
--a comment
  -- refactor code structure in general structure (use of typeclasses, foldr,map, functor/applicative/monadic structures)

  ---------------------------------------------------------------------------------------------------------------
--Rafael code

--   import qualified Data.Sequence as Seq

-- import Data.Foldable 

-- type Size = Int

-- type PlayColumn = Int

-- data Disc = X | O | E deriving (Eq, Read, Show)

-- type Row = [Disc]

-- type Collumn = [Disc]

-- type Board = [[Disc]]

-- ir= [E,E,E,E,E,E,E]

-- --playerOneInput :: (Disc -> ([a1] -> Seq.Seq a1) -> [Disc] -> Int) -> a2 -> Seq.Seq a2 -> Seq.Seq a2
-- --playerOneInput y = Seq.update (y X Seq.fromList ir )

-- z= Seq.fromList ["hi" , "world", "all", "good"] 

-- f n = Seq.update n 4 (Seq.fromList [1,2,3])

-- g n  =  Seq.update n "hello" (Seq.fromList ["hi", "world", "!"])

-- playerOneInput :: Int ->  Seq.Seq Disc
-- playerOneInput n = Seq.update n X (Seq.fromList [ E,E,E,E,E,E,E])

-- playerTwoInput :: Int ->  Seq.Seq Disc
-- playerTwoInput n = Seq.update n O (Seq.fromList [ E,E,E,E,E,E,E])
-- -- Maybe we can work with an if Function and an update function to figure out if we can add things without overlapping? 

-- createBoard :: Size -> Size -> Board
-- createBoard m n = replicate m (replicate n E)

-- defineRow :: Size -> Row
-- defineRow n = replicate n E  

-- playerOneInput' :: Int ->  Seq.Seq Disc
-- playerOneInput' y = Seq.update y X (Seq.fromList (defineRow 7))

-- -- using this function when there is an error and we state the wrong it jsut returns the same list

-- playerTwoInput' :: Int ->  Seq.Seq Disc
-- playerTwoInput' y = Seq.update y O (Seq.fromList (defineRow 7))

-- playerOneInputwrong :: Int -> Row -> Seq.Seq Disc
-- playerOneInputwrong n r = Seq.update n X (Seq.fromList r)


-- --- maybe we can use define row in the create Board function to make things look clearner.

-- standardBoard = createBoard 6 7 

-- updateRowPlayerOne ::  Int -> Seq.Seq Row
-- updateRowPlayerOne n = Seq.update n [E,E,E,X,E,E,E] (Seq.fromList standardBoard)

-- addToBoardPlayerOnet :: Int -> Int -> Seq.Seq Row
-- addToBoardPlayerOnet y n = Seq.update n  (toList (playerOneInput y)) (Seq.fromList standardBoard)

-- addToBoardPlayerTwot :: Int -> Int -> Seq.Seq Row
-- addToBoardPlayerTwot y n = Seq.update n  (toList (playerTwoInput y)) (Seq.fromList standardBoard)


-- --- this above is the test bellow is the real function once we get inputs from the user 

-- --addToBoardPlayerOne :: Int -> Int -> Seq.Seq Row
-- --addToBoardPlayerOne y x = Seq.update x  (toList (playerOneInput' y)) (Seq.fromList createBoard m n)

-- -- addToBoardPlayerTwot :: Int -> Int -> Seq.Seq Row
-- -- addToBoardPlayerTwot y n = Seq.update n  (toList (playerTwoInput' y)) (Seq.fromList createBoard m n )

-- -- thsi fucntion should not only be create board, it should be the latest borad we have.

-- -- Maybe we will ned to create a monad to store the state of the board. we should still use the smae functions we have to transform ti but the monad will be useful to get the values to use.

-- -- Make a function that creates a tupple and indexes a collumn 

-- orderCollumn :: Int -> [(Collumn, Int)]
-- orderCollumn n  = zipWith (,) (getCollumn n) [1..1000]

-- -- This function should create a tuple with number and columns  the second number is alrgely big on purpose to make sure we always have a lsit number.


-- addToEmpty :: [(Collumn, Int)] -> Int
-- addToEmpty [] = - 6
-- addToEmpty (x:xs) = if fst x == E then snd x  else addToEmpty xs 

-- --- we are not using maybe value, or lookup here because we should have first another function that guarantees that we have always an E in each column otherwise you will have to pick again.

-- filterE :: Collumn -> Collumn
-- filterE y = filter (== E) y



-- tr1 = [X,O,X,O,E,E,E]
-- tr2 = [X,O,X,O,O,X,E]
-- tr3 = [X,O,X,O,X,X,O]

-- validCollumn:: Int -> Bool
-- validCollumn n = if filterE (getCollumn n) == [] then False else True

-- -- This is a function we can use to check if aCollumn ifs full. We can opt it to have it pure like this and just use it on our io actions or we can make it Io from the get go- Basically if True we should carry on with the rest of tjhe functions previously defined, if false we should inform the player the row is full and needs to pick another one. 

-- -- Afterwards we use the Int of the addToEmpty as the input for the addToBoard player functions

-- -- Bellow there is a function that checks the Board if the board is full there is a draw and we should give a message and start a new game.

-- isDraw :: Board -> Bool
-- isDraw [] = True
-- isDraw (b:bs) =  if filterE b  /= [] then False else isDraw bs  

-- dBoard = [ [X,O,X,O,X,X,O],[X,O,X,O,X,X,O] , [X,O,X,O,X,X,O], [X,O,X,O,X,X,O] , [X,O,X,O,X,X,O] , [X,O,X,O,X,X,O] , [X,O,X,O,X,X,O] ]

-- -- we always need to play teh functions to check fi the game as been won before the isDraw function as in an edge case someone could win in the last round

-- -- we either need to get the collumn with opposite order to check or we might need to create a new system to append to the bototom, but I believe the first one should be wenouh if we want to add the rest using rows. SUPER IMPORTANT TO CHECK AND TEST HOW WE ARE ADDING THE NEW VALUES TO MAKE SURE WE ARE ALWAYS ADDING TO THE BOTTOM AND NOT THE TOP.

-- towin =  4 :: Int

-- --checkPlayerOneRow :: Row 

-- winRowPlayerOne :: Row -> Bool
-- winRowPlayerOne [] = False
-- winRowPlayerOne r = if  filter (== X) (take 4 r)  == take 4 r then True else winRowPlayerOne (tail r)

-- tr4 = [X,X,X,X,O,X,E]
-- tr5 = [O,O,X,X,X,X,O]

-- winRowPlayerTwo :: Row -> Bool
-- winRowPlayerTwo [] = False
-- winRowPlayerTwo r = if  filter (== O) (take 4 r)  == take 4 r then True else winRowPlayerTwo (tail r)


-- -- we should use this function to check a victory on the row after each game.

-- winRowBoardPlayerOne :: Board -> Bool
-- winRowBoardPlayerOne b = if filter (== True)( map (winRowPlayerOne) b) == [] then False else True 

-- standardBoard = createBoard 6 7 

-- mapWinRowBoardPlayerOne :: Board -> [Bool]
-- mapWinRowBoardPlayerOne b =  map (winRowPlayerOne) b


-- wBoardP1 = [[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[O,O,X,X,X,X,O],[X,O,X,O,X,X,O]]
-- wBoardP1' = [[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[X,X,X,X,O,X,E],[X,O,X,O,O,X,E],[X,O,X,O,X,X,O],[X,O,X,O,X,X,O]]

-- winRowBoardPlayerTwo :: Board -> Bool
-- winRowBoardPlayerTwo b = if filter (== True)( map (winRowPlayerTwo) b) == [] then False else True 

-- ---- 

-- -- After checking the rows we need to check  for a win with the collumns

-- -- DON'T FORGET TO CHANGE ALL MY EQUATIONS WITH THE NEW COLLUMN Shared by Ivanick

-- winCollumnPlayerOne :: Collumn -> Bool
-- winCollumnPlayerOne [] = False
-- winCollumnPlayerOne c = if  filter (== X) (take 4 c)  == take 4 c then True else winCollumnPlayerOne (tail c)

-- winCollumnPlayerTwo :: Collumn -> Bool
-- winCollumnPlayerTwo  [] = False
-- winCollumnPlayerOner = if  filter (== O) (take 4 c)  == take 4 c then True else winCollumnPlayerOne (tail c)

-- winCollumnBoardPlayerOne :: Collumn-> Bool
-- winCollumnBoardPlayerOne b = if filter (== True)( map (winCollumnPlayerOne) b) == [] then False else True 

-- winCollumnBoardPlayerTwo :: Collumn-> Bool
-- winCollumnBoardPlayerTwo b = if filter (== True)( map (winCollumnPlayerTwo) b) == [] then False else True 

-- -- Here we don't need the new column function we only need to use, the output of the column function as the input for our functions 