{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}

module Four where

--LIBS
import Data.Char ()
import Data.Foldable (toList)
import Data.List ()
import Data.Maybe as Maybe ( Maybe(Just, Nothing) )
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering))
import System.Random (getStdGen, Random (randomRs), randomRIO, randomIO)
import Control.Monad.State (lift)
import Control.Lens ( (^?), element, (^.))
import Control.Lens.Tuple ()
import Unicode.Char ()
import Text.Show.Unicode ()
import Control.Concurrent ( threadDelay )



--TYPES/DATA
type Size = Int
data Disc = X | O | E deriving (Eq, Read, Show)
type Row = [Disc]
type Board = [[Disc]]
data Maybe a = Nothing | Just a
data Game = Game
  { _gPlayerScore   :: Int
  , _gComputerScore :: Int
  } deriving Show

-- PURE FUNCTIONS
createBoard :: Size -> Size -> Board
createBoard m n = replicate m (replicate n E)

randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = getStdGen >>= return . (randomRs range)

switchPlayer :: Char -> Char
switchPlayer 'X' = 'O'
switchPlayer 'O' = 'X'

showDisc :: Disc -> Char
showDisc E = 'E'
showDisc X = 'X'
showDisc O = 'O'

toDisc :: Char -> Disc
toDisc 'X' = X
toDisc 'O' = O

showPlayerName :: Char -> String
showPlayerName 'X' = "Player 1"
showPlayerName 'O' = "Player 2"

playerInput :: Char -> Int -> Row -> [Disc]
playerInput p n r = toList $ Seq.update n (toDisc p) (Seq.fromList r)

-- takes a reverse board
updateBoard :: Char -> Int -> Board -> Board
updateBoard p i [] = []
updateBoard p i (xs : xss) = case xs ^? element i of
    Maybe.Nothing -> xs : xss
    Maybe.Just E  -> playerInput p i xs : xss
    Maybe.Just X  -> xs : updateBoard 'O' i xss
    Maybe.Just O  -> xs : updateBoard 'X' i xss

-- IMPURE FUNCTIONS
initRow :: Row -> IO ()
initRow []  = putStrLn "There are no rows inserted"
initRow [x] = putChar (showDisc $ head [x]) >> putStr "\n"
initRow (x : xs) = putChar (showDisc $ head [x]) >> putChar ' ' >> initRow xs

showBoard :: Board -> IO ()
showBoard = mapM_ initRow

playGameP :: Int -> Int -> Char -> Board -> IO ()
playGameP m r p b = do
    putStr (showPlayerName p) >> putStr " choose input column: "
    i :: Int <- readLn
    let rb = reverse b
    let em = updateBoard p i rb
    if em == rb then (do
        putStrLn "Column is full, please choose another column"
        playGameP m r p rb) else showBoard $ reverse $ updateBoard p i rb
    let rbr2 = reverse $ updateBoard p i rb
    if m == 1 then playGameP m r (switchPlayer p) rbr2 else playGameC m r (switchPlayer p)  rbr2

playGameC :: Int -> Int -> Char -> Board -> IO ()
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
  hSetBuffering stdin NoBuffering
  putStrLn "Choose a game mode: "
  putStrLn "1 -> Player vs Player"
  putStrLn "2 -> Player vs Computer"
  m :: Int <- readLn
  -- board size
  putStrLn "Choose a board size column: "
  putStrLn "Columns: "
  c :: Int <- readLn
  putStrLn "Rows: "
  r :: Int <- readLn
  -- create board
  showBoard $ createBoard c r
  playGameP m r 'X' $ createBoard c r
  return ()


  -- For the gameplay introduce a strategy Player vs computer
  -- random numbers for computer
  -- State Monad for keeping score