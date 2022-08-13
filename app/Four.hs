{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Four where

--LIBS
import Data.Char ()
import Data.Foldable (toList)
import Data.List ()
import Data.Maybe as Maybe ( Maybe(Just, Nothing) )
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import System.IO ()
import Control.Lens ( (^?), element )

--TYPES/DATA
type Size = Int
data Disc = X | O | E deriving (Eq, Read, Show)
type Row = [Disc]
type Board = [[Disc]]
data Maybe a = Nothing | Just a
-- PURE FUNCTIONS
createBoard :: Size -> Size -> Board
createBoard m n = replicate m (replicate n E)

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
initRow [x] = putChar (showDisc $ head [x]) >> putStr "\n"
initRow (x : xs) = putChar (showDisc $ head [x]) >> putChar ' ' >> initRow xs

showBoard :: Board -> IO ()
showBoard = mapM_ initRow

playGame :: Char -> Int -> Int -> Board -> IO ()
playGame p c r b = do
  putStr (showPlayerName p) >> putStr " choose input column: "
  i :: Int <- readLn
  let rb = reverse b
  let em = updateBoard p i rb
  if em == rb then (do
      putStrLn "Column is full, please choose another column"
      playGame p c r rb) else showBoard $ reverse $ updateBoard p i rb
  let rbr = updateBoard p i rb
  let rbr2 = reverse rbr
  playGame (switchPlayer p) c r rbr2

main :: IO ()
main = do
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
  let b = createBoard c r
  playGame 'X' c r b
  return ()