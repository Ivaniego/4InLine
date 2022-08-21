module Functions where
-- -- https://stackoverflow.com/questions/15542328/checking-to-see-if-a-list-is-ordered-consecutively


--   --TODO:
--   -- finish IO with exception handling user input
--   -- State Monad for keeping score individual player
--   -- Diagonal board check

--   -- Irfan Cabal, to use cabal in vs code.

--   --EXTRA
--   -- random (funny) computer expressions
--   -- color code X and O || Uni code
--   -- State Monad -> play with money (keep a balance add/substract)
--   --             -> best of 3 sets
--   -- Read/write from file to keep track High score of players in previous games
-- --a comment
--   -- refactor code structure in general structure (use of typeclasses, foldr,map, functor/applicative/monadic structures)

--   ---------------------------------------------------------------------------------------------------------------
-- --Rafael code

-- --   import qualified Data.Sequence as Seq

-- -- import Data.Foldable 

-- -- type Size = Int

-- -- type PlayColumn = Int

-- -- data Disc = X | O | E deriving (Eq, Read, Show)

-- -- type Row = [Disc]

-- -- type Collumn = [Disc]

-- -- type Board = [[Disc]]

-- -- ir= [E,E,E,E,E,E,E]

-- -- --playerOneInput :: (Disc -> ([a1] -> Seq.Seq a1) -> [Disc] -> Int) -> a2 -> Seq.Seq a2 -> Seq.Seq a2
-- -- --playerOneInput y = Seq.update (y X Seq.fromList ir )

-- -- z= Seq.fromList ["hi" , "world", "all", "good"] 

-- -- f n = Seq.update n 4 (Seq.fromList [1,2,3])

-- -- g n  =  Seq.update n "hello" (Seq.fromList ["hi", "world", "!"])

-- -- playerOneInput :: Int ->  Seq.Seq Disc
-- -- playerOneInput n = Seq.update n X (Seq.fromList [ E,E,E,E,E,E,E])

-- -- playerTwoInput :: Int ->  Seq.Seq Disc
-- -- playerTwoInput n = Seq.update n O (Seq.fromList [ E,E,E,E,E,E,E])
-- -- -- Maybe we can work with an if Function and an update function to figure out if we can add things without overlapping? 

-- -- createBoard :: Size -> Size -> Board
-- -- createBoard m n = replicate m (replicate n E)

-- -- defineRow :: Size -> Row
-- -- defineRow n = replicate n E  

-- -- playerOneInput' :: Int ->  Seq.Seq Disc
-- -- playerOneInput' y = Seq.update y X (Seq.fromList (defineRow 7))

-- -- -- using this function when there is an error and we state the wrong it jsut returns the same list

-- -- playerTwoInput' :: Int ->  Seq.Seq Disc
-- -- playerTwoInput' y = Seq.update y O (Seq.fromList (defineRow 7))

-- -- playerOneInputwrong :: Int -> Row -> Seq.Seq Disc
-- -- playerOneInputwrong n r = Seq.update n X (Seq.fromList r)


-- -- --- maybe we can use define row in the create Board function to make things look clearner.

-- -- standardBoard = createBoard 6 7 

-- -- updateRowPlayerOne ::  Int -> Seq.Seq Row
-- -- updateRowPlayerOne n = Seq.update n [E,E,E,X,E,E,E] (Seq.fromList standardBoard)

-- -- addToBoardPlayerOnet :: Int -> Int -> Seq.Seq Row
-- -- addToBoardPlayerOnet y n = Seq.update n  (toList (playerOneInput y)) (Seq.fromList standardBoard)

-- -- addToBoardPlayerTwot :: Int -> Int -> Seq.Seq Row
-- -- addToBoardPlayerTwot y n = Seq.update n  (toList (playerTwoInput y)) (Seq.fromList standardBoard)


-- -- --- this above is the test bellow is the real function once we get inputs from the user 

-- -- --addToBoardPlayerOne :: Int -> Int -> Seq.Seq Row
-- -- --addToBoardPlayerOne y x = Seq.update x  (toList (playerOneInput' y)) (Seq.fromList createBoard m n)

-- -- -- addToBoardPlayerTwot :: Int -> Int -> Seq.Seq Row
-- -- -- addToBoardPlayerTwot y n = Seq.update n  (toList (playerTwoInput' y)) (Seq.fromList createBoard m n )

-- -- -- thsi fucntion should not only be create board, it should be the latest borad we have.

-- -- -- Maybe we will ned to create a monad to store the state of the board. we should still use the smae functions we have to transform ti but the monad will be useful to get the values to use.

-- -- -- Make a function that creates a tupple and indexes a collumn 

-- -- orderCollumn :: Int -> [(Collumn, Int)]
-- -- orderCollumn n  = zipWith (,) (getCollumn n) [1..1000]

-- -- -- This function should create a tuple with number and columns  the second number is alrgely big on purpose to make sure we always have a lsit number.


-- -- addToEmpty :: [(Collumn, Int)] -> Int
-- -- addToEmpty [] = - 6
-- -- addToEmpty (x:xs) = if fst x == E then snd x  else addToEmpty xs 

-- -- --- we are not using maybe value, or lookup here because we should have first another function that guarantees that we have always an E in each column otherwise you will have to pick again.

-- -- filterE :: Collumn -> Collumn
-- -- filterE y = filter (== E) y



-- -- tr1 = [X,O,X,O,E,E,E]
-- -- tr2 = [X,O,X,O,O,X,E]
-- -- tr3 = [X,O,X,O,X,X,O]

-- -- validCollumn:: Int -> Bool
-- -- validCollumn n = if filterE (getCollumn n) == [] then False else True

-- -- -- This is a function we can use to check if aCollumn ifs full. We can opt it to have it pure like this and just use it on our io actions or we can make it Io from the get go- Basically if True we should carry on with the rest of tjhe functions previously defined, if false we should inform the player the row is full and needs to pick another one. 

-- -- -- Afterwards we use the Int of the addToEmpty as the input for the addToBoard player functions

-- -- -- Bellow there is a function that checks the Board if the board is full there is a draw and we should give a message and start a new game.

-- -- isDraw :: Board -> Bool
-- -- isDraw [] = True
-- -- isDraw (b:bs) =  if filterE b  /= [] then False else isDraw bs  

-- -- dBoard = [ [X,O,X,O,X,X,O],[X,O,X,O,X,X,O] , [X,O,X,O,X,X,O], [X,O,X,O,X,X,O] , [X,O,X,O,X,X,O] , [X,O,X,O,X,X,O] , [X,O,X,O,X,X,O] ]

-- -- -- we always need to play teh functions to check fi the game as been won before the isDraw function as in an edge case someone could win in the last round

-- -- -- we either need to get the collumn with opposite order to check or we might need to create a new system to append to the bototom, but I believe the first one should be wenouh if we want to add the rest using rows. SUPER IMPORTANT TO CHECK AND TEST HOW WE ARE ADDING THE NEW VALUES TO MAKE SURE WE ARE ALWAYS ADDING TO THE BOTTOM AND NOT THE TOP.

-- -- towin =  4 :: Int

-- -- --checkPlayerOneRow :: Row 

-- -- winRowPlayerOne :: Row -> Bool
-- -- winRowPlayerOne [] = False
-- -- winRowPlayerOne r = if  filter (== X) (take 4 r)  == take 4 r then True else winRowPlayerOne (tail r)

-- -- tr4 = [X,X,X,X,O,X,E]
-- -- tr5 = [O,O,X,X,X,X,O]

-- -- winRowPlayerTwo :: Row -> Bool
-- -- winRowPlayerTwo [] = False
-- -- winRowPlayerTwo r = if  filter (== O) (take 4 r)  == take 4 r then True else winRowPlayerTwo (tail r)


-- -- -- we should use this function to check a victory on the row after each game.

-- -- winRowBoardPlayerOne :: Board -> Bool
-- -- winRowBoardPlayerOne b = if filter (== True)( map (winRowPlayerOne) b) == [] then False else True 

-- -- standardBoard = createBoard 6 7 

-- -- mapWinRowBoardPlayerOne :: Board -> [Bool]
-- -- mapWinRowBoardPlayerOne b =  map (winRowPlayerOne) b


-- -- wBoardP1 = [[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[O,O,X,X,X,X,O],[X,O,X,O,X,X,O]]
-- -- wBoardP1' = [[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[X,X,X,X,O,X,E],[X,O,X,O,O,X,E],[X,O,X,O,X,X,O],[X,O,X,O,X,X,O]]

-- -- winRowBoardPlayerTwo :: Board -> Bool
-- -- winRowBoardPlayerTwo b = if filter (== True)( map (winRowPlayerTwo) b) == [] then False else True 

-- -- ---- 

-- -- -- After checking the rows we need to check  for a win with the collumns

-- -- -- DON'T FORGET TO CHANGE ALL MY EQUATIONS WITH THE NEW COLLUMN Shared by Ivanick

-- -- winCollumnPlayerOne :: Collumn -> Bool
-- -- winCollumnPlayerOne [] = False
-- -- winCollumnPlayerOne c = if  filter (== X) (take 4 c)  == take 4 c then True else winCollumnPlayerOne (tail c)

-- -- winCollumnPlayerTwo :: Collumn -> Bool
-- -- winCollumnPlayerTwo  [] = False
-- -- winCollumnPlayerOner = if  filter (== O) (take 4 c)  == take 4 c then True else winCollumnPlayerOne (tail c)

-- -- winCollumnBoardPlayerOne :: Collumn-> Bool
-- -- winCollumnBoardPlayerOne b = if filter (== True)( map (winCollumnPlayerOne) b) == [] then False else True 

-- -- winCollumnBoardPlayerTwo :: Collumn-> Bool
-- -- winCollumnBoardPlayerTwo b = if filter (== True)( map (winCollumnPlayerTwo) b) == [] then False else True 
-- -- Here we don't need the new column function we only need to use, the output of the column function as the input for our functions




-- --- THIS IS EVERYTHING I IMPORTED 

-- -- import qualified Data.Sequence as Seq
-- import Functions where

-- import Data.Foldable 
-- import qualified Data.Sequence as Seq

-- type Size = Int

-- type PlayColumn = Int

-- data Disc = X | O | E deriving (Eq, Read, Show)

-- type Row = [Disc]

-- type Collumn = [Disc]

-- type Board = [[Disc]]

-- ir= [E,E,E,E,E,E,E]

-- --playerOneInput :: (Disc -> ([a1] -> Seq.Seq a1) -> [Disc] -> Int) -> a2 -> Seq.Seq a2 -> Seq.Seq a2
-- --playerOneInput y = Seq.update (y X Seq.fromList ir )

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

-- -- orderCollumn :: Int -> [(Collumn, Int)]
-- -- orderCollumn n  = zipWith (,) (getCollumn n) [1..1000]

-- -- This function should create a tuple with number and columns  the second number is alrgely big on purpose to make sure we always have a lsit number.


-- -- addToEmpty :: [(Collumn, Int)] -> Int
-- -- addToEmpty [] = - 6
-- -- addToEmpty (x:xs) = if fst x == E then snd x  else addToEmpty xs 

-- --- we are not using maybe value, or lookup here because we should have first another function that guarantees that we have always an E in each column otherwise you will have to pick again.

-- filterE :: Collumn -> Collumn
-- filterE y = filter (== E) y



-- tr1 = [X,O,X,O,E,E,E]
-- tr2 = [X,O,X,O,O,X,E]
-- tr3 = [X,O,X,O,X,X,O]

-- -- validCollumn:: Int -> Bool
-- -- validCollumn n = if filterE (getCollumn n) == [] then False else True

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
-- winRowPlayerOne [x,y,z] = False 
-- winRowPlayerOne r = if  filter (== X) (take 4 r)  == take 4 r then True else winRowPlayerOne (tail r)

-- tr4 = [X,X,X,X,O,X,E]
-- tr5 = [O,O,X,X,X,X,O]

-- winRowPlayerTwo :: Row -> Bool
-- winRowPlayerTwo [] = False
-- winRowPlayerTwo [x,y,z] = False 
-- winRowPlayerTwo r = if  filter (== O) (take 4 r)  == take 4 r then True else winRowPlayerTwo (tail r)


-- -- we should use this function to check a victory on the row after each game.

-- winRowBoardPlayerOne :: Board -> Bool
-- winRowBoardPlayerOne b = if filter (== True)( map (winRowPlayerOne) b) == [] then False else True 


-- mapWinRowBoardPlayerOne :: Board -> [Bool]
-- mapWinRowBoardPlayerOne b =  map (winRowPlayerOne) b


-- wBoardP1 = [[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[O,O,X,X,X,X,O],[X,O,X,O,X,X,O]]
-- wBoardP1' = [[E,E,E,E,E,E,E],[E,E,E,E,E,E,E],[X,X,X,X,O,X,E],[X,O,X,O,O,X,E],[X,O,X,O,X,X,O],[X,O,X,O,X,X,O]]

-- winRowBoardPlayerTwo :: Board -> Bool
-- winRowBoardPlayerTwo b = if filter (== True)( map (winRowPlayerTwo) b) == [] then False else True 

-- ---- 

-- -- After checking the rows we need to check  for a win with the collumns

-- -- DON'T FORGET TO CHANGE ALL MY EQUATIONS WITH THE NEW COLLUMN Shared by Ivanick

-- -- winCollumnPlayerOne :: Collumn -> Bool
-- -- winCollumnPlayerOne [] = False
-- -- winCollumnPlayerOne [x,y,z] = False 
-- -- winCollumnPlayerOne c = if  filter (== X) (take 4 c)  == take 4 c then True else winCollumnPlayerOne (tail c)

-- -- winCollumnPlayerTwo :: Collumn -> Bool
-- -- winCollumnPlayerTwo  [] = False
-- -- winCollumnPlayerTwo [x,y,z] = False 
-- -- winCollumnPlayerTwo c = if  filter (== O) (take 4 c)  == take 4 c then True else winCollumnPlayerOne (tail c)

-- -- winCollumnBoardPlayerOne :: Collumn-> Bool
-- -- winCollumnBoardPlayerOne b = if filter (== True)( map (winCollumnPlayerOne) b) == [] then False else True 

-- -- winCollumnBoardPlayerTwo :: Collumn-> Bool
-- -- winCollumnBoardPlayerTwo b = if filter (== True)( map (winCollumnPlayerTwo) b) == [] then False else True 

-- -- Here we don't need the new column function we only need to use, the output of the column function as the input for our functions 

-- -- Now let's create a function to define diagonals


-- leftdiagonals :: Board-> Board
-- leftdiagonals [] = []
-- leftdiagonals ([] : x) = x
-- leftdiagonals  x = zipWith (++) (map ( (: []). head) x ++ repeat [])([]:leftdiagonals (map tail x))

-- --- this is the function to get all the diagonals starting from the upper left bound 

-- rightdiagonals :: Board -> Board
-- rightdiagonals x = leftdiagonals (reverse x) 

-- --- this is the function to get all the other diagonals 


-- diagonals ::  Board -> Board
-- diagonals x = moreThanThree (leftdiagonals x ++ rightdiagonals x)

-- -- this is function to get only the diagonals that matter where it's possible to have 4 in line 

-- moreThanThree :: [[a]] -> [[a]]
-- moreThanThree x = filter (\x-> (length x) >3) x 

-- -- this is just an auxiliary function to get the diagonals that matter

-- winDiagonalsPlayerOne :: Board -> Bool
-- winDiagonalsPlayerOne x = winRowBoardPlayerOne ( diagonals x) 

-- -- this is the function to check within the diagonals that are at least 4 or larger wether we have a winner 


-- wdBoardP1 = [[E,E,E,E,E,E,E],
--              [E,E,E,E,E,E,E],
--              [E,X,E,E,E,E,E],
--              [E,O,X,E,E,E,E],
--              [O,O,X,X,X,O,O],
--              [X,O,X,O,X,X,O]]

-- wdBoardP1' = [[E,E,E,E,E,E,E],
--               [E,E,O,E,E,X,E],
--               [O,X,X,O,X,X,E],
--               [X,O,O,X,O,O,O],
--               [X,O,X,O,X,X,O], 
--               [X,O,X,O,X,X,O]]


-- winDiagonalsPlayerTwo :: Board -> Bool
-- winDiagonalsPlayerTwo x = winRowBoardPlayerTwo ( diagonals x) 


-- Extra function

-- Extra functions
    -- i= always 0 column and then +1 for next column
-- r= length head of board -> total amount of columns to know how many times to 'iterate'
-- getColumns :: Board -> Int -> Int -> Columns
-- getColumns (xs :xss) r i = if r > 0 
--   then map (!!i) (xs :xss) : getColumns (xs : xss) (r -1) (i+1) 
--   else []

-- randomElement :: RandomGen g => g -> [a] -> IO a
-- randomElement rnd list = do
--   gen <- getStdGen
--   let (i, _) = randomR (0, length list - 1) gen
--   return $ list !! i

--   replaceNth _ _ [] = []
-- replaceNth n newVal (xs:xss)
--   | n == 0 = newVal:xss
--   | otherwise = xs:replaceNth (n-1) newVal xss
  -- doesColumnExist :: [Disc] -> Bool
  -- doesColumnExist c = find E c || find X c || find O c
  
  -- getMultipleColumns :: Board -> Int -> [[b]]
  -- getMultipleColumns :: [[b]] -> Int -> Int -> [[b]]
  -- getMultipleColumns (xs : xss) j i = case j <= i of
  --   True -> getColumn (xs : xss) j i : []
  --   False -> (xs : xss)
  
    -- getMultipleColumns :: Board -> Int -> [[b]]
  -- getMultipleColumns :: [[b]] -> Int -> Int -> [[b]]
  -- getMultipleColumns (xs : xss) j i = case j <= i of
  --   True -> getColumn (xs : xss) j i : getMultipleColumns (xs : xss) (j + 1) i
  --   False -> (xs : xss)

-- pickOri :: RandomGen g => g -> [a] -> (a, g)
-- pickOri rnd xs =
--   let len = length xs - 1
--       (i, g) = randomR (0, len) rnd
--   in (xs !! i, g)
-- randomTest :: IO ()
-- randomTest = do
--   let stdGen = mkStdGen 2021
--   putStrLn $ fst $ runRand (fromList [("hello", 0.5), ("world", 0.1)]) stdGen

-- findEmptySlotInColumn b i = elemIndex E $ getSelectedColumn b i


-- columnHasEmptySlotM :: [[Disc]] -> Int -> Int -> [[Int]]
-- columnHasEmptySlotM b i = if i < length b then case columnHasEmptySlot b i of
--   True -> [i] : columnHasEmptySlotM b (i + 1) ++ []
--   False -> columnHasEmptySlotM b (i + 1) ++ [] else []