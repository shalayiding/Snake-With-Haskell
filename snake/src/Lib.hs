module Lib
  ( main,
    setBorder,
    drawBorder,
    draw,
    codeName,
    singleCode,
    insertList,
    insertMatrix,
    foodCode,
    headCode,
    insBody,
    insertHeadPos,
    snakeMove,
    snakeBody,
    foodCandidates,
    generateFood,
    gmLoop,
    sdir,
    gameStatus,
    getRandomValue,
    eat,
    SnakeDirection,
    Elt,
    -- refresh,
  )
where

import Control.Concurrent
import Control.Monad (when)
import Data.Maybe
import System.Console.ANSI
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.IO.Unsafe
import System.Process (system)
import System.Random

-- Data type defined for the direction of the snake
data SnakeDirection
  = SnakeUp
  | SnakeDown
  | SnakeRight
  | SnakeLeft
  deriving (Show, Eq)

data Elt = Elt Int Char (Int, Int)
  deriving (Eq, Read, Show)

-- Data type defined for whether the snake is Dead or Alive
data SnakeStatus = Alive | Dead
  deriving (Show, Eq)

-- Determines the status of the snake in the game (Alive or Dead)
gameStatus :: [[Elt]] -> [(Int, Int)] -> SnakeStatus
gameStatus brd snake
  | xy `elem` tail snake = Dead
  | eltSize < 1 || code == 1 = Dead
  | otherwise = Alive
  where
    xy = head snake
    eltSize = length (eltFind brd xy)
    (Elt code _ _) = head $ eltFind brd xy

eltFind :: [[Elt]] -> (Int, Int) -> [Elt]
eltFind [] _ = []
eltFind (b : bs) xy = filter (\(Elt _ _ corr) -> corr == xy) b ++ eltFind bs xy

-- Define codes for elements in the game.
-- 1 = '#' corresponds to the border
-- 2 = '@' corresponds to the snake head
-- 3 = 'o' corresponds to the snake body
-- 4 = '0' corresponds to food on the board
singleCode :: Int -> Char
singleCode c
  | c == 1 = '#'
  | c == 2 = '@'
  | c == 3 = 'o'
  | c == 4 = '0'
  | otherwise = ' '

-- Checking code in the matrix, and outputting corresponding character with associated code.
codeName :: [[Elt]] -> [[Elt]]
codeName =
  map
    (map (\(Elt code _ corr) -> Elt code (singleCode code) corr))

-- The dimensions of a matrix are defined the by two integers, m and n.
-- The result is the size of our game board.
setBorder :: Int -> Int -> [[Elt]]
setBorder m n =
  [ [ if x == 0 || x == m - 1 || y == 0 || y == n - 1
        then Elt 1 '#' (y, x)
        else Elt 0 ' ' (y, x)
      | x <- [0 .. m - 1]
    ]
    | y <- [0 .. n - 1]
  ]

-- Draw the border of the game board as well as elements in the game (snake and food).
drawBorder :: [[Elt]] -> [String] -> String
drawBorder [] _ = "\n"
drawBorder (n : ns) [] = convertString n ++ "\n" ++ drawBorder ns []
drawBorder (n : ns) (x : xs) = convertString n ++ x ++ "\n" ++ drawBorder ns xs

convertString :: [Elt] -> String
convertString [] = ""
convertString (n : ns) = s : convertString ns
  where
    (Elt _ s _) = n

insertList :: (Int, Int) -> Int -> [Elt] -> [Elt]
insertList _ _ [] = []
insertList xy t (l : ls)
  | xy == corr = Elt t name corr : insertList xy t ls
  | otherwise = l : insertList xy t ls
  where
    (Elt _ name corr) = l

-- The function insertMatrix takes 3 inputs, (x,y) the location, value, element py (code number), and matrix rs
-- It will overwrite the given matrix (b:bs) at location (x,y) where the corrdinate of Elt is == (x,y)
-- and it replace the code number with given value

insertMatrix :: (Int, Int) -> Int -> [[Elt]] -> [[Elt]]
insertMatrix _ _ [] = []
insertMatrix xy target (b : bs) = insertList xy target b : insertMatrix xy target bs

-- Insert snake head position code into the board
headCode :: (Int, Int) -> [[Elt]] -> [[Elt]]
headCode xy = insertMatrix xy 2

-- Insert the positions of the rest of the snake (the body) into the board.
insBody :: [(Int, Int)] -> [[Elt]] -> [[Elt]]
insBody _ [] = []
insBody [] brd = brd
insBody (p : pos) brd = insBody pos new
  where
    new = insertMatrix p 3 brd

insertHeadPos :: [(Int, Int)] -> [[Elt]] -> [[Elt]]
insertHeadPos xs brd = insBody (tail xs) $ headCode (head xs) brd

-- Draw the game board
draw :: [[Elt]] -> [String] -> IO ()
draw ns contents = do
  putStrLn (drawBorder ns contents)

-- Insert food code into the board
foodCode :: (Int, Int) -> [[Elt]] -> [[Elt]]
foodCode xy = insertMatrix xy 4

-- Gives back a list of possible food candidate locations on the board.
foodCandidates :: [[Elt]] -> [(Int, Int)]
foodCandidates [] = []
foodCandidates (b : bs) = locals ++ foodCandidates bs
  where
    emptyspace = filter (\(Elt code _ _) -> code == 0) b
    locals = map (\(Elt _ _ xy) -> xy) emptyspace

-- Get a random value within some range (low, high).
getRandomValue :: Int -> Int -> Int
getRandomValue low high = unsafePerformIO (getStdRandom (randomR (low, high)))

-- randomly generate food here by inserted randomly into list of free food positions (foodCandidates)
generateFood :: [(Int, Int)] -> (Int, Int)
generateFood fCand = fCand !! ind
  where
    ind = getRandomValue 1 (length fCand) - 2

-- Every time a piece of food is eaten, this function will append a new body part onto the snake.
eat :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
eat s d = s ++ [d]

scoreBoard :: [String] -> Int -> Int -> [String]
scoreBoard [] _ _ = []
scoreBoard (s : str) p1 p2
  | s == "    CURRENT SCORE :" = (s ++ show p1) : scoreBoard str p1 p2
  | s == "    CURRENT SPEED :" = (s ++ show p2) : scoreBoard str p1 p2
  | otherwise = s : scoreBoard str p1 p2

gmLoop :: SnakeStatus -> MVar Char -> MVar Char -> [(Int, Int)] -> SnakeDirection -> (Int, Int) -> [String] -> IO ()
gmLoop status future_input_real latest_input_real xs d food ui
  -- First condition checks if the snake is alive
  | status /= Dead =
    do
      _ <- system "clear" -- clear the screen before drawing
      draw fbrd (scoreBoard ui (length xs) ((length xs * 10) `div` 20))
      -- print (gameStatus brdSnakeFood xs)
      threadDelay ((20 `div` length xs) * 30000) -- delay, everytime length increases, speed of snake increases
      -- Sequence of actions are:
      -- Get a character << fill an MVar if empty, block otherwise (future input) << fill an MVar if empty (latest input)
      _ <-
        forkIO
          ( do
              c <- getChar
              putMVar future_input_real c
              _ <- tryPutMVar latest_input_real c -- tryPutMVar is a non-blocking version of putMVar
              return ()
          )
      wait future_input_real latest_input_real -- future input (fi) and latest input (li)
  | otherwise = do
    _ <- system "clear"
    putStr "You Lost \n"
  where
    snakeBrd = insertHeadPos xs (setBorder 45 45) -- [[Int]] snake body in board
    fCand = foodCandidates snakeBrd -- possible food candidate spawn points
    rf = generateFood fCand -- random food generated
    fbrd = codeName $ foodCode food snakeBrd -- new board after food is inserted
    updStat = gameStatus fbrd xs -- update status
    ns = eat xs food -- new snake after eating food
    -- This function will take care of reading in new input from the user as well as recursing on gmLoop
    wait fi' li' = do
      input <- tryTakeMVar fi'
      old_input <- takeMVar li'
      if isJust input -- if there is an input
        then do
          putMVar li' (fromJust input) -- fill MVar with new input, otherwise wait if full
          when (status /= Dead) $ do
            if head xs == food -- check if the head of the snake has interacted with a food element
            -- if so, we want to update the snake with an new snake after eating the food so gmLoop takes ns
            -- otherwise, we want to do nothing to the body of the snake and continue the gmLoop as is
              then gmLoop updStat fi' li' (setSnakeBody (fromJust input) d ns) (sdir (fromJust input) d) rf ui
              else gmLoop updStat fi' li' (setSnakeBody (fromJust input) d xs) (sdir (fromJust input) d) food ui
        else do
          putMVar li' old_input
          when (status /= Dead) $ do
            if head xs == food -- check if the head of the snake has interacted with a food element
            -- if so, we want to update the snake with an new snake after eating the food so gmLoop takes ns
            -- otherwise, we want to do nothing to the body of the snake and continue the gmLoop as is
              then gmLoop updStat fi' li' (setSnakeBody old_input d ns) (sdir old_input d) rf ui
              else gmLoop updStat fi' li' (setSnakeBody old_input d xs) (sdir old_input d) food ui

-- Utilize the functions sdir, snakeMove, and snakeBody to set the direction
setSnakeBody :: Char -> SnakeDirection -> [(Int, Int)] -> [(Int, Int)]
setSnakeBody input cur_dir xs = snakeBody (snakeMove (sdir input cur_dir) (head xs)) xs

-- Relate corresponding WASD key presses with changes in snake direction
sdir :: Char -> SnakeDirection -> SnakeDirection
sdir key snkdir = case key of
  'd' -> SnakeRight
  'a' -> SnakeLeft
  'w' -> SnakeUp
  's' -> SnakeDown
  _ -> snkdir

-- Move the snake in the corresponding direction each frame
snakeMove :: SnakeDirection -> (Int, Int) -> (Int, Int)
snakeMove d (x, y) = case d of
  SnakeUp -> (x - 1, y)
  SnakeDown -> (x + 1, y)
  SnakeRight -> (x, y + 1)
  SnakeLeft -> (x, y - 1)

snakeBody :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
snakeBody x xs = x : init xs

-- Split a string at a character and return a list of strings containing the split parts
-- of the original string.
splitnew :: Char -> String -> [String]
splitnew _ "" = []
splitnew delimiter str =
  let (start, rest) = break (== delimiter) str
      (_, remain) = span (== delimiter) rest
   in start : splitnew delimiter remain

-- Main is printing the UI, running the game loop, gmLoop and checking
-- for new keypresses as the game runs.
main :: IO ()
main = do
  System.Console.ANSI.hideCursor
  ui <- readFile "ui.txt"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False -- don't echo key presses to terminal
  fi <- newEmptyMVar -- (fi) future input
  li <- newEmptyMVar -- (li) lastest input
  putMVar li 'd' -- assuming a previous input the first time
  gmLoop Alive fi li dfs SnakeRight (10, 10) (map fnl (splitnew '\n' ui))
  where
    dfs = reverse [(1, 1), (1, 2), (1, 3), (1, 4)] -- default snake position
    fnl = filter (/= '\n') -- eliminate newline char