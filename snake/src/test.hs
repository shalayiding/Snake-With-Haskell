-- stack ghci --package QuickCheck test.hs

module Test
  ( prop_setBorderLength,
    prop_randomGenerator,
    prop_checkEat,
    prop_foodGenerate,
  )
where

import Lib
import Test.QuickCheck

prop_randomGenerator :: Int -> Int -> Bool
prop_randomGenerator a b = (a <= 0 || b <= 0 || a > b) || (a <= result && result <= b)
  where
    result = getRandomValue a b

prop_setBorderLength :: Int -> Int -> Bool
prop_setBorderLength m n = (n <= 0 || m <= 0 || (m * n < 4)) || sum (map length (setBorder m n)) == m * n

-- Checking if the eating function is inserting a new body part of the snake
prop_checkEat :: [(Int, Int)] -> (Int, Int) -> Bool
prop_checkEat snake food = food `elem` newsnake
  where
    newsnake = eat snake food

prop_foodGenerate :: [(Int, Int)] -> Bool
prop_foodGenerate foods = (length foods < 4) || newfood `elem` foods
  where
    newfood = generateFood foods

-- prop_insertList :: (Int, Int) -> Int -> [Elt] -> Bool
-- prop_insertList xy t ls = length ls == length newls
--   where
--     newls = insertList xy t ls

-- insertList :: (Int, Int) -> Int -> [Elt] -> [Elt]
-- insertList _ _ [] = []
-- insertList xy t (l : ls)
--   | xy == corr = Elt t name corr : insertList xy t ls
--   | otherwise = l : insertList xy t ls
--   where
--     (Elt _ name corr) = l

-- insertMatrix :: (Int, Int) -> Int -> [[Elt]] -> [[Elt]]
-- insertMatrix _ _ [] = []
-- insertMatrix xy target (b : bs) = insertList xy target b : insertMatrix xy target bs

-- prop_inputCheck :: Char -> SnakeDirection -> Bool
-- prop_inputCheck c dr = case sdir c dr of
--   SnakeRight -> True
--   SnakeLeft -> True
--   SnakeUp -> True
--   SnakeDown -> True
--   _ -> False

-- (SnakeRight == sdir c dr) || (SnakeLeft == sdir c dr) || (SnakeUp == sdir c dr) || (SnakeDown == sdir c dr)

-- sdir :: Char -> SnakeDirection -> SnakeDirection
-- sdir key snkdir = case key of
--   'd' -> SnakeRight
--   'a' -> SnakeLeft
--   'w' -> SnakeUp
--   's' -> SnakeDown
--   _ -> snkdir

-- generateFood :: [(Int, Int)] -> (Int, Int)
-- generateFood fCand = fCand !! ind
--   where
--     ind = getRandomValue 1 (length fCand) - 2

-- eat :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
-- eat s d = s ++ [d]

-- verboseCheck prop_setBorderLength

--  verboseCheck (withMaxSuccess 10000 prop_setBorderLength)
