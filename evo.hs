-- Environment for Evolutionary algorithms
-- You provide a fitness function, and the enironment 
-- takes care of the rest.

import Data.List
import Data.Functor
import ControlMonadRandomClass(weighted, fromList, uniform)

-- pick n elements from the list randomly
-- pickN :: (Eq a, RandomGen g) => [a] -> Int -> g -> ([a], g)
-- pickN list 0 g= ([], g)
-- pickN list n g = 
--        let (num, g') = randomRIO (0, n - 1) g
--            picked = list !! num
--            newList = delete picked list
--            (res, g'') = pickN newList (n - 1) g'
--         in (picked : res, g'')

pickN :: (Eq a) => [a] -> Int -> IO [a]
pickN _ 0 = return []
pickN list n = do
                picked <- uniform list
                rest <- pickN (delete picked list) (n - 1)
                return $ picked : rest

-- Extracts the sub-sequence between start and finish indexes
-- extractRange :: Ord a => [a] -> Int -> Int -> [a]
-- extractRange list start finish | start == finish = [list !! finish]
--                                | otherwise       = list !! start : extractRange list (start + 1) finish

-- -- N point mutation, source alternated between parents
-- nPointMutate :: Ord a => [a] -> [a] -> Int -> [a]
-- nPointMutate par1 par2 n = fst $ fmap (pickN par1 n) getStdGen

-- -- Uniform mutation, each position taken randomly from one of the parents
-- uniform_mutate :: Ord a => [a] -> [a] -> Float [a]

-- Picks n elements from a list based on their weights, and returns them packed in an IO action
weightsNTimes :: [(a, Rational)] -> Int -> IO [a]
weightsNTimes list 0 = return []
weightsNTimes list n = do
                        current <- weighted list
                        rest <- weightsNTimes list (n - 1)
                        return $ current : rest

-- Uniformly picks an element from the list n-times, returns an IO action of list of picked elements
uniformNTimes :: [a] -> Int -> IO [a]
uniformNTimes list 0 = return []
uniformNTimes list n = do
                        current <- uniform list
                        rest <- uniformNTimes list (n-1)
                        return $ current : rest

-- Most 1s fitness function
mostOnesFitness ::(Num a, Eq a) => [a] -> Rational
mostOnesFitness list = fromIntegral $ length $ filter (==1) list

-- Uniform selection
-- arguments: fitness = fit. function, list = list of individuals, n = tourney size, m = number of repetitions
selectionUni :: ([a] -> Rational)->[[a]]-> Float -> IO [[a]]
selectionUni fitness population p = do
                                     let weights = map (\x -> (x, fitness x)) population
                                     weightsNTimes weights (length population)
-- Tourney selection
-- arguments: fitness = fit. function, list = list of individuals, n = tourney size, m = number of repetitions
selectionTourney :: ([a] -> Rational) -> [[a]] -> Int -> Int -> IO [[a]]
selectionTourney fitness list n m = do 
                            chosen <- uniformNTimes list n
                            let winner = foldl (\x y -> if fitness y > fitness x then y else x) (list !! 0) list
                            rest <- selectionTourney fitness list n (m-1)
                            return $ winner:rest




-- Gets evolution parameters such as
-- fitness - fitness function
-- pop_size - population size
-- gens - # of generations
-- p_mut_indiv - prob. of mutation of an individual
-- p_mut - prob. of mutation on a specific point
-- p_cross - prob. of crossover between individuals
-- and returns a historical list of best individuals in each generation, based on the fitness function
-- evolve :: Ord b => ([a] -> b) -> Int -> Int -> Float -> Float -> [(a, b)]
-- evolve fitness pop_size gens p_mut_indiv p_mut p_cross = 