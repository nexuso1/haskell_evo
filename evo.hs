-- Environment for Evolutionary algorithms
-- You provide a fitness function, and the enironment 
-- takes care of the rest.

import System.Random

-- pick n elements from the list randomly
pick_n :: [a] -> Int -> [a]
pick_n list 0 = []
pick_n list n = picked : (pick_n (delete picked list) (n - 1))
                    where picked = list !! (randomRIO (0, n))

-- Extracts the sub-sequence between start and finish indexes
extract_range :: Ord a => [a] -> Int -> Int -> [a]
extract_range list start finish | start == finish = [list !! finish]
                                | otherwise       = list !! start : (extract_range list (start + 1) finish)

-- -- N point mutation, source alternated between parents
-- n_point_mutate :: Ord a => [a] -> [a] -> Int ->[a]
-- n_point_mutate indiv n = 
                         
-- -- Uniform mutation, each position taken randomly from one of the parents
-- uniform_mutate :: Ord a => [a] -> [a] -> Float [a]

-- evolve :: Ord a => ([a] -> a)