-- Environment for Evolutionary algorithms
-- You provide a fitness function, and the enironment 
-- takes care of the rest.

import Data.List
import Data.Functor
import ControlMonadRandomClass(weighted, uniform, getRandomR)

nicePrint :: (Show a) => [(a, Rational)] -> IO ()
nicePrint [(a,b)] = do
        let temp = (a, fromRational b)
        print temp
nicePrint ((a,b):rest) = do
    let temp = (a, fromRational b)
    print temp
    nicePrint rest
-- pick n elements from the list randomly
pickN :: (Eq a) => [a] -> Int -> IO [a]
pickN _ 0 = return []
pickN list n = do
                picked <- uniform list
                rest <- pickN (delete picked list) (n - 1)
                return $ picked : rest

-- Extracts the sub-sequence between start and finish indexes
extractRange :: Ord a => [a] -> Int -> Int -> [a]
extractRange list start finish | start == finish - 1 = [list !! start]
                               | start == finish = [] -- if the length of the sub-sequence is 0, extract empty list
                               | otherwise       = list !! start : extractRange list (start + 1) finish

makePairs :: [a] -> [(a,a)]
makePairs [] = []
makePairs [a] = []
makePairs (a:b:rest) = (a,b):makePairs rest

-- N point crossover, source alternated between parents
-- args : par1,par2 = parents, n = # of crossover points
nPointCrossover :: Ord a => [a] -> [a] -> Int -> IO ([a], [a])
nPointCrossover par1 par2 n = do
                            idxs <- pickN [0..length par1] n
                            let sorted = sort idxs
                            return $ alternateCrossover par1 par2 [] [] sorted 0 0

-- Alternate between parents for crossover
-- args : par1,par2 = parents, idxs = indexes where to switch, aka first index of the other parent, i = current position, parent_i = which parent to take from
alternateCrossover :: Ord a => [a] -> [a] -> [a] -> [a] -> [Int] -> Int -> Int -> ([a], [a])
alternateCrossover [] [] ch1 ch2 [] _ _ = (reverse ch1, reverse ch2)
alternateCrossover par1 par2 ch1 ch2 [] i parent_i = alternateCrossover (tail par1) (tail par2) (currentHead : ch1) (otherHead : ch2) [] i parent_i
                                                        where currentHead = if parent_i == 0 then head par1 else head par2
                                                              otherHead = if parent_i == 0 then head par2 else head par1
alternateCrossover par1 par2 ch1 ch2 idxs i parent_i | i < head idxs = alternateCrossover (tail par1) (tail par2) (currentHead : ch1) (otherHead : ch2) idxs (i + 1) parent_i
                                                     | i == head idxs && (null par1 || null par2) = (reverse ch1, reverse ch2) -- the only crossover point was at the end
                                                     | otherwise = alternateCrossover (tail par1) (tail par2) (otherHead : ch1) ( currentHead : ch2) (tail idxs) (i + 1) ((parent_i +1) `mod` 2)
                                                        where currentHead = if parent_i == 0 then head par1 else head par2
                                                              otherHead = if parent_i == 0 then head par2 else head par1

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

-- Uniform selection
-- arguments: fitness = fit. function, population = list of individuals,
selectionUni :: ([a] -> Rational)->[[a]]-> IO [[a]]
selectionUni fitness population = do
                                     let weights = map (\x -> (x, fitness x)) population
                                     weightsNTimes weights (length population)
-- Tourney selection
-- arguments: fitness = fit. function, list = list of individuals, n = tourney size, m = number of repetitions
selectionTourney :: ([a] -> Rational) -> [[a]] -> Int -> Int -> IO [[a]]
selectionTourney _ _ _ 0 = return []
selectionTourney fitness list n m = do 
                                     chosen <- uniformNTimes list n
                                     let winner = foldl (\x y -> if fitness y > fitness x then y else x) (head chosen) chosen
                                     rest <- selectionTourney fitness list n (m-1)
                                     return $ winner:rest

-- creates a starting population of individuals for evolution
createPopulation :: (Int -> IO a) -> Int -> Int-> IO [a]
createPopulation _ 0 _ = return []
createPopulation createFunc pop_size indiv_size= do
                                                  indiv <- createFunc indiv_size
                                                  rest <- createPopulation createFunc (pop_size - 1) indiv_size
                                                  return $ indiv : rest

-- generic crossover function, applies the crossover to the pairs given in its argument
crossover :: Ord a => [([a],[a])] -> Int -> Float -> IO [[a]]
crossover [] _ _ = return []
crossover ((a,b):pairs) n p = do 
                    rest <- crossover pairs n p
                    num <- getRandomR (0.0, 1.0)
                    if num < p then do
                        (ch1, ch2) <- nPointCrossover a b n
                        return $ ch1:ch2:rest
                    else do
                        return $ a:b:rest

-- general mutation function, applies the mutate function from its argument to the 
-- whole population
mutate :: Ord a => (a -> Float -> IO a) -> [a] -> Float -> Float -> IO [a]
mutate _ [] _ _ = return []
mutate mutFunc population p_mut p_mut_indiv =
    do
        num <- getRandomR (0.0, 1.0) -- get a random in this range
        rest <- mutate mutFunc (tail population) p_mut p_mut_indiv
        if num < p_mut then do
            mutated <- mutFunc (head population) p_mut_indiv
            return $ mutated : rest
        else do
            return $ (head population) : rest

-- Recursive evolve function that takes a population as a parameter and executes it until there are no remaining generations
evolveImpl :: (Show a, Ord a) => ([a] -> Rational) -> ([a] -> Float -> IO [a])-> [[a]] -> Int -> Float -> Float -> Float -> IO [([a], Rational)]
evolveImpl _ _ _ 0 _ _ _ = return []
evolveImpl fitness mutFunc population gens p_mut_indiv p_mut p_cross = 
    do
        selected <- selectionTourney fitness population 5 (length population) -- selection
        children <- crossover (makePairs selected) 1 p_cross -- crossover
        mutated_children <- mutate (mutFunc) children p_mut p_mut_indiv -- mutation
        let best_child = foldl (\x y -> if fitness y > fitness x then y else x) (head mutated_children) mutated_children -- remember the best child
        rest <- evolveImpl fitness mutFunc mutated_children (gens - 1) p_mut_indiv p_mut p_cross -- other generations
        return $ (best_child, fitness best_child) : rest

-- Gets evolution parameters such as
-- fitness - fitness function, has to return a Rational value
-- createIndiv - function that creates an individual
-- mutFunc - mutation function which takes an inidividual and a float as an argument
-- pop_size - population size
-- indiv_size - size of an individual
-- gens - # of generations
-- p_mut_indiv - prob. of mutation of an individual
-- p_mut - prob. of mutation on a specific point
-- p_cross - prob. of crossover between individuals
-- and returns a historical list of best individuals in each generation, based on the fitness function

evolve :: (Show a, Ord a) => ([a] -> Rational) -> (Int -> IO [a]) -> ([a] -> Float -> IO [a])-> Int -> Int -> Int -> Float -> Float -> Float -> IO [([a], Rational)]
evolve fitness createIndiv mutFunc pop_size indiv_size gens p_mut_indiv p_mut p_cross = 
    do
        population <- createPopulation createIndiv pop_size indiv_size
        evolveImpl fitness mutFunc population gens p_mut_indiv p_mut p_cross

-- ///////////////////////////////////////////////////////////////////////// --
-- //////////////////////////////// Demos ////////////////////////////////// --
-- ///////////////////////////////////////////////////////////////////////// --

-- ////////////////////////////// Most Ones //////////////////////////////// --

-- Demonstration of the most ones problem
mostOnesDemo :: IO ()
mostOnesDemo =
    do
        res <- evolve mostOnesFitness mostOnesCreateIndiv mostOnesMutate 100 50 100 0.1 0.1 0.3
        nicePrint res


-- Most 1s fitness function
mostOnesFitness ::(Num a, Eq a) => [a] -> Rational
mostOnesFitness list = fromIntegral $ length $ filter (==1) list

-- Creates a random array of 1s and 0s
mostOnesCreateIndiv :: Int -> IO [Int]
mostOnesCreateIndiv 0 = return []
mostOnesCreateIndiv n = do
                         num <- getRandomR (0, 1)
                         rest <- mostOnesCreateIndiv (n-1)
                         return $ num : rest

-- Randomly changes a position to its opposite
mostOnesMutate :: [Int] -> Float -> IO [Int]
mostOnesMutate [] _ = return []
mostOnesMutate indiv p = do
                          num <- getRandomR (0.0, 1.0) -- get a random float in this range
                          rest <- mostOnesMutate (tail indiv) p
                          if num < p then do
                              let new = ((head indiv) + 1) `mod` 2
                              return $ new : rest
                          else do
                              return $ head indiv : rest

-- //////////////////////////// Subset sum less than k ///////////////////////// --

subsetFitness :: (Num a, Eq a, Integral a) => [a] -> a -> [Int] -> Rational
subsetFitness set max indiv =
    let sum = subsetFitnessImpl set indiv
    in if sum > max then 0 else fromIntegral sum

subsetFitnessImpl :: (Num a, Eq a, Integral a) => [a] -> [Int] -> a
subsetFitnessImpl [] [] = 0
subsetFitnessImpl set indiv | head indiv == 1 = head set + subsetFitnessImpl (tail set) (tail indiv)
                            | otherwise = subsetFitnessImpl (tail set) (tail indiv)

subsetCreateIndiv :: Int -> IO [Int]
subsetCreateIndiv 0 = return []
subsetCreateIndiv n = do
                         num <- getRandomR (0, 1)
                         rest <- mostOnesCreateIndiv (n-1)
                         return $ num : rest

subsetMutate :: [Int] -> Float -> IO [Int]
subsetMutate [] _ = return []
subsetMutate indiv p = do
                          num <- getRandomR (0.0, 1.0) -- get a random float in this range
                          rest <- mostOnesMutate (tail indiv) p
                          if num < p then do
                              let new = ((head indiv) + 1) `mod` 2
                              return $ new : rest
                          else do
                              return $ head indiv : rest

subsetDemo :: IO ()
subsetDemo = do
    res <- evolve (subsetFitness [10, 2, 3, 6, 7, 9, 3, 5, 4, 0, 1, 4] 49) subsetCreateIndiv subsetMutate 10 12 50 0.1 0.1 0.5
    nicePrint res