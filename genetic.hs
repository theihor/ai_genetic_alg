import System.Random
import Data.List
import qualified Data.Set as Set

epsilon = 1e-16

numberOfGenes :: Int
numberOfGenes = 12  
lowerLimit = -5.0
upperLimit = 5.0

populationSize :: Int
populationSize = numberOfGenes * 10

eliteRate :: Double
eliteRate = 0.15
tournamentT = 2

maxEpoch :: Int
maxEpoch = round $ 2.0 * (fromIntegral numberOfGenes) * (log ((upperLimit - lowerLimit) / epsilon))

mutationDepth :: Double -> Double
mutationDepth i = c / (c + i ** 6) where c = fromIntegral maxEpoch  -- i is epoch counter  

mutationWidth :: Int -> Int
mutationWidth i = (c - i) * n `div` (c + 1) + 1 
    where c = maxEpoch
          n = numberOfGenes

hypersphere :: [Double] -> Double 
hypersphere lst = sum [x * x | x <- lst] 

fitness :: [Double] -> Double
fitness lst = hypersphere lst

take' :: Int -> [Double] -> [[Double]]
take' 0 _ = []
take' n lst = 
    let (one, rest) = splitAt numberOfGenes lst
    in one : take' (n - 1) rest

genPhenotypes :: Int -> StdGen -> [[Double]]
genPhenotypes n g = take' n $ randomRs (lowerLimit, upperLimit) g

fitnessBetter :: [Double] -> [Double] -> Ordering
fitnessBetter lst1 lst2
    | f1 > f2 = GT
    | f1 < f2 = LT
    | f1 == f2 = EQ
    where f1 = fitness(lst1)
          f2 = fitness(lst2)

newGen g = let (_, g') = random g :: (Int, StdGen) in g' 

eliteN = round $ (fromIntegral populationSize) * eliteRate
elitism :: [[Double]] -> ([[Double]], [[Double]])
elitism lst = splitAt eliteN $ sortBy fitnessBetter lst -- returns (elite, notElite)

removeAt :: Int -> [a] -> [a]
removeAt i lst = 
    let (left, right) = splitAt i lst 
    in left ++ (drop 1 right)

choose :: [a] -> Int -> StdGen -> [a]
choose [] _ _ = []
choose _ 0 _ = []
choose lst n g = 
    let (i, newG) = randomR (0, (length lst) - 1) g
    in (lst !! i) : choose (removeAt i lst) (n - 1) newG 

tournament' :: Int -> [[Double]] -> StdGen -> [[Double]]
tournament' 0 _ _ = []
tournament' k lst g = (minimumBy fitnessBetter $ choose lst tournamentT newG) : tournament' (k - 1) lst newG 
    where newG = newGen g

removeDuplicates lst = Set.toList $ Set.fromList lst

tournament lst g = removeDuplicates $ tournament' (length lst) lst g

crossover :: [Double] -> [Double] -> StdGen -> [Double]
crossover father mother g =
    let (i, g1) = randomR (0, (length father) - 1) g
        (j, _) = randomR (i, (length father) - 1) g1
    in (take i father) ++ take (j - i) (drop i mother) ++ (drop j father)

select :: [[Double]] -> StdGen -> [[Double]]
select lst g = 
    let (elite, rest) = elitism lst
        champions = tournament rest g
    in take populationSize $ sortBy fitnessBetter (elite ++ champions)

genList g = (\(_, g') -> g' : genList g') (random g :: (Int, StdGen))

reproduce lst gen = 
    let pairs = [ (m, f) | m <- lst, f <- lst, m /= f ]
    in map (\((m, f), g) -> crossover m f g) $ zip pairs $ genList gen

applyMutation p [] = p
applyMutation p (m:ms) = 
    let (l, r) = splitAt (fst m) p
    in applyMutation (l ++ [head r + snd m] ++ drop 1 r) ms

mutate :: Int -> [Double] -> StdGen -> [Double]
mutate i p g = let (n, g') = randomR (1, mutationWidth i) g 
                   genes = choose [0..numberOfGenes-1] n g'
                   mutationMax = 0.5 * (mutationDepth $ fromIntegral i) * (upperLimit - lowerLimit)
                   mutations = take n $ randomRs (-mutationMax, mutationMax) (newGen g')
               in applyMutation p $ zip genes mutations 

evolve i lst g 
    | i >= maxEpoch = lst
    | otherwise
        lst
    
