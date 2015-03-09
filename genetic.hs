import System.Random
import Data.List
import qualified Data.Set as Set

numberOfGenes = 5
lowerLimit = -5.0
upperLimit = 5.0

populationSize = 10

eliteRate = 0.15
tournamentT = 2

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

eliteN = round $ populationSize * eliteRate
elite :: [[Double]] -> [[Double]]
elite lst = take eliteN $ sortBy fitnessBetter lst

removeAt :: Int -> [a] -> [a]
removeAt i lst = 
    let (left, right) = splitAt i lst 
    in left ++ (drop 1 right)

choose :: [[Double]] -> Int -> StdGen -> [[Double]]
choose [] _ _ = []
choose _ 0 _ = []
choose lst n g = 
    let (i, newG) = randomR (0, (length lst) - 1) g
    in (lst !! i) : choose (removeAt i lst) (n - 1) newG 

tournament' :: Int -> [[Double]] -> StdGen -> [[Double]]
tournament' 0 _ _ = []
tournament' k lst g =
    let (_, newG) = random g :: (Int, StdGen)
    in (minimumBy fitnessBetter $ choose lst tournamentT newG) : tournament' (k - 1) lst newG 

removeDuplicates lst = Set.toList $ Set.fromList lst

tournament lst g = removeDuplicates $ tournament' (length lst) lst g

