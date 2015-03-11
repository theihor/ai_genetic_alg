import System.Random
import Data.List
import Data.Ord
import qualified Data.Set as Set

epsilon = 1e-16

numberOfGenes :: Int
numberOfGenes = 12 
lowerLimit = -5.0
upperLimit = 5.0

populationSize :: Int
populationSize = numberOfGenes * numberOfGenes 

eliteRate :: Double
eliteRate = 0.15
tournamentT = 2

maxEpoch :: Int
maxEpoch = round $ 2.0 * (fromIntegral numberOfGenes) * (log ((upperLimit - lowerLimit) / epsilon))

mutationDepth :: Double -> Double
mutationDepth i = c / (c + i ** k) -- i is epoch counter  
    where c = fromIntegral maxEpoch
          k = (logBase c (1 / epsilon - 1))  

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

removeAt :: Int -> [a] -> [a]
removeAt i lst = 
    let (left, right) = splitAt i lst 
    in left ++ (drop 1 right)

genPhenotypes :: Int -> StdGen -> [[Double]]
genPhenotypes n g = take' n $ randomRs (lowerLimit, upperLimit) g

fitnessBetter :: [Double] -> [Double] -> Ordering
fitnessBetter lst1 lst2
    | f1 > f2 = GT
    | f1 < f2 = LT
    | f1 == f2 = EQ
    where f1 = fitness(lst1)
          f2 = fitness(lst2)

takeBest' :: Int -> [(([Double], Double), Int)] -> [(([Double], Double), Int)]
takeBest' 0 _ = []
takeBest' _ [] = []
takeBest' n lst = 
    let (best, i) = minimumBy (comparing (\((_, f), _) -> f)) lst
    in (best, i) : (takeBest' (n - 1) $ removeAt i lst)

takeBest :: Int -> [[Double]] -> [[Double]]
takeBest 0 _ = []
takeBest _ [] = []
takeBest n lst = map (\((p, _), _) -> p) $ takeBest' n $ zip (zip lst (map fitness lst)) [0..]

newGen g = let (_, g') = random g :: (Int, StdGen) in g' 

eliteN = round $ (fromIntegral populationSize) * eliteRate
elitism :: [[Double]] -> [[Double]]
elitism lst = takeBest eliteN lst

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

genList g = (\(_, g') -> g' : genList g') (random g :: (Int, StdGen))

reproduce lst gen = 
    let pairs = [ (m, f) | m <- lst, f <- lst, m /= f ]
    in map (\((m, f), g) -> crossover m f g) $ zip pairs $ genList gen

limitedAdd x y = 
    let res = x + y
    in if res > upperLimit then upperLimit
       else if res < lowerLimit then lowerLimit
           else res

applyMutation p [] = p
applyMutation p (m:ms) = 
    let (l, r) = splitAt (fst m) p
    in applyMutation (l ++ [limitedAdd (head r) (snd m)] ++ drop 1 r) ms

mutate :: Int -> [Double] -> StdGen -> [Double]
mutate i p g = let (n, g') = randomR (1, mutationWidth i) g 
                   genes = choose [0..numberOfGenes-1] n g'
                   mutationMax = 0.5 * (mutationDepth $ fromIntegral i) * (upperLimit - lowerLimit)
                   mutations = take n $ randomRs (-mutationMax, mutationMax) (newGen g')
               in applyMutation p $ zip genes mutations 

evolve i lst g 
    | i >= maxEpoch = lst
    | otherwise = let elite = elitism lst
                      champions = tournament (filter (\p -> elem p elite) lst) g
                      parents = elite ++ champions
                      children = (map (\(g', kid) -> mutate i kid g') $ zip (genList g) $ reproduce parents g)
                  in takeBest populationSize children
        
bestSolution lst = minimumBy fitnessBetter lst 

evolution :: Int -> [[Double]] -> StdGen -> IO ()
evolution i lst g = do
    if i < maxEpoch then do
        let generation = evolve i lst g
        putStrLn $ (show i) ++ ": " ++ (show $ fitness $ bestSolution generation)
        evolution (i + 1) generation (newGen g)
    else print "Done."

main = do
    g <- newStdGen
    evolution 0 (genPhenotypes populationSize g) g 
    
