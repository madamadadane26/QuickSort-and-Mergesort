import System.Random
import Control.Monad
import Data.List
import System.CPUTime
---Quick Sort in Haskell---


-- Quicksort implementation
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Generates a list of n random numbers
generateRandomNumbers :: Int -> IO [Int]
generateRandomNumbers n
  | n > 0 = do
    gen <- newStdGen
    return $ take n (randomRs (1, 1000) gen)
  | otherwise = return []

-- Measures the execution time of the given sorting function with the provided input
measureExecutionTime :: ([Int] -> [Int]) -> String -> [Int] -> IO ()
measureExecutionTime sortFn listType input = do
  startTime <- getCPUTime
  let sortedNumbers = sortFn input
  endTime <- getCPUTime
  print $ "Sorting " ++ show (length input) ++ " " ++ listType ++ " numbers took " ++ show (fromIntegral (endTime - startTime) / 10^12) ++ " seconds."

main :: IO ()
main = do
  let n1 = 1000
  randomNumbers <- generateRandomNumbers n1
  measureExecutionTime quicksort "random" randomNumbers

  let n2 = 1000000
  let sortedList = [1..n2]
  let reversedList = reverse sortedList

  measureExecutionTime quicksort "sorted" sortedList
  measureExecutionTime quicksort "reversed" reversedList
