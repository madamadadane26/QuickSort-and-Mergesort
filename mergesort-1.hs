import System.Random
import Control.Monad
import Data.List
import System.CPUTime

---Merge Sort in Haskell---

-- Merge sort implementation
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
    -- Split the input list into two equal halves
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

    -- Merge two sorted lists into a single sorted list
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x <= y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

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
  measureExecutionTime mergeSort "random" randomNumbers

  let n2 = 1000000
  let sortedList = [1..n2]
  let reversedList = reverse sortedList

  measureExecutionTime mergeSort "sorted" sortedList
  measureExecutionTime mergeSort "reversed" reversedList
