module Main (main) where
import Control.Monad (when)

-- Find all ways to partition a list into pairs
allPairings :: [a] -> [[(a, a)]]
allPairings [] = [[]]  -- Empty list has one pairing: the empty pairing
allPairings [_] = []   -- Single element cannot be paired
allPairings (x:xs) = do
  -- For each element y in xs, pair x with y
  (y, rest) <- selectOne xs
  -- Recursively find all pairings for the remaining elements
  pairing <- allPairings rest
  -- Return the current pair plus the recursive pairing
  return ((x, y) : pairing)

-- Helper function to select one element and return it with the rest
selectOne :: [a] -> [(a, [a])]
selectOne [] = []
selectOne (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- selectOne xs]

-- Example usage with user input
main :: IO ()
main = do
  putStrLn "Enter elements separated by spaces:"
  input <- getLine
  let inputElements = words input
  
  -- Add "Personne" if odd number of elements
  let elements = if odd (length inputElements)
                 then inputElements ++ ["Personne"]
                 else inputElements
  
  let pairings = allPairings elements
  
  putStrLn "\nAll possible pairings:"
  mapM_ print pairings
  
  putStrLn $ "\nTotal number of pairings: " ++ show (length pairings)
  
  -- Show a message if we added "Personne"
  when (odd (length inputElements)) $
    putStrLn "(Note: Added 'Personne' to make an even number of elements)"
