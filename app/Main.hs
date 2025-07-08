module Main (main) where

import Text.Printf (printf)
import Control.Monad (when)

-- Type definitions
type Element = String
type Elements = [Element]
type Binome = (Element,Element)
type Combination = [Binome]
type Results = [Combination]

-- Functions
splitElementsAt :: Elements -> Int -> (Element,Elements)
splitElementsAt xs i
    | i < 0 || i >= length xs = error $ printf "Cannot split at index %i !" i
    | otherwise = (xs !! i, take i xs ++ drop (i+1) xs)

binomes :: Elements -> Results
binomes [] = [[]]                                       -- If L is empty, return.
binomes [e1] =                                          -- If L has only 1 element => error
    error $ printf "Cannot make binome out of a single element list ! %s" e1
binomes (l1:lr) =                                       -- Split the list in L1 = {the 1rst element} + Lr = {the rest}
    concatMap (\i -> time lr i l1) [0 .. length lr - 1] --Do the following r times - r is the number of elements in Lr.
    where 
        time :: Elements -> Int -> Element -> Results
        time l i l1' = 
            let (li, lri) = splitElementsAt l i -- Split the list Lr => Li = {the ith element} + Lri = {the rest}
                ri = (l1', li)                 -- Create a pair-result Ri with L1 and Li.
                rriList = binomes lri               -- Get all possible combinations for the remaining elements
            in map (ri :) rriList             -- Prepend ri to each combination in rriList

main :: IO ()
main = do
  putStrLn "Enter elements separated by spaces:"
  input <- getLine
  let inputElements = words input
  
  -- Add "Personne" if odd number of elements
  let elements = if odd (length inputElements)
                 then inputElements ++ ["Personne"]
                 else inputElements
  
  let myPairings = binomes elements
    
  putStrLn "\nAll possible pairings :"
  mapM_ print myPairings
  
  putStrLn $ "\nTotal number of pairings: " ++ show (length myPairings)
  
  -- Show a message if we added "Personne"
  when (odd (length inputElements)) $
    putStrLn "(Note: Added 'Personne' to make an even number of elements)"
