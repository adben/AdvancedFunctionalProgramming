
module Main where

import           Data.Vector         ((!), (!?), (//))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

constructVectors :: IO ()
constructVectors = do
  let e = V.empty :: V.Vector Int
  let s = V.singleton "one" :: V.Vector String
  let r = V.replicate 10 "same" :: V.Vector String
  let g = V.generate 10 (const "generated")  :: V.Vector String
  let i = V.iterateN 10 ('x':) "o"
  putStrLn $ "Empty vector " ++ show e
  putStrLn $ "Singleton vector " ++ show s
  putStrLn $ "Replicated vector " ++ show r
  putStrLn $ "Generated vector " ++ show g
  putStrLn $ "Iterated vector " ++ show i

enumeratedVectors :: IO ()

enumeratedVectors = do
  putStrLn "Create a list of 10 floats, 1.1, 2.1 ... etc"
  print $ (V.enumFromN 1.1 10 :: V.Vector Float)
  putStrLn "Create a list of 10 floats, incremented by 0.5"
  print $ (V.enumFromStepN 1.1 0.5 10 :: V.Vector Float)

vectorAsList :: IO ()
vectorAsList = do
  let vec = V.enumFromStepN 1 3 30 :: V.Vector Int
  putStrLn "All elements but the last"
  print $ V.init vec
  putStrLn "Head of the vector"
  print $ V.head vec
  putStrLn "Tail of the vector"
  print $ V.tail vec
  putStrLn "Take first five elements"
  print $ V.take 5 vec
  putStrLn "Drop first five elements"
  print $ V.drop 5 vec
  putStrLn "Prepend and Append an element"
  print $ V.cons 99 vec
  print $ V.snoc vec 99
  putStrLn "Concatenate two vectors"
  print $ vec V.++ (V.fromList [101,102,103])

bulkOperations :: IO ()
bulkOperations = do
  putStrLn "Replace elements by list of index and value."
  print $ (V.fromList [2,5,8]) // [(0,3),(1,6),(2,9)]
  putStrLn "Update with another vector with index and value"
  print $ (V.fromList [2,5,8]) `V.update` (V.fromList [(0,3),
                                                       (1,6),(2,9)])

indexing :: IO ()
indexing = do
  let vec = V.enumFromStepN 1.1 0.5 20
  putStrLn "Input Vector"
  print vec
  putStrLn "Accessing 10 th element"
  print $ vec ! 9
  putStrLn "Safely accessing 10th element, and 100th one"
  print $ vec !? 9
  print $ vec !? 99

mutableVec :: IO (MV.IOVector Int)
mutableVec = do
  v <- MV.new 2  -- Create a vector of size 2
  MV.write v 0 1 -- Assign all values
  MV.write v 1 2
  return v

useMutable :: IO ()
useMutable = do
  mv <- mutableVec
  vec <- V.freeze mv
  putStrLn "Mutable to vector conversion"
  print vec

main :: IO ()
main = do
  putStrLn "Constructing Vectors"
  constructVectors
  putStrLn "Enumerating Vectors"
  enumeratedVectors
  putStrLn "Vector as fast lists"
  vectorAsList
  putStrLn "Bulk operations on vector"
  bulkOperations
  putStrLn "Accessing elements of vector"
  indexing
  putStrLn "Working with mutable, and converting it to vector"
  useMutable
