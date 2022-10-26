module Main where

main :: IO ()
main = do
  let ioActions = [putStrLn "first", putStrLn "second", putStrLn "third"]
      first = head ioActions
  first