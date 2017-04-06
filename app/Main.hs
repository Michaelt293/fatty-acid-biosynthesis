module Main where

import Lib
import Data.List (intercalate)

main :: IO ()
main = do
  putStrLn "If the program doesn't teminate, a pathway cannot be found!"
  let pathways = findPathways enzymes ala dha
  putStrLn "Biosynthetic pathway/s found: "
  mapM_ putStrLn $ intercalate " -> " . fmap show <$> pathways
