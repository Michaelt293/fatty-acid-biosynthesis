module Main where

import Lib
import Data.List (intercalate)

main :: IO ()
main = do
  let pathways = findPathways enzymes dha ala
  putStrLn "Biosynthetic pathway/s found: "
  mapM_ putStrLn $ intercalate " -> " . fmap show <$> pathways
