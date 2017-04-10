module Main where

import Lib
import Data.List (intercalate)
import Data.Monoid ((<>))


main :: IO ()
main = do
  let pathways = findPathways enzymes dha ala
  if null pathways
    then putStrLn $ "A biochemical pathway between " <>
                    show ala <> " and " <> show dha <> " could not be found"
    else do
      putStrLn "Biosynthetic pathway/s found: "
      mapM_ putStrLn $ intercalate " -> " . fmap show <$> pathways
