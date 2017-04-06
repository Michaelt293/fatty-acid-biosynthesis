module Lib where

import Control.Monad.Writer.Lazy
import Data.List (intercalate)
import Data.Monoid ((<>))


data FA = FA Int [Int] deriving (Eq, Ord)

instance Show FA where
  show (FA cs dbs) =
    let doubleBonds = intercalate "Z," (show <$> dbs)
    in doubleBonds <> "-" <> show cs <> ":" <> show (length dbs)

ala = FA 18 [9, 12, 15]

dha = FA 22 [4, 7, 10, 13, 16, 19]

elongase :: FA -> FA
elongase(FA cs dbs) = FA (cs + 2) (fmap (+2) dbs)

betaOxidation :: FA -> FA
betaOxidation(FA cs dbs) = if cs > 2
                             then FA (cs - 2) (fmap (\n -> n - 2) dbs)
                             else FA cs dbs

delta5 :: FA -> FA
delta5(FA cs dbs) = if 5 `notElem` dbs && cs > 5
                      then FA cs (5:dbs)
                      else FA cs dbs

delta6 :: FA -> FA
delta6(FA cs dbs) = if 6 `notElem` dbs && cs > 6
                      then FA cs (6:dbs)
                      else FA cs dbs

enzymes = [elongase, betaOxidation, delta5, delta6]

convertToWriter :: (FA -> FA) -> FA -> Writer [FA] FA
convertToWriter f fa = do
                         tell [fa]
                         return (f fa)

findPathways :: [FA -> FA] -> FA -> FA -> [[FA]]
findPathways enzymes precursor product'=
  let pathways = runWriter <$> loop enzymes [return precursor] product'
  in filter (\pathway -> product' `elem` pathway) $
       fmap (\fas -> snd fas <> [fst fas]) pathways
  where
    explorePathways :: [FA -> FA] -> [Writer [FA] FA] -> [Writer [FA] FA]
    explorePathways enzymes' wFAs  = do
      enzyme <- enzymes'
      wFA <- wFAs
      return $ convertToWriter enzyme =<< wFA
    loop :: [FA -> FA] -> [Writer [FA] FA] -> FA -> [Writer [FA] FA]
    loop enzymes' wFAs product'' =
      let products = explorePathways enzymes' wFAs
      in
        if product'' `elem` fmap (fst . runWriter) products
          then products
          else loop enzymes' products product''
