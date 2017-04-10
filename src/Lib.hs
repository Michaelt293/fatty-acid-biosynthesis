module Lib where

import Control.Monad.Writer.Lazy
import Data.List (intercalate)
import Data.Monoid ((<>))
import Control.Monad (guard)


data FA = FA Int [Int] deriving (Eq, Ord)

instance Show FA where
  show (FA cs dbs) =
    let doubleBonds = intercalate "," ((<> "Z") . show <$> dbs)
    in
      if null doubleBonds
        then show cs <> ":" <> show (length dbs)
        else doubleBonds <> "-" <> show cs <> ":" <> show (length dbs)

ala :: FA
ala = FA 18 [9, 12, 15]

dha :: FA
dha = FA 22 [4, 7, 10, 13, 16, 19]

elongase :: FA -> FA
elongase(FA cs dbs) = FA (cs + 2) (fmap (+2) dbs)

betaOxidation :: FA -> FA
betaOxidation fa =
  case fa of
    unreactedFA@(FA cs dbs) ->
      if cs > 2
        then FA (cs - 2) (fmap (\n -> n - 2) dbs)
        else unreactedFA

delta5Desaturase :: FA -> FA
delta5Desaturase fa =
  case fa of
    unreactedFA@(FA cs dbs) ->
      if all (> 5) dbs && cs > 6
        then FA cs (5:dbs)
        else unreactedFA

delta6Desaturase :: FA -> FA
delta6Desaturase fa =
  case fa of
    unreactedFA@(FA cs dbs) ->
      if all (> 6) dbs && cs > 7
        then FA cs (6:dbs)
        else unreactedFA

enzymes :: [FA -> FA]
enzymes = [elongase, betaOxidation, delta5Desaturase, delta6Desaturase]

convertToWriter :: (FA -> FA) -> FA -> WriterT [FA] [] FA
convertToWriter f fa = do
    tell [fa]
    return (f fa)

findPathways :: [FA -> FA] -> FA -> FA -> [[FA]]
findPathways enzymes product' precursor =
  let pathways = runWriterT $ loop enzymes (return precursor) product' 10
  in filter (\pathway -> product' `elem` pathway) $
       fmap (\fas -> snd fas <> [fst fas]) pathways
  where
    explorePathways :: [FA -> FA] -> WriterT [FA] [] FA -> WriterT [FA] [] FA
    explorePathways enzymes' wFAs = do
      enzyme <- lift enzymes'
      wFA <- wFAs
      let result = convertToWriter enzyme wFA
      let [(fa, faList)] = runWriterT result
      guard $ fa `notElem` faList
      result
    loop :: [FA -> FA] -> WriterT [FA] [] FA -> FA -> Int -> WriterT [FA] [] FA
    loop enzymes' wFAs product'' count =
      let products = explorePathways enzymes' wFAs
      in
        if product'' `elem` fmap fst (runWriterT products)
          then products
          else if count == 0
                 then WriterT []
                 else loop enzymes' products product'' (count - 1)
