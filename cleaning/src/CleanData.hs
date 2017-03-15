{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module CleanData where

import Data.Maybe
import Data.Either
import Data.Char
import Data.Monoid
import Data.List
import qualified Data.Vector as V
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text.Lazy as R

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Control.Monad.State.Lazy as ST

-- | Reads in 
entityName = do
    name <- R.many1 (R.try R.letter <|> R.digit)
    dot <- R.option " " (R.try (R.string ".") <|> R.string ",")
    R.option ' ' R.space
    return (T.pack name <> T.fromStrict dot)


-- | Parses an opinion name from the table of contents
--   with the appelant entity given before the
--   defendant and seperated by a "v.".
--   eg. Wright v. Nix . . . . . . . . . . . . . 838
appelantDefendant = do
    appelant <- R.manyTill entityName (R.string "v. ")
    defendant <- R.manyTill entityName (R.string ". ")
    return $ (T.concat appelant, T.concat defendant)

-- | Similar to appelantDefendant except the entities
--   the entities are given in reverse and seperated
--   by a ';'. The result is given in the same order
--   as appelantDefendant.
--
--   eg. Yelder; Alabama v.  . . . . . . . . . . 898
defendantAppelant = do
    defendant <- R.manyTill entityName (R.string "; ")
    appelant <- R.manyTill entityName (R.string "v. ")
    return (T.concat appelant, T.concat defendant)

-- | Parses an opinion name from the table of contents
--   also returning the page number the case appears
--   on. This page number is later used to sort the
--   cases into the order that the appear in the
--   document.
opinionName style = do
    (d, a) <- style
    R.sepBy1 (R.char '.') R.space
    R.space
    pageNum <- R.many1 R.digit
    return . first (formatOpinionName . T.concat)
           $ ([d, " v. ",a], read pageNum::Int)

formatOpinionName = T.filter (not . isSpace) . T.toUpper
    
tableOfContents :: R.Parser (T.Text, Int)
tableOfContents = R.try (opinionName defendantAppelant) <|> opinionName appelantDefendant

opinionsStart :: T.Text -> Bool
opinionsStart = (==) "CASES ADJUDGED"

parseTOC :: [T.Text] ->  ([T.Text], [T.Text])
parseTOC = first toc . span (not . opinionsStart) . drop 1 . dropWhile (not . opinionsStart)
    where toc = fmap fst . sortOn snd . rights
              . fmap (R.eitherResult . R.parse tableOfContents)
              
parse :: R.Parser a -> T.Text -> Bool
parse r = isJust . R.maybeResult . R.parse r

opinionTitle :: T.Text -> Bool
opinionTitle = parse $ do
    entityName
    R.string "v."
    R.try R.space
    entityName

endOfCase = parse $ R.string "It is so ordered." >> R.many' R.anyChar

opinionHeading caseName = parse $  R.many1 R.digit >>  R.space >>  R.string caseName

opinion :: ST.State [T.Text] [T.Text]
opinion = do
    ST.modify (dropWhile (not . opinionTitle))
    (c, file) <- span (not . endOfCase) <$> ST.get
    ST.put file
    return $ c

removeCaseHeaders :: [T.Text] -> [T.Text]
removeCaseHeaders [] = []
removeCaseHeaders (c:cs) = c : filter (not . opinionHeading (T.toStrict c)) cs


cleanOpinion :: T.Text -> [T.Text] -> [T.Text]
cleanOpinion opinionName
    = removeCaseHeaders
    . filter ("" /=)
    . filter ((opinionName `T.isSuffixOf`) . formatOpinionName)

cleanData :: IO()
cleanData = do
    let filePath = "../data/502.txt"
    print $ "Cleaning " ++ filePath
    (toc, cases) <- parseTOC . T.lines . T.fromStrict <$> TIO.readFile filePath
    mapM_ print . take 5 $ toc
    --mapM_ (TIO.putStrLn . T.toStrict) $ cleanCase $ ST.evalState opinion cases
      
    
    
    
    


