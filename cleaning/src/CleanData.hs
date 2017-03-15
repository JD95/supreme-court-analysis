{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module CleanData where

import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text.Lazy as R
import qualified Control.Monad.State.Lazy as ST
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Monoid

entityName = do
    name <- R.many1 (R.try R.letter <|> R.digit)
    dot <- R.option " " (R.string ".")
    R.option ' ' R.space
    return (T.pack name <> T.fromStrict dot)

usualCaseName = do
    appelant <- R.manyTill entityName (R.string "v. ")
    defendant <- R.manyTill entityName (R.string ". ")
    R.sepBy1 (R.char '.') R.space
    R.space
    R.many1 R.digit
    return $ T.concat [T.concat appelant,"v.", T.concat defendant]

semiColonCaseName = do
    defendant <- R.manyTill entityName (R.string "; ")
    appelant <- R.manyTill entityName (R.string "v. ")
    R.sepBy1 (R.char '.') R.space
    R.space
    R.many1 R.digit
    return $ T.concat [T.concat appelant, "v. ", T.concat defendant]

tableOfContents :: R.Parser T.Text
tableOfContents = R.try usualCaseName <|> semiColonCaseName

casesStart :: T.Text -> Bool
casesStart = (==) "CASES ADJUDGED"

parseTOC :: [T.Text] ->  ([T.Text], [T.Text])
parseTOC = first (rights . fmap (R.eitherResult . R.parse tableOfContents))
         . span (not . casesStart)
         . drop 1 . dropWhile (not . casesStart)

parse :: R.Parser a -> T.Text -> Bool
parse r = isJust . R.maybeResult . R.parse r

caseTitle :: T.Text -> Bool
caseTitle = parse $ do
    entityName
    R.string "v."
    R.try R.space
    entityName

endOfCase = parse $ R.string "It is so ordered." >> R.many' R.anyChar

caseHeading caseName = parse $  R.many1 R.digit >>  R.space >>  R.string caseName

nextCase :: ST.State [T.Text] [T.Text]
nextCase = do
    ST.modify (dropWhile (not . caseTitle))
    (c, file) <- span (not . endOfCase) <$> ST.get
    ST.put file
    return $ c

removeCaseHeaders :: [T.Text] -> [T.Text]
removeCaseHeaders [] = []
removeCaseHeaders (c:cs) = c : filter (not . caseHeading (T.toStrict c)) cs


cleanCase :: [T.Text] -> [T.Text]
cleanCase = removeCaseHeaders
          . filter (""/=)

cleanData :: IO()
cleanData = do
    let filePath = "../data/502.txt"
    print $ "Cleaning " ++ filePath
    (toc, cases) <- parseTOC . T.lines . T.fromStrict <$> TIO.readFile filePath
    mapM_ print . take 5 $ toc
    mapM_ (TIO.putStrLn . T.toStrict) $ cleanCase $ ST.evalState nextCase cases
      
    
    
    
    


