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

upperLetter = R.satisfy isUpper

manyTill1 r end = do
    s <- r
    ss <- R.manyTill r end
    return (s:ss)

-- | Reads in 
entityName letterCase = do
    s <- letterCase
    name <- R.many' (R.try letterCase <|> R.digit)
    dot <- R.option " " (R.try (R.string ".") <|> R.try (R.string ", INC.") <|> R.string ",")
    R.option ' ' R.space
    return (T.pack (s:name) <> T.fromStrict dot)


-- | Parses an opinion name from the table of contents
--   with the appelant entity given before the
    --   defendant and seperated by a "v.".
--   eg. Wright v. Nix . . . . . . . . . . . . . 838
appelantDefendant = do
    appelant <- R.manyTill (entityName R.letter) (R.string "v. ")
    defendant <- R.manyTill (entityName R.letter) (R.string ". ")
    return $ (T.concat appelant, T.concat defendant)

-- | Similar to appelantDefendant except the entities
--   the entities are given in reverse and seperated
--   by a ';'. The result is given in the same order
--   as appelantDefendant.
--
--   eg. Yelder; Alabama v.  . . . . . . . . . . 898
defendantAppelant = do
    defendant <- R.manyTill (entityName R.letter) (R.string "; ")
    appelant <- R.manyTill (entityName R.letter) (R.string "v. ")
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
parseTOC = first (nub . toc) . span (not . opinionsStart) . drop 1 . dropWhile (not . opinionsStart)
    where toc = fmap fst . sortOn snd . rights
              . fmap (R.eitherResult . R.parse tableOfContents)
              
parse :: R.Parser a -> T.Text -> Bool
parse r = isJust . R.maybeResult . R.parse r

opinionTitle :: T.Text -> Bool
opinionTitle = parse $ do
    manyTill1 (entityName upperLetter) (R.string "v. ")
    R.many1 (entityName upperLetter)
    R.option "" (R.option "" (R.string ", ") >> R.string "et al.")
    R.many' R.space
    R.endOfInput

-- A line reading "It is so ordered."
endOfCase = parse $ R.try (R.string "It is so ordered.") <|> R.string "Affirmed." >> R.many' R.anyChar

opinionHeading caseName = parse $  R.many1 R.digit >>  R.space >>  R.string caseName

joiningJustices = do
        R.string "with whom Justice "
        R.many1 R.letter
        R.space
        R.option ' ' $ do
            R.string "and Justice"
            R.many1 R.letter
            R.space
        R.try (R.string "joins ") <|> R.string "join "

additionalOpinion opinionType = do
    R.string "Justice "
    lastName <- R.many1 R.letter
    R.string ","
    R.space
    R.option " " $ joiningJustices
    R.string opinionType
    return lastName

-- | Parses a dissent begining
--   eg. Scalia, J., dissenting
--       Justice Stevens, with whom Justice Blackmun and Justice O'Connor join, dissenting.
dissent = additionalOpinion "dissenting."

opinion :: T.Text -> [[T.Text]] -> ST.State [T.Text] [[T.Text]]
opinion opinionName cases = do
    ST.modify (dropWhile (not . opinionTitle)) -- (opinionName `T.isPrefixOf`) . formatOpinionName))
    (c, file) <- span (not . endOfCase) <$> ST.get
    ST.put file
    return $ c:cases

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
    --mapM_ print . take 5 $ toc
    -- Take the toc and fold it using the cases
    --print $ take 10 cases
    print $ take 5 toc 
    let f = foldr1 (>=>) $ fmap opinion $ take 10 toc
    let g [] = ""
        g (h:t) = h
    mapM_ (TIO.putStrLn . T.toStrict . g) $ ST.evalState (f []) cases
      
    
    
    
    


