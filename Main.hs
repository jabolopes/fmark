module Main where

import Prelude hiding (lex)

import Data.Char
import Data.List
import System.IO.Error


data Token = Text String
           | BeginSection
           | EndSection
             deriving (Show)


indentation :: String -> Int
indentation ln =
    length $ takeWhile isSpace ln


join :: String -> String -> String
join ln1 ln2 = (dropWhileEnd isSpace ln1) ++ " " ++ (dropWhile isSpace ln2)


trim :: String -> String
trim ln = dropWhileEnd isSpace (dropWhile isSpace ln)


section :: Int -> Int -> Maybe Token
section idn1 idn2 =
    case compare idn1 idn2 of
      EQ -> Nothing
      LT -> Just BeginSection
      GT -> Just EndSection


reduce :: Int -> String -> [Token]
reduce idn ln =
    maybe [Text $ trim ln] (\s -> [s, Text $ trim ln]) $ section idn (indentation ln)


classify :: Int -> [String] -> [Token]
classify _ [] = []
classify idn [ln] | all isSpace ln = []
classify idn [ln] = reduce idn ln
classify idn (ln1:lns) | all isSpace ln1 = classify idn lns

classify idn (ln1:ln2:lns) =
    if all isSpace ln2 || indentation ln1 /= indentation ln2 then
        classify idn [ln1] ++ classify (indentation ln1) (ln2:lns)
    else
        classify idn (join ln1 ln2:lns)



data Document = Heading String
              | Paragraph String
              | Content [Document]
              | Section Document

instance Show Document where
    show (Heading str) = "Heading = " ++ str
    show (Paragraph str) = "Paragraph = " ++ str
    show (Content docs) = intercalate "\n" $ map show docs
    show (Section doc) = "begin\n" ++ show doc ++ "\nend"


docify :: [Token] -> Document
docify tokens =
    loop tokens [[]]
    where loop :: [Token] -> [[Document]] -> Document
          loop [] [doc] = Content $ reverse doc

          loop [] st = loop [EndSection] st

          loop ((Text str):tokens) (top:st) =
              let cons = if isPunctuation $ last str then Paragraph else Heading in
              loop tokens ((cons str:top):st)

          loop (BeginSection:tokens) st =
              loop tokens ([]:st)

          loop (EndSection:tokens) (top:bot:st) =
              loop tokens (((Section $ Content $ reverse top):bot):st)


main =
    do contents <- getContents
       let lns = lines contents
       putStrLn $ show $ docify $ classify 0 lns