module Main where

import Prelude hiding (lex)

import Data.Char
import Data.List

import System.Console.GetOpt
import System.Environment
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


docToLatex doc =
    intercalate "\n\n" ["\\documentclass[a4paper]{article}",
                        "\\begin{document}",
                        loop 0 doc,
                        "\\end{document}"]
    where ltStr str =
              concatMap (\c -> if c == '#' then "\\#" else [c]) str

          ltBold str =
              "\\textbf{" ++ ltStr str ++ "}"

          ltSection lvl str
              | lvl < 3 = "\\" ++ (concat (replicate lvl "sub")) ++ "section{" ++ ltStr str ++ "}"
              | lvl == 3 = "\\paragraph{" ++ ltStr str ++ "}"
              | lvl == 4 = "\\subparagraph{" ++ ltStr str ++ "}"

          ltParagraph str = ltStr str

          loop lvl (Heading str) = ltSection lvl str
          loop lvl (Paragraph str) = ltParagraph str
          loop lvl (Content docs) = intercalate "\n\n" $ map (loop lvl) docs
          loop lvl (Section doc) = loop (lvl + 1) doc

          -- loop lvl (Section (Heading str)) =
          --     ltSection lvl str ++ "\n\n" ++ loop (lvl + 1) doc

          -- loop lvl (Section (Content ((Heading str):docs))) =
          --     ltSection lvl str ++ "\n\n" ++ loop (lvl + 1) (Content docs)

          -- loop lvl (Section doc) =
          --     ltSection lvl "" ++ "\n\n" ++ loop (lvl + 1) doc
    

process isDoc =
    do contents <- getContents
       let lns = lines contents
       if isDoc then
           putStrLn $ show $ docify $ classify 0 lns
       else
           putStrLn $ docToLatex $ docify $ classify 0 lns


data Flag = OutputDoc
          | OutputLatex
            deriving (Eq, Show)

opts = [Option ['d'] ["doc"] (NoArg OutputDoc) "Output doc",
        Option ['l'] ["latex"] (NoArg OutputLatex) "Output latex"]

main =
    do args <- getArgs
       case getOpt Permute opts args of
         (opts, nonOpts, []) -> if OutputLatex `elem` opts then
                                    process False
                                else
                                    process True
         (_, _, errs) -> do progName <- getProgName
                            ioError (userError (concat errs ++ usageInfo (header progName) opts))
    where header progName = "Usage: " ++ progName ++ " [OPTION...] files..."