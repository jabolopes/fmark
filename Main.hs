module Main where

import Prelude hiding (lex)

import Data.Char
import Data.List
import System.IO.Error


data Document = Title String
              | Paragraph String
              | BeginSection
              | EndSection
              | Section


indentation :: String -> Int
indentation ln =
    length $ takeWhile isSpace ln


join :: String -> String -> String
join ln1 ln2 = (dropWhileEnd isSpace ln1) ++ " " ++ (dropWhile isSpace ln2)


trim :: String -> String
trim ln = dropWhileEnd isSpace (dropWhile isSpace ln)


section :: Int -> Int -> String -> [String]
section idn1 idn2 ln =
    case compare idn1 idn2 of
      EQ -> [ln]
      LT -> ["begin", ln]
      GT -> ["end", ln]


reduce :: Int -> String -> [String]
reduce idn ln =
    let t = if last (dropWhileEnd isSpace ln) == '.' then "paragraph" else "title" in
    section idn (indentation ln) $ t ++ " = " ++ (trim ln)


classify :: Int -> [String] -> [String]
classify _ [] = []
classify idn [ln] | all isSpace ln = []
classify idn [ln] = reduce idn ln
classify idn (ln1:lns) | all isSpace ln1 = classify idn lns

classify idn (ln1:ln2:lns) =
    if all isSpace ln2 || indentation ln1 /= indentation ln2 then
        classify idn [ln1] ++ classify (indentation ln1) (ln2:lns)
    else
        classify idn (join ln1 ln2:lns)


main =
    do contents <- getContents
       let lns = lines contents
       putStrLn $ show lns
       mapM_ putStrLn $ classify 0 lns