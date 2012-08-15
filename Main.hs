module Main where

import Prelude hiding (lex)

import Data.Char
import Data.List
import System.IO.Error


data Token = Title String
           | Paragraph String
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
    let t = if isPunctuation $ last (dropWhileEnd isSpace ln) then Paragraph else Title in
    maybe [t $ trim ln] (\s -> [s, t $ trim ln]) $ section idn (indentation ln)


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



data Document = DToken Token
              | Content [Document]
              | Section Document

instance Show Document where
    show (DToken token) = show token
    show (Content docs) = intercalate "\n" $ map show docs
    show (Section doc) = "begin\n" ++ show doc ++ "\nend"


isContent (Title _) = True
isContent (Paragraph _) = True
isContent _ = False


docify :: [Token] -> Document
docify tokens =
    restructure tokens [[]]
    where restructure :: [Token] -> [[Document]] -> Document
          restructure [] [doc] = Content $ reverse doc

          restructure [] st = restructure [EndSection] st

          restructure (token:tokens) (top:st) | isContent token =
              restructure tokens ((DToken token:top):st)

          restructure (BeginSection:tokens) st =
              restructure tokens ([]:st)

          restructure (EndSection:tokens) (top:bot:st) =
              restructure tokens (((Section $ Content $ reverse top):bot):st)

          restructure tokens st =
              error $ "\n\n\trestructure: unhandled case" ++
                      "\n\n\t tokens = " ++ show tokens ++
                      "\n\n\t st = " ++ show st ++ "\n\n"
              

main =
    do contents <- getContents
       let lns = lines contents
       -- mapM_ (putStrLn . show) $ classify 0 lns
       putStrLn $ show $ docify $ classify 0 lns