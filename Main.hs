module Main where

import Prelude hiding (lex)

import Control.Monad.State hiding (join)

import Data.Char
import Data.Functor
import Data.List

import System.Console.GetOpt
import System.Environment
import System.IO.Error

import Debug.Trace


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


section :: [Int] -> Int -> [Token]
section [] _ = error "section: idns is empty"
section (idn1:idns) idn2 =
    case compare idn1 idn2 of
      EQ -> []
      LT -> [BeginSection]
      GT -> EndSection:section (dropWhile (> idn1) idns) idn2


reduce :: [Int] -> String -> [Token]
reduce idns ln = section idns (indentation ln) ++ [Text $ trim ln]


push :: Eq a => a -> [a] -> [a]
push x (y:ys) | x /= y = x:y:ys
push x xs = xs


classify :: [Int] -> [String] -> [Token]
classify idns _ | trace ("idns = " ++ show idns) False = undefined
classify _ [] = []
classify _ [ln] | all isSpace ln = []
classify idns [ln] = reduce idns ln
classify idns (ln1:lns) | all isSpace ln1 = classify idns lns

classify idns (ln1:ln2:lns) =
    if all isSpace ln2 || indentation ln1 /= indentation ln2 then
        reduce idns ln1 ++ classify (push (indentation ln1) idns) (ln2:lns)
    else
        classify idns (join ln1 ln2:lns)



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

          loop tokens st =
              error $ "\n\n\tloop: unhandled case" ++
                      "\n\n\t tokens = " ++ show tokens ++
                      "\n\n\t st = " ++ show st ++ "\n\n"


data XmlState = XmlState Int
type XmlM a = State XmlState a


getIdn :: XmlM Int
getIdn =
    do XmlState idn <- get
       return idn


withIdn :: XmlM a -> XmlM a
withIdn m =
    do idn <- getIdn
       put $ XmlState $ idn + 2
       val <- m
       put $ XmlState idn
       return val


docToXml :: Document -> String
docToXml doc =
    intercalate "\n" ["<xml>",
                      str,
                      "</xml>"]
    where str = evalState (loop doc) (XmlState 2)

          xmlIndent lvl str = replicate lvl ' ' ++ str

          xmlShortTag tag str =
              do idn <- getIdn
                 return $ xmlIndent idn "<" ++ tag ++ ">" ++ str ++ "</" ++ tag ++ ">"

          xmlLongTag tag m =
              do idn <- getIdn
                 str <- withIdn m
                 return $
                   (xmlIndent idn "<" ++ tag ++ ">\n") ++
                   str ++ "\n" ++
                   (xmlIndent idn "</" ++ tag ++ ">")

          loop (Heading str) = xmlShortTag "heading" str
          loop (Paragraph str) = xmlShortTag "paragraph" str
          loop (Content docs) = xmlLongTag "content" $ intercalate "\n" <$> mapM loop docs 
          loop (Section doc) = xmlLongTag "section" $ loop doc


docToLatex :: Document -> String
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


process :: Flag -> IO ()
process format =
    do contents <- getContents
       let lns = lines contents
       let doc = classify [0] lns
       let fn = case format of
                  OutputDoc -> putStrLn . show . docify
                  OutputLatex -> putStrLn . docToLatex . docify
                  OutputTokens -> mapM_ (putStrLn . show)
                  OutputXml -> putStrLn . docToXml . docify
       fn doc


data Flag = OutputDoc
          | OutputLatex
          | OutputTokens
          | OutputXml
            deriving (Eq, Show)

opts = [Option ['d'] ["doc"] (NoArg OutputDoc) "Output doc",
        Option ['l'] ["latex"] (NoArg OutputLatex) "Output latex",
        Option ['t'] ["token"] (NoArg OutputTokens) "Output tokens",
        Option ['x'] ["xml"] (NoArg OutputXml) "Output xml"]

main =
    do args <- getArgs
       case getOpt Permute opts args of
         ([], _, []) -> process OutputDoc
         (opts, nonOpts, []) -> process $ last opts
         (_, _, errs) -> do progName <- getProgName
                            ioError (userError (concat errs ++ usageInfo (header progName) opts))
    where header progName = "Usage: " ++ progName ++ " [OPTION...] files..."