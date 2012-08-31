module Main where

import Prelude hiding (lex)

import Control.Monad.State hiding (join)

import Data.Char
import Data.Functor
import Data.List

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import System.Unix.Directory


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
classify _ [] = []
classify _ [ln] | all isSpace ln = []
classify idns (ln1:lns) | all isSpace ln1 = classify idns lns
classify idns [ln] = reduce idns ln

classify idns (ln1:ln2:lns)
    | all isSpace ln2 = reduce idns ln1 ++ classify (push idn1 (dropWhile (> idn1) idns)) lns
    | idn1 < idn2 = reduce idns ln1 ++ classify (push idn1 (dropWhile (> idn1) idns)) (ln2:lns)
    | idn1 > idn2 = reduce idns ln1 ++ classify (push idn1 idns) (ln2:lns)
    | otherwise = classify idns (join ln1 ln2:lns)
    where idn1 = indentation ln1
          idn2 = indentation ln2



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


--pdflatex :: String -> IO String
pdflatex outFp contents =
    do withTemporaryDirectory "fmark" $
         \outDir -> withFile "/dev/null" WriteMode $
                    \hNull -> do (Just hIn, _, _, h) <- createProcess
                                                        (proc "pdflatex" ["-output-directory=" ++ outDir,
                                                                          "--jobname=" ++ outFname])
                                                        { std_in = CreatePipe,
                                                          std_out = UseHandle hNull,
                                                          std_err = UseHandle hNull }
                                 mapM_ (hPutStrLn hIn) $ filterLines contents
                                 hClose hIn
                                 waitForProcess h
                                 copyFile (pdfFp outDir) (addExtension outFp "pdf")
    where outFname = "texput"
          pdfFp fp = combine fp $ addExtension outFname "pdf"
          

filterLines :: String -> [String]
filterLines str =
    [ ln | ln <- lines str, (trim ln) /= "" ]


fmark :: Flag -> String -> IO String
fmark fmt contents =
    formatFn $ classify [0] $ lines contents
    where formatFn =
              case fmt of
                OutputDoc -> return . show . docify
                OutputLatex -> return . docToLatex . docify
                OutputPdf -> error "cannot use stdin with PDF output format"
                OutputTokens -> return . intercalate "\n" . map show
                OutputXml -> return . docToXml . docify


fmarkFp :: Flag -> String -> String -> IO ()
fmarkFp OutputPdf contents fp =
    formatFn $ classify [0] $ lines contents
    where formatFn = (pdflatex fp) . docToLatex . docify


fmarkH :: Flag -> Handle -> Either Handle String -> IO ()
fmarkH fmt hIn (Left hOut) =
    do contents <- hGetContents hIn
       fmark fmt contents >>= hPutStrLn hOut

fmarkH fmt hIn (Right fp) =
    do contents <- hGetContents hIn
       fmarkFp fmt contents fp


data Flag = OutputDoc
          | OutputLatex
          | OutputPdf
          | OutputTokens
          | OutputXml
            deriving (Eq, Show)

opts = [Option ['d'] ["doc"] (NoArg OutputDoc) "Output doc",
        Option ['l'] ["latex"] (NoArg OutputLatex) "Output latex",
        Option ['p'] ["pdf"] (NoArg OutputPdf) "Output PDF",
        Option ['t'] ["token"] (NoArg OutputTokens) "Output tokens",
        Option ['x'] ["xml"] (NoArg OutputXml) "Output xml"]

main =
    do args <- getArgs
       case getOpt Permute opts args of
         (opts, nonOpts, []) -> processOpts opts nonOpts
         (_, _, errs) -> do progName <- getProgName
                            ioError (userError (concat errs ++ usageInfo (header progName) opts))
    where header progName = "Usage: " ++ progName ++ " [OPTION...] files..."
          processOpts opts nonOpts =
              let fmt = if null opts then OutputDoc else last opts in
              let hInFn = case nonOpts of
                            [] -> \fn -> fn stdin
                            _ -> withFile (last nonOpts) ReadMode in
              let eOut = case nonOpts of
                          [] -> Left stdout
                          _ | fmt /= OutputPdf -> Left stdout
                          _ -> Right $ last nonOpts in
              hInFn $ \hIn -> fmarkH fmt hIn eOut