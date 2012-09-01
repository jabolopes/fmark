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


-- | The lines of a 'String' without empty lines.
filterLines :: String -> [String]
filterLines str = [ ln | ln <- lines str, trim ln /= "" ]


-- | The number of space characters at the begining of a 'String'.
indentation :: String -> Int
indentation ln = length $ takeWhile isSpace ln


-- | A 'String' that results from joining two 'String's with a single
-- space in between.
join :: String -> String -> String
join ln1 ln2 = (dropWhileEnd isSpace ln1) ++ "\n" ++ (dropWhile isSpace ln2)


-- | The 'List' with an additional element only if that element is not
-- equal to the first element in the original 'List'.
push :: Eq a => a -> [a] -> [a]
push x (y:ys) | x /= y = x:y:ys
push _ xs = xs


-- | A 'String' without leading and trailing space characters.
trim :: String -> String
trim str = dropWhileEnd isSpace (dropWhile isSpace str)


data Token
    -- | The 'Text' token corresponds to successive lines of text
    -- joined together.
    = Text String
    -- | The beginning of a new section corresponds to an increase in
    -- indentation or an unmatched decrease in indentation.
    | BeginSection
    -- | The end of a section corresponds to a decrease in
    -- indentation.
    | EndSection
      deriving (Show)


-- | A 'List' of 'BeginSection' and 'EndSection' 'Token's issued
-- according the indentation stack and current indentation.
section :: [Int] -> Int -> [Token]
section [] _ = error "section: idns is empty"
section (idn1:idns) idn2 =
    case compare idn1 idn2 of
      EQ -> []
      LT -> [BeginSection]
      GT -> EndSection:section (dropWhile (> idn1) idns) idn2


-- | A 'List' of 'Text' 'Token's preceeded by the appropriate
-- 'BeginSection' or 'EndSection' 'Token's.
reduce :: [Int] -> String -> [Token]
reduce idns ln = section idns (indentation ln) ++ [Text $ trim ln]


-- | Tokenizes a string.
classify :: String -> [Token]
classify lns =
    classify' [0] $ lines lns
        where classify' _ [] = []
              classify' _ [ln] | all isSpace ln = []
              classify' idns (ln1:lns) | all isSpace ln1 = classify' idns lns
              classify' idns [ln] = reduce idns ln
              classify' idns (ln1:ln2:lns)
                  | all isSpace ln2 = reduce idns ln1 ++ classify' (push idn1 (dropWhile (> idn1) idns)) lns
                  | idn1 < idn2 = reduce idns ln1 ++ classify' (push idn1 (dropWhile (> idn1) idns)) (ln2:lns)
                  | idn1 > idn2 = reduce idns ln1 ++ classify' (push idn1 idns) (ln2:lns)
                  | otherwise = classify' idns (join ln1 ln2:lns)
                  where idn1 = indentation ln1
                        idn2 = indentation ln2


data Document
    -- | A 'Heading' in a 'Document'.
    = Heading String
    -- | A 'Paragraph' in a 'Document'.
    | Paragraph String
    -- | A sequence of 'Document' elements.
    | Content [Document]
    -- | A subsection in a 'Document'.
    | Section Document

instance Show Document where
    show (Heading str) = "Heading = " ++ str
    show (Paragraph str) = "Paragraph = " ++ str
    show (Content docs) = intercalate "\n" $ map show docs
    show (Section doc) = "begin\n" ++ show doc ++ "\nend"


-- | Parses a sequence of 'Token's into a 'Document'.
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
    intercalate "\n" ["<xml>", str, "</xml>"]
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
    where ltStr str newlines =
              concatMap sub str
              where sub '#' = "\\#"
                    sub '\n' | newlines = "\\\\"
                    sub c = [c]

          ltSection lvl str
              | lvl < 3 = "\\" ++ (concat (replicate lvl "sub")) ++ "section{" ++ ltStr str True ++ "}"
              | lvl == 3 = "\\paragraph{" ++ ltStr str True ++ "}"
              | lvl == 4 = "\\subparagraph{" ++ ltStr str True ++ "}"

          ltParagraph str = ltStr str False

          loop lvl (Heading str) = ltSection lvl str
          loop lvl (Paragraph str) = ltParagraph str
          loop lvl (Content docs) = intercalate "\n\n" $ map (loop lvl) docs
          loop lvl (Section doc) = loop (lvl + 1) doc


pdflatex :: FilePath -> String -> IO ()
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
                                 copyFile (pdfFp outDir) (addExtension (dropExtensions outFp) "pdf")
    where outFname = "texput"
          pdfFp fp = combine fp $ addExtension outFname "pdf"


-- | Command line flags that specify output format or display usage
-- information.
data Flag
    -- | Output to 'stdout' in 'Document' format.
    = OutputDoc
    -- | Output to 'stdout' in LaTeX format.
    | OutputLatex
    -- | Output to a PDF file using LaTeX format and 'pdflatex'.
    | OutputPdf
    -- | Output to 'stdout' the 'Token's of the input.
    | OutputTokens
    -- | Output to 'stdout' in XML format.
    | OutputXml
    -- | Display usage information.
    | Help
      deriving (Eq, Show)


-- | Run friendly markup with the specified output format and input
-- 'String'. The output is a 'String'.
fmark :: Flag -> String -> IO String
fmark fmt contents =
    formatFn $ classify contents
    where formatFn =
              case fmt of
                OutputDoc -> return . show . docify
                OutputLatex -> return . docToLatex . docify
                OutputPdf -> error "cannot use stdin with PDF output format"
                OutputTokens -> return . intercalate "\n" . map show
                OutputXml -> return . docToXml . docify


-- | Run friendly markup with the specified output format and input
-- 'String'. The output is written to the file specified by
-- 'FilePath'.
fmarkFp :: Flag -> String -> FilePath -> IO ()
fmarkFp OutputPdf contents fp =
    formatFn $ classify contents
    where formatFn = (pdflatex fp) . docToLatex . docify


-- | Run friendly markup with the specified output format and input
-- 'Handle'. The result is 'Either' written to an output 'Handle' or
-- to the file specified by 'FilePath'.
fmarkH :: Flag -> Handle -> Either Handle FilePath -> IO ()
fmarkH fmt hIn (Left hOut) =
    do contents <- hGetContents hIn
       fmark fmt contents >>= hPutStrLn hOut

fmarkH fmt hIn (Right fp) =
    do contents <- hGetContents hIn
       fmarkFp fmt contents fp


-- | Command line options.
options = [Option ['d'] ["doc"] (NoArg OutputDoc) "Output doc",
           Option ['l'] ["latex"] (NoArg OutputLatex) "Output latex",
           Option ['p'] ["pdf"] (NoArg OutputPdf) "Output PDF",
           Option ['t'] ["token"] (NoArg OutputTokens) "Output tokens",
           Option ['x'] ["xml"] (NoArg OutputXml) "Output xml",
           Option ['h'] ["help"] (NoArg Help) "Display help"]


-- | Main.
main =
    do args <- getArgs
       case getOpt Permute options args of
         (opts, nonOpts, []) -> processOpts opts nonOpts
         (_, _, errs) -> do putErrors errs
                            putUsage
    where header progName = "Usage: " ++ progName ++ " [OPTION...] files..."

          putUsage =
              do progName <- getProgName
                 hPutStr stderr $ usageInfo (header progName) options

          putErrors errs =
              hPutStrLn stderr $ intercalate ", " $ map (dropWhileEnd (== '\n')) errs
          
          processOpts opts _ | Help `elem` opts = putUsage
          processOpts opts nonOpts =
              hInFn $ \hIn -> fmarkH fmt hIn eOut
              where fmt = case opts of
                            [] -> OutputDoc
                            _ -> last opts
                    
                    hInFn = case nonOpts of
                              [] -> \fn -> fn stdin
                              _ -> withFile (last nonOpts) ReadMode

                    eOut = case nonOpts of
                             [] -> Left stdout
                             _ | fmt /= OutputPdf -> Left stdout
                             _ -> Right $ last nonOpts