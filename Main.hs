{-# LANGUAGE ParallelListComp #-}
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
import System.Unix.Directory hiding (find)


-- | The lines of a 'String' without empty lines.
filterLines :: String -> [String]
filterLines str = [ ln | ln <- lines str, trim ln /= "" ]


flatten str =
    map loop str
    where loop '\n' = ','
          loop c = c


-- | The number of space characters at the begining of a 'String'.
indentation :: String -> Int
indentation ln = length $ takeWhile isSpace ln


-- | A 'String' that results from joining two 'String's with a single
-- space in between.
join :: String -> String -> String
join ln1 ln2 = (dropWhileEnd isSpace ln1) ++ "\n" ++ (dropWhile isSpace ln2)


prefix :: String -> String -> String
prefix pre str =
    pre ++ prefix' str
    where prefix' str =
              concatMap (\c -> case c of
                                 '\n' -> '\n':pre
                                 _ -> [c]) str


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
    = Heading (Maybe String) String
    -- | A 'Paragraph' in a 'Document'.
    | Paragraph (Maybe String) String
    -- | A sequence of 'Document' elements.
    | Content (Maybe String) [Document]
    -- | A subsection in a 'Document'.
    | Section (Maybe String) Document

instance Show Document where
    show (Heading Nothing str) = "Heading = " ++ str
    show (Heading (Just sty) str) = "Heading(" ++ flatten sty ++ ") = " ++ str
    show (Paragraph Nothing str) = "Paragraph = " ++ str
    show (Paragraph (Just sty) str) = "Paragraph(" ++ sty ++ ") = " ++ str
    show (Content _ docs) = intercalate "\n" $ map show docs
    show (Section _ doc) = "begin\n" ++ show doc ++ "\nend"


-- | Parses a sequence of 'Token's into a 'Document'.
docify :: [Token] -> Document
docify tokens =
    loop tokens [[]]
    where loop :: [Token] -> [[Document]] -> Document
          loop [] [doc] = Content Nothing $ reverse doc
          loop [] st = loop [EndSection] st

          loop ((Text str):tokens) (top:st) =
              let cons = if isPunctuation $ last str then Paragraph else Heading in
              loop tokens ((cons Nothing str:top):st)

          loop (BeginSection:tokens) st =
              loop tokens ([]:st)

          loop (EndSection:tokens) (top:bot:st) =
              loop tokens (((Section Nothing $ Content Nothing $ reverse top):bot):st)

          loop tokens st =
              error $ "\n\n\tloop: unhandled case" ++
                      "\n\n\t tokens = " ++ show tokens ++
                      "\n\n\t st = " ++ show st ++ "\n\n"


weaveStyle :: Document -> Document -> (Document, [String])
weaveStyle doc style =
    loop doc style
    where msg title str desc sty =
              "In " ++ title ++ "\n"
                    ++ prefix "  " str ++ "\n"
                    ++ desc ++ "\n"
                    ++ prefix "  " sty
          
          loop doc@(Heading _ str) (Heading _ sty) =
              let errs = case length (lines str) == length (lines sty) of
                           True -> []
                           _ -> [msg "Heading" str "does not match style" sty] in
              (Heading (Just sty) str, errs)

          loop (Paragraph _ str) (Paragraph _ sty) =
              -- currently, paragraph newlines are striped before getting here...
              let errs | length (lines sty) == 1 = []
                       | otherwise = [msg "Paragraph" str "paragraph styles must be one line" sty] in
              (Paragraph (Just (init sty)) str, [])

          loop (Content _ docs1) (Content _ docs2) =
              let (docsSty, docsNoSty) = splitAt (length docs2) docs1 in
              let (docsSty', errss) = unzip [ loop doc1 doc2 | doc1 <- docsSty | doc2 <- docs2 ] in
              (Content Nothing (docsSty' ++ docsNoSty), concat errss)

          loop (Section _ doc1) (Section _ doc2) =
              let (doc', errs) = loop doc1 doc2 in
              (Section Nothing doc', errs)

          loop doc _ = (doc, [show doc])
              

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

          loop (Heading _ str) = xmlShortTag "heading" str
          loop (Paragraph _ str) = xmlShortTag "paragraph" str
          loop (Content _ docs) = xmlLongTag "content" $ intercalate "\n" <$> mapM loop docs 
          loop (Section _ doc) = xmlLongTag "section" $ loop doc


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

          loop lvl (Heading _ str) = ltSection lvl str
          loop lvl (Paragraph _ str) = ltParagraph str
          loop lvl (Content _ docs) = intercalate "\n\n" $ map (loop lvl) docs
          loop lvl (Section _ doc) = loop (lvl + 1) doc


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
    | Style String
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

fmarkStyle fmt hIn hOut styleFp =
    withFile styleFp ReadMode $
      \hStyle -> do style <- hGetContents hStyle
                    let styleDoc = docify $ classify style
                    
                    contents <- hGetContents hIn
                    let doc = docify $ classify contents

                    let (doc', errs) = weaveStyle doc styleDoc
                    putStrLn $ show doc'
                    case errs of
                      [] -> return ()
                      _ -> do hPutStrLn stderr "Style warnings:"
                              mapM_ (hPutStrLn stderr) errs


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
           Option ['s'] ["style"] (ReqArg Style "style-name") "Style",
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
              case mstyle of
                Nothing -> hInFn $ \hIn -> fmarkH fmt hIn eOut
                Just (Style style) -> hInFn $ \hIn -> fmarkStyle fmt hIn eOut style
              where fmt = case opts of
                            [] -> OutputDoc
                            _ -> last opts

                    mstyle = find (\x -> case x of
                                           Style _ -> True
                                           _ -> False) $ reverse opts

                    hInFn = case nonOpts of
                              [] -> \fn -> fn stdin
                              _ -> withFile (last nonOpts) ReadMode

                    eOut = case nonOpts of
                             [] -> Left stdout
                             _ | fmt /= OutputPdf -> Left stdout
                             _ -> Right $ last nonOpts