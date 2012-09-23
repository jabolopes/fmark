module Main where

import Data.List (intercalate)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.IO

import Fmark
import Utils


-- | 'Flag' represents the command line flags that specify output
-- format, display usage information, and 'Style' filename.
data Flag
    -- | Output to 'stdout' in 'Document' format.
    = OutputDoc
    -- | Output to 'stdout' in LaTeX format.
    | OutputLatex
    -- | Output to a PDF file using LaTeX format and 'pdflatex'.
    | OutputPdf
      
    | OutputToken

    -- | Output to 'stdout' in XML format.
    | OutputXml
    -- | Display usage information.
    | Help
    -- | Specify the filename of the style file.
    | StyleName String
      deriving (Eq)


formatOfFlag :: Flag -> Format
formatOfFlag OutputDoc = FormatDoc
formatOfFlag OutputLatex = FormatLatex
formatOfFlag OutputPdf = FormatPdf
formatOfFlag OutputToken = FormatToken
formatOfFlag OutputXml = FormatXml


-- | 'options' represents the command line options.
options = [Option "d" ["doc"] (NoArg OutputDoc) "Output doc",
           Option "l" ["latex"] (NoArg OutputLatex) "Output latex",
           Option "p" ["pdf"] (NoArg OutputPdf) "Output PDF",
           Option "s" ["style"] (ReqArg StyleName "style-name") "Style",
           Option "t" ["token"] (NoArg OutputToken) "Output tokens",
           Option "x" ["xml"] (NoArg OutputXml) "Output xml",
           Option [] ["help"] (NoArg Help) "Display help"]


-- | 'main'.
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
              styleFn $ \mstyle -> (inFn $ \hIn -> fmarkH fmt hIn eOut mstyle)
              where revOpts = reverse opts
                    
                    fmt = fmt' revOpts
                        where fmt' [] = FormatDoc
                              fmt' (Help:opts) = fmt' opts
                              fmt' (StyleName _:opts) = fmt' opts
                              fmt' (flag:opts) = formatOfFlag flag

                    styleFn = style' revOpts
                        where style' [] = \fn -> fn Nothing
                              style' (StyleName fp:opts) = \fn -> withFile fp ReadMode $ \h -> fn $ Just h
                              style' (_:opts) = style' opts

                    inFn = case nonOpts of
                             [] -> \fn -> fn stdin
                             [_] -> withFile (last nonOpts) ReadMode
                             _ -> error "only one Fmark file can be specified"

                    eOut = case nonOpts of
                             [] | fmt == FormatPdf -> error "cannot use stdin with PDF output format"
                             _ | fmt == FormatPdf -> Right $ last nonOpts
                             _ -> Left stdout