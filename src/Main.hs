module Main where

import Data.List (intercalate, dropWhileEnd)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.IO


import Fmark


-- | 'options' represents the command line options.
options = [Option "d" ["doc"] (NoArg OutputDoc) "Output doc",
           Option "l" ["latex"] (NoArg OutputLatex) "Output latex",
           Option "p" ["pdf"] (NoArg OutputPdf) "Output PDF",
           Option "s" ["style"] (ReqArg StyleName "style-name") "Style",
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
                        where fmt' [] = OutputDoc
                              fmt' (Help:opts) = fmt' opts
                              fmt' (StyleName _:opts) = fmt' opts
                              fmt' (flag:opts) = flag

                    styleFn = style' revOpts
                        where style' [] = \fn -> fn Nothing
                              style' (StyleName fp:opts) = \fn -> withFile fp ReadMode $ \h -> fn $ Just h
                              style' (_:opts) = style' opts

                    inFn = case nonOpts of
                             [] -> \fn -> fn stdin
                             _ -> withFile (last nonOpts) ReadMode

                    eOut = case nonOpts of
                             [] | fmt == OutputPdf -> error "cannot use stdin with PDF output format"
                             _ | fmt == OutputPdf -> Right $ last nonOpts
                             _ -> Left stdout