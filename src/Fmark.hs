{-# LANGUAGE ParallelListComp #-}
module Fmark where

import Control.Monad ((>=>))
import Data.List (intercalate)

import System.Directory (copyFile)
import System.FilePath (addExtension, dropExtensions, combine)
import System.IO
import System.Process
--import System.Unix.Directory (withTemporaryDirectory)

--import Backend.Latex
import Backend.Xml
import Data.Document
import Data.Token
import Lexer
import Parser
import Utils
--import Weaver


-- | 'pdflatex' @outFp contents@ executes the 'pdflatex' 'Process'
-- with @contents@ as input and @outFp@ as the 'FilePath' for the
-- output PDF.
-- pdflatex :: FilePath -> String -> IO ()
-- pdflatex outFp contents =
--     withTemporaryDirectory "fmark" $
--       \outDir -> withFile "/dev/null" WriteMode $
--                    \hNull -> do (Just hIn, _, _, h) <- createProcess
--                                                        (proc "pdflatex" ["-output-directory=" ++ outDir,
--                                                                          "--jobname=" ++ outFname])
--                                                        { std_in = CreatePipe,
--                                                          std_out = UseHandle hNull,
--                                                          std_err = UseHandle hNull }
--                                 mapM_ (hPutStrLn hIn) $ filterLines $ lines contents
--                                 hClose hIn
--                                 waitForProcess h
--                                 copyFile (pdfFp outDir) (addExtension (dropExtensions outFp) "pdf")
--     where outFname = "texput"
--           pdfFp fp = combine fp $ addExtension outFname "pdf"


data Format
    -- | Output to 'stdout' in 'Document' format.
    = FormatDoc
    -- | Output to 'stdout' in LaTeX format.
    | FormatLatex
    -- | Output to a PDF file using LaTeX format and 'pdflatex'.
    | FormatPdf      

    | FormatToken

    -- | Output to 'stdout' in XML format.
    | FormatXml
    deriving (Eq, Show)


-- | 'formatFn' @fmt@ maps the output format @fmt@ into the
-- appropriate formatter function.
formatFn :: Format -> Maybe Document -> Document -> String
formatFn FormatDoc = const show
--formatFn FormatLatex = docToLatex
formatFn FormatPdf = formatFn FormatLatex
formatFn FormatXml = docToXml


-- | 'formatH' @fmt eOut@ maps the output format @fmt@ and either an
-- output 'Handle' or a 'FilePath' into the appropriate 'IO' function.
formatH :: Format -> Either Handle FilePath -> String -> IO ()
formatH FormatPdf (Left _) = error "cannot use stdin with PDF output format"
--formatH FormatPdf (Right fp) = pdflatex fp
formatH fmt (Left hOut) = hPutStrLn hOut
formatH fmt (Right fp) = \str -> withFile fp WriteMode $ \hOut -> formatH fmt (Left hOut) str


-- | 'fmark' @fmt contents mstring@ is the friendly markup algorithm,
-- using @fmt@ as output format, @contents@ as input and @mstyle@ as
-- an optional style input. 'fmark' return a formatted string
-- according to @fmt@ and a 'List' of warning messages.
fmark :: Format -> String -> Maybe String -> (String, [String])
fmark FormatToken contents _ =
    (intercalate "\n" $ map show $ classify contents, [])

fmark fmt contents Nothing =
    (formatFn fmt Nothing $ docify $ classify contents, [])

-- fmark fmt contents (Just style) =
--     let doc = docify $ classify contents in
--     let styleDoc = docify $ classify style in
--     let (doc', errs) = weave doc styleDoc in
--     (formatFn fmt (Just styleDoc) doc', errs)


-- | 'fmarkH' is an alternative version of 'fmark' that uses 'Handle's
-- instead of 'String's thus performing 'IO' directly for input and
-- output.
fmarkH :: Format -> Handle -> Either Handle FilePath -> Maybe Handle -> IO ()
fmarkH fmt hIn eOut mstyle =
    do contents <- hGetContents hIn
       mstyle <- maybe (return Nothing)
                       (hGetContents >=> (return . Just))
                       mstyle
       let (str, errs) = fmark fmt contents mstyle
       formatH fmt eOut str
       case errs of
         [] -> return ()
         _ -> do hPutStrLn stderr "Style warnings:"
                 mapM_ (hPutStrLn stderr) errs