{-# LANGUAGE ParallelListComp #-}
module Fmark where

import Control.Monad ((>=>))

import System.Directory (copyFile)
import System.FilePath (addExtension, dropExtensions, combine)
import System.IO
import System.Process
import System.Unix.Directory (withTemporaryDirectory)

import Backend.Latex
import Backend.Xml
import Data.Document
import Data.Text
import Data.Token
import Parser
import Utils
import Weaver


-- | 'pdflatex' @outFp contents@ executes the 'pdflatex' 'Process'
-- with @contents@ as input and @outFp@ as the 'FilePath' for the
-- output PDF.
pdflatex :: FilePath -> String -> IO ()
pdflatex outFp contents =
    withTemporaryDirectory "fmark" $
      \outDir -> withFile "/dev/null" WriteMode $
                   \hNull -> do (Just hIn, _, _, h) <- createProcess
                                                       (proc "pdflatex" ["-output-directory=" ++ outDir,
                                                                         "--jobname=" ++ outFname])
                                                       { std_in = CreatePipe,
                                                         std_out = UseHandle hNull,
                                                         std_err = UseHandle hNull }
                                mapM_ (hPutStrLn hIn) $ filterLines $ lines contents
                                hClose hIn
                                waitForProcess h
                                copyFile (pdfFp outDir) (addExtension (dropExtensions outFp) "pdf")
    where outFname = "texput"
          pdfFp fp = combine fp $ addExtension outFname "pdf"


-- | 'Flag' represents the command line flags that specify output
-- format, display usage information, and 'Style' filename.
data Flag
    -- | Output to 'stdout' in 'Document' format.
    = OutputDoc
    -- | Output to 'stdout' in LaTeX format.
    | OutputLatex
    -- | Output to a PDF file using LaTeX format and 'pdflatex'.
    | OutputPdf
    -- | Output to 'stdout' in XML format.
    | OutputXml
    -- | Display usage information.
    | Help
    | StyleName String
      deriving (Eq, Show)


-- | 'formatFn' @fmt@ maps the output format @fmt@ into the
-- appropriate formatter function.
formatFn :: Flag -> Maybe Document -> Document -> String
formatFn OutputDoc = const show
-- formatFn OutputLatex = docToLatex
formatFn OutputPdf = formatFn OutputLatex
formatFn OutputXml = docToXml
formatFn fmt = error $ "unhandled case: " ++ show fmt


-- | 'formatH' @fmt eOut@ maps the output format @fmt@ and either an
-- output 'Handle' or a 'FilePath' into the appropriate 'IO' function.
formatH :: Flag -> Either Handle FilePath -> String -> IO ()
formatH OutputPdf (Left _) = error "cannot use stdin with PDF output format"
formatH OutputPdf (Right fp) = pdflatex fp
formatH fmt (Left hOut) = hPutStrLn hOut
formatH fmt (Right fp) = \str -> withFile fp WriteMode $ \hOut -> formatH fmt (Left hOut) str


-- | 'fmark' @fmt contents mstring@ is the friendly markup algorithm,
-- using @fmt@ as output format, @contents@ as input and @mstyle@ as
-- an optional style input. 'fmark' return a formatted string
-- according to @fmt@ and a 'List' of warning messages.
fmark :: Flag -> String -> Maybe String -> (String, [String])
fmark fmt contents Nothing =
    (formatFn fmt Nothing $ docify $ classify contents, [])

fmark fmt contents (Just style) =
    let doc = docify $ classify contents in
    let styleDoc = docify $ classify style in
    let (doc', errs) = weaveStyle doc styleDoc in
    (formatFn fmt (Just styleDoc) doc', errs)


-- | 'fmarkH' is an alternative version of 'fmark' that uses 'Handle's
-- instead of 'String's thus performing 'IO' directly for input and
-- output.
fmarkH :: Flag -> Handle -> Either Handle FilePath -> Maybe Handle -> IO ()
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