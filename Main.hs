{-# LANGUAGE ParallelListComp #-}
module Main where

import Prelude hiding (lex)

import Control.Monad ((>=>))
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


-- | 'filterLines' @lns@ filters empty lines and lines containing only
-- space characters from @lns@.
filterLines :: [String] -> [String]
filterLines lns = [ ln | ln <- lns, trim ln /= "" ]


-- | 'flatten' @str@ replaces newlines in @str@ with commas.
--
-- > flatten "hello\ngoodbye" == "hello,goodbye"
flatten :: String -> String
flatten =
    map loop
    where loop '\n' = ','
          loop c = c


-- | 'indentation' @ln@ is the number of space characters at the
-- begining of @ln@.
--
-- > indentation "\t hello" == 2
indentation :: String -> Int
indentation ln = length $ takeWhile isSpace ln


-- | 'join' @str1 str2@ appends @str1@ and @str2@ ensuring that is
-- only a single newline space character between them.
--
-- > join "hello \n" "\t goodbye" == "hello\ngoodbye"
join :: String -> String -> String
join str1 str2 = dropWhileEnd isSpace str1 ++ "\n" ++ dropWhile isSpace str2


-- | 'prefix' @pre str@ prepends all lines in 'str' with 'pre'.
--
-- > prefix "->" "hello\ngoodbye" == "->hello\n->goodbye"
prefix :: String -> String -> String
prefix pre str =
    pre ++ prefix' str
    where prefix' =
              concatMap (\c -> case c of
                                 '\n' -> '\n':pre
                                 _ -> [c])


-- | 'push' @x xs@ adds @x@ to @xs@ only if the first element in @xs@
-- is different from @x@.
--
-- > push 1 [2,3] == [1,2,3]
-- > push 1 [1,3] == [1,3]
push :: Eq a => a -> [a] -> [a]
push x (y:ys) | x /= y = x:y:ys
push _ xs = xs


-- | 'trim' @str@ removes leading and trailing space characters from
-- @str@.
--
-- > trim "\t hello \n" == "hello"
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


-- | 'Token' is an unstructured representation of the input.
data Token
    -- | 'Text' corresponds to successive lines of text joined
    -- together.
    = Text String
    -- | 'BeginSection' represents the beginning of a new section,
    -- i.e., increase in indentation or an unmatched decrease in indentation.
    | BeginSection
    -- | 'EndSection' represents the end of a section, i.e., a
    -- decrease in indentation.
    | EndSection
      deriving (Show)


-- | 'section' @idns idn@ is the 'List' of 'BeginSection' and
-- 'EndSection' 'Token's issued according the indentation stack @idns@
-- and current indentation @idn@.
section :: [Int] -> Int -> [Token]
section [] _ = error "section: idns is empty"
section (idn1:idns) idn2 =
    case compare idn1 idn2 of
      EQ -> []
      LT -> [BeginSection]
      GT -> EndSection:section (dropWhile (> idn1) idns) idn2


-- | 'reduce' @idns ln@ is the 'List' containing the 'Text' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> String -> [Token]
reduce idns ln = section idns (indentation ln) ++ [Text $ trim ln]


-- | 'classify' @str@ is the 'List' of 'Token's of @str@.
classify :: String -> [Token]
classify str =
    classify' [0] $ lines str
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


-- | 'Document' is a structured representation of the input.
data Document
    -- | 'Heading' is a 'Document' part with a style 'String' and
    -- heading content.
    = Heading (Maybe String) [Document]
    -- | 'Paragraph' is a 'Document' part with a style 'String' and
    -- paragraph content.
    | Paragraph (Maybe String) [Document]
    -- | 'Content' is a 'Document' part that represents a sequence of
    -- 'Document's.
    | Content (Maybe String) [Document]
    -- | 'Section' is a 'Document' part that represents a subsection.
    | Section (Maybe String) Document

    | Footnote (Maybe String) Document
    | Line String

instance Show Document where
    show (Heading Nothing doc) = "Heading = " ++ show doc
    show (Heading (Just sty) doc) = "Heading(" ++ flatten sty ++ ") = " ++ show doc
    show (Paragraph Nothing doc) = "Paragraph = " ++ show doc
    show (Paragraph (Just sty) doc) = "Paragraph(" ++ sty ++ ") = " ++ show doc
    show (Content _ docs) = intercalate "\n" $ map show docs
    show (Section _ doc) = "begin\n" ++ show doc ++ "\nend"

    show (Footnote Nothing doc) = "Footnote = " ++ show doc
    show (Footnote (Just sty) doc) = "Footnote(" ++ sty ++ ") = " ++ show doc
    show (Line str) = str


reconstruct :: String -> [Document]
reconstruct = map (reconstruct' . loop) . lines
    where loop [] = []
          loop ('[':str) =
              case span (/= ']') str of
                (hd, []) -> [Line $ trim hd]
                (hd, _:tl) -> Footnote Nothing (Line (trim hd)):loop tl
          loop str =
              Line (trim hd):loop tl
              where (hd, tl) = span (/= '[') str

          reconstruct' [] = error "reconstruct: reconstruct': empty list"
          reconstruct' [doc] = doc
          reconstruct' docs = Content Nothing docs


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks =
    loop tks [[]]
    where loop :: [Token] -> [[Document]] -> Document
          loop [] [doc] = Content Nothing $ reverse doc
          loop [] st = loop [EndSection] st

          loop (Text str:tks) (top:st) =
              let cons = if isPunctuation $ last str then Paragraph else Heading in
              loop tks ((cons Nothing (reconstruct str):top):st)

          loop (BeginSection:tks) st =
              loop tks ([]:st)

          loop (EndSection:tks) (top:bot:st) =
              loop tks ((Section Nothing (Content Nothing $ reverse top):bot):st)

          loop tks st =
              error $ "\n\n\tloop: unhandled case" ++
                      "\n\n\t tks = " ++ show tks ++
                      "\n\n\t st = " ++ show st ++ "\n\n"

-- | 'weaveStyle' @doc style@ combines content 'Document' @doc@ and
-- style 'Document' @style@ in a single 'Document'.
weaveStyle :: Document -> Document -> (Document, [String])
weaveStyle doc style =
    loop doc style
    where msg title cnt desc sty =
              "In " ++ title ++ "\n"
                    ++ prefix "  " (show cnt) ++ "\n"
                    ++ desc ++ "\n"
                    ++ prefix "  " (show sty)

          loop :: Document -> Document -> (Document, [String])
          loop (Heading _ strLns) (Heading _ styLns) =
              let errs = if length strLns == length styLns then
                             []
                         else
                             [msg "Heading" strLns "does not match style" styLns]
                  (withStyLns, noStyLns) = splitAt (length styLns) strLns
                  hds = [ Heading (Just sty) [Line str] | Line str <- withStyLns | Line sty <- styLns ]
                        ++
                        case noStyLns of
                          [] -> []
                          _ -> [Heading Nothing noStyLns]
              in
                case hds of
                  [] -> error "weaveStyle: loop: headings are empty"
                  [hd] -> (hd, errs)
                  _ -> (Content Nothing hds, errs)

          loop (Paragraph _ cnts) (Paragraph _ stys) =
              -- currently, paragraph newlines are striped before getting here... is this true ?
              let errs | length stys == 1 = []
                       | otherwise = [msg "Paragraph" cnts "paragraph styles must be one line" stys]
                  [Line sty] = stys
              in
                (Paragraph (Just sty) cnts, [])

          loop (Content _ docs1) (Content _ docs2) =
              let (docsSty, docsNoSty) = splitAt (length docs2) docs1
                  (docsSty', errss) = unzip [ loop doc1 doc2 | doc1 <- docsSty | doc2 <- docs2 ] in
              (Content Nothing (docsSty' ++ docsNoSty), concat errss)

          loop (Section _ doc1) (Section _ doc2) =
              let (doc', errs) = loop doc1 doc2 in
              (Section Nothing doc', errs)

          loop doc _ = (doc, [show doc])
              

-- | 'XmlState' is the XML generator state.
data XmlState = XmlState Int

-- | 'XmlM' is the type of the XML generator 'Monad'.
type XmlM a = State XmlState a


-- | 'getIdn' is a 'Monad' with the current indentation level.
getIdn :: XmlM Int
getIdn =
    do XmlState idn <- get
       return idn


-- | 'withIdn' @m@ is a 'Monad' that temporarily creates a deeper
-- indentation level for 'Monad' @m@.
withIdn :: XmlM a -> XmlM a
withIdn m =
    do idn <- getIdn
       put $ XmlState $ idn + 2
       val <- m
       put $ XmlState idn
       return val


-- | 'docToXml' @mstyle doc@ formats a styled 'Document' @doc@ into a
-- XML 'String', where @mstyle@ specifies the style 'Document' used to
-- stylize @doc@.
docToXml :: Maybe Document -> Document -> String
docToXml _ doc =
    intercalate "\n" ["<xml>", str, "</xml>"]
    where str = evalState (loop doc) (XmlState 2)

          xmlIndent lvl str = replicate lvl ' ' ++ str

          xmlAttribute Nothing = []
          xmlAttribute (Just val) = [("style", val)]

          xmlAttributes :: [(String, String)] -> String
          xmlAttributes [] = ""
          xmlAttributes attrs = ' ':unwords (map (\(id, val) -> id ++ "=\"" ++ val ++ "\"") attrs)

          xmlShortTag attrs tag str =
              do idn <- getIdn
                 return $ xmlIndent idn "<" ++ tag ++ xmlAttributes attrs ++ ">" ++ str ++ "</" ++ tag ++ ">"

          xmlLongTag attrs tag m =
              do idn <- getIdn
                 str <- withIdn m
                 return $
                   (xmlIndent idn "<" ++ tag ++ xmlAttributes attrs ++ ">\n") ++
                   str ++ "\n" ++
                   (xmlIndent idn "</" ++ tag ++ ">")

          loop (Heading mstyle doc) = xmlLongTag (xmlAttribute mstyle) "heading" $ intercalate "\n" <$> mapM loop doc
          loop (Paragraph mstyle doc) = xmlLongTag (xmlAttribute mstyle) "paragraph" $ intercalate "\n" <$> mapM loop doc
          loop (Content mstyle docs) = xmlLongTag (xmlAttribute mstyle) "content" $ intercalate "\n" <$> mapM loop docs 
          loop (Section mstyle doc) = xmlLongTag (xmlAttribute mstyle) "section" $ loop doc

          loop (Line str) = xmlShortTag [] "line" str
          loop (Footnote mstyle doc) = xmlLongTag (xmlAttribute mstyle) "footnote" $ loop doc


-- | 'docToLatex' @mstyle doc@ formats a styled 'Document' @doc@ into
-- a LaTeX 'String', where @mstyle@ specifies the style 'Document'
-- used to stylize @doc@.
-- docToLatex :: Maybe Document -> Document -> String
-- docToLatex mstyle doc =
--     let ps = properties doc 
--         title = lookup "Title" ps
--         author = lookup "Author" ps
--         date = lookup "Date" ps
--         abstract = lookup "Abstract" ps
--         maketitle = case mstyle of
--                       Nothing -> ""
--                       _ -> def "maketitle"
--     in
--       seq [comArgs "documentclass" ["a4paper"] "article",
--            comArgs "usepackage" ["utf8"] "inputenc",
--            prop (com "title") title,
--            prop (com "author") author,
--            prop (com "date") date,
--            env "document" $ seq [maketitle,
--                                  prop (env "abstract") abstract,
--                                  loop 0 doc]]
--     where properties (Heading msty str) = maybe [] (\sty -> [(sty, str)]) msty
--           properties (Paragraph msty str) = maybe [] (\sty -> [(sty, str)]) msty
--           properties (Content _ docs) = concatMap properties docs
--           properties (Section _ doc) = properties doc

--           lit = concatMap lit'
--               where lit' '#' = "\\#"
--                     lit' c = [c]

--           nls = concatMap nls'
--               where nls' '\n' = "\\\\"
--                     nls' c = [c]

--           def id = '\\':lit id
--           com id str = "\\" ++ lit id ++ "{" ++ lit str ++ "}"
--           comArgs id args str = "\\" ++ lit id ++ "[" ++ intercalate "," (map lit args) ++ "]{" ++ lit str ++ "}"
--           env id str = "\\begin{" ++ lit id ++ "}\n" ++ lit str ++ "\n\\end{" ++ lit id ++ "}"

--           prop fn = maybe "" $ fn . lit

--           sec lvl str
--               | lvl < 3 = com (concat (replicate lvl "sub") ++ "section") $ nls str
--               | lvl == 3 = com "paragraph" $ nls str
--               | lvl == 4 = com "subparagraph" $ nls str

--           par = lit

--           seq = intercalate "\n\n" . filter (\ln -> trim ln /= "")

--           loop lvl (Heading Nothing str) = sec lvl str
--           loop _ (Paragraph Nothing str) = par str
--           loop lvl (Content _ docs) = seq $ map (loop lvl) docs
--           loop lvl (Section _ doc) = loop (lvl + 1) doc
--           loop _ _ = ""


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
    | Style String
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

fmark fmt contents mstyle@(Just style) =
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


-- | 'options' represents the command line options.
options = [Option "d" ["doc"] (NoArg OutputDoc) "Output doc",
           Option "l" ["latex"] (NoArg OutputLatex) "Output latex",
           Option "p" ["pdf"] (NoArg OutputPdf) "Output PDF",
           Option "s" ["style"] (ReqArg Style "style-name") "Style",
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
                              fmt' (Style _:opts) = fmt' opts
                              fmt' (flag:opts) = flag

                    styleFn = style' revOpts
                        where style' [] = \fn -> fn Nothing
                              style' (Style fp:opts) = \fn -> withFile fp ReadMode $ \h -> fn $ Just h
                              style' (_:opts) = style' opts

                    inFn = case nonOpts of
                             [] -> \fn -> fn stdin
                             _ -> withFile (last nonOpts) ReadMode

                    eOut = case nonOpts of
                             [] | fmt == OutputPdf -> error "cannot use stdin with PDF output format"
                             _ | fmt == OutputPdf -> Right $ last nonOpts
                             _ -> Left stdout