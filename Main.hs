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


-- | 'replace' @c str@ replaces newlines in @str@ with @c@
--
-- > replace "hello\ngoodbye" == "hello,goodbye"
replace :: Char -> String -> String
replace c =
    map loop
    where loop '\n' = c
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
    = Heading [Document]
    -- | 'Paragraph' is a 'Document' part with a style 'String' and
    -- paragraph content.
    | Paragraph Document
    -- | 'Content' is a 'Document' part that represents a sequence of
    -- 'Document's.
    | Content [Document]
    -- | 'Section' is a 'Document' part that represents a subsection.
    | Section Document

    | Footnote String
    | Literal String
    | Style String Document

instance Show Document where
    show (Heading docs) = "Heading = " ++ show docs
    show (Paragraph doc) = "Paragraph = " ++ show doc
    show (Content docs) = intercalate "\n" $ map show docs
    show (Section doc) = "begin\n" ++ show doc ++ "\nend"

    show (Footnote str) = "Footnote = " ++ str
    show (Literal str) = str
    show (Style sty doc) = "Style(" ++ sty ++ ") = " ++ show doc


reconstruct :: String -> [Document]
reconstruct = loop
    where loop [] = []
          loop ('[':str) =
              case span (/= ']') str of
                (hd, []) -> [Literal $ trim hd]
                (hd, _:tl) -> Footnote (trim hd):loop tl
          loop str =
              Literal (trim hd):loop tl
              where (hd, tl) = span (/= '[') str


ensureDocument :: [Document] -> Document
ensureDocument [doc] = doc
ensureDocument docs = Content docs


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks =
    loop tks [[]]
    where reconstructParagraph :: String -> Document
          reconstructParagraph = ensureDocument . reconstruct

          reconstructHeading :: String -> [Document]
          reconstructHeading = map reconstructParagraph . lines
          
          loop :: [Token] -> [[Document]] -> Document
          loop [] [docs] = ensureDocument $ reverse docs
          loop [] st = loop [EndSection] st

          loop (Text str:tks) (top:st) =
              loop tks ((doc:top):st)
              where doc | isPunctuation $ last str = Paragraph $ reconstructParagraph $ replace ' ' str
                        | otherwise = Heading $ reconstructHeading str

          loop (BeginSection:tks) st =
              loop tks ([]:st)

          loop (EndSection:tks) (top:bot:st) =
              loop tks ((Section (ensureDocument $ reverse top):bot):st)

          loop tks st =
              error $ "\n\n\tloop: unhandled case" ++
                      "\n\n\t tks = " ++ show tks ++
                      "\n\n\t st = " ++ show st ++ "\n\n"

-- | 'weaveStyle' @doc style@ combines content 'Document' @doc@ and
-- style 'Document' @style@ in a single 'Document'.
weaveStyle :: Document -> Document -> (Document, [String])
weaveStyle doc style =
    let (docs, errs) = loop doc style in (ensureDocument docs, errs)
    where msg title cnt desc sty =
              "In " ++ title ++ "\n"
                    ++ prefix "  " (show cnt) ++ "\n"
                    ++ desc ++ "\n"
                    ++ prefix "  " (show sty)

          loop :: Document -> Document -> ([Document], [String])
          loop (Heading cnts) (Heading stys) =
              let errs | length cnts == length stys = []
                       | otherwise = [msg "Heading" cnts "does not match style" stys]
                  (matCnts, unmatCnts) = splitAt (length stys) cnts
                  hds = case unmatCnts of
                          [] -> []
                          _ -> [Heading unmatCnts]
              in
                ([ Style sty cnt | cnt <- matCnts | Literal sty <- stys ] ++ hds, errs)

          loop (Paragraph cnt) (Paragraph (Literal sty)) =
              ([Style (init sty) cnt], [])

          loop doc@(Paragraph _) (Paragraph stys) =
              ([doc], [msg "Paragraph" doc "paragraph styles must be one line long" stys])

          loop (Content docs1) (Content docs2) =
              let (matDocs, unmatDocs) = splitAt (length docs2) docs1
                  (docsSty', errss) = unzip [ loop doc1 doc2 | doc1 <- matDocs | doc2 <- docs2 ]
              in
                ([Content (concat docsSty' ++ unmatDocs)], concat errss)

          loop (Section doc1) (Section doc2) =
              let (doc', errs) = loop doc1 doc2 in
              ([Section $ ensureDocument doc'], errs)

          loop doc _ = ([doc], [show doc])
              

-- | 'XmlState' is the XML generator state.
data XmlState = XmlState Int Bool

-- | 'XmlM' is the type of the XML generator 'Monad'.
type XmlM a = State XmlState a


-- | 'getIdn' is a 'Monad' with the current indentation level.
getIdn :: XmlM Int
getIdn =
    do XmlState idn _ <- get
       return idn

putIdn :: Int -> XmlM ()
putIdn idn =
    modify $ \(XmlState _ pre) -> XmlState idn pre


-- | 'withIdn' @m@ is a 'Monad' that temporarily creates a deeper
-- indentation level for 'Monad' @m@.
withIdn :: XmlM a -> XmlM a
withIdn m =
    do idn <- getIdn
       putIdn $ idn + 2
       val <- m
       putIdn idn
       return val


getPrefix :: XmlM Bool
getPrefix =
    do XmlState _ pre <- get
       return pre


putPrefix :: Bool -> XmlM ()
putPrefix pre =
    modify $ \(XmlState idn _) -> XmlState idn pre


withPrefix :: Bool -> XmlM a -> XmlM a
withPrefix pre m =
    do pre' <- getPrefix
       putPrefix pre
       val <- m
       putPrefix pre'
       return val


-- | 'docToXml' @mstyle doc@ formats a styled 'Document' @doc@ into a
-- XML 'String', where @mstyle@ specifies the style 'Document' used to
-- stylize @doc@.
docToXml :: Maybe Document -> Document -> String
docToXml _ doc =
    intercalate "\n" ["<xml>", str, "</xml>"]
    where str = evalState (loop doc) (XmlState 2 True)

          --xmlIndent lvl str = replicate lvl ' ' ++ str

          xmlIndent :: XmlM String -> XmlM String
          xmlIndent m =
              do idn <- getIdn
                 pre <- getPrefix
                 if pre then
                     do str <- m
                        return $ replicate idn ' ' ++ str
                 else
                     m

          xmlAttribute Nothing = []
          xmlAttribute (Just val) = [("style", val)]

          xmlAttributes [] = ""
          xmlAttributes attrs = ' ':unwords (map (\(id, val) -> id ++ "=\"" ++ val ++ "\"") attrs)

          xmlStr = xmlIndent

          xmlShortTag attrs tag m =
              concat <$> sequence [xmlIndent (return ("<" ++ tag ++ xmlAttributes attrs ++ ">")), withPrefix False m, return ("</" ++ tag ++ ">")]

          xmlLongTag attrs tag m =
              do pre <- getPrefix
                 if pre then
                     concat <$> sequence [xmlIndent (return ("<" ++ tag ++ xmlAttributes attrs ++ ">\n")),
                                          withIdn m,
                                          return "\n",
                                          xmlIndent (return ("</" ++ tag ++ ">"))]
                 else
                     xmlShortTag attrs tag m

          loop (Heading [doc]) = xmlShortTag [] "heading" $ loop doc
          loop (Heading docs) = xmlLongTag [] "heading" $ intercalate "\n" <$> mapM loop docs
          loop (Paragraph doc) = xmlShortTag [] "paragraph" $ loop doc
          loop (Content [doc]) = xmlShortTag [] "content" $ loop doc
          loop (Content docs) = xmlLongTag [] "content" $ intercalate "\n" <$> mapM loop docs
          loop (Section doc) = xmlLongTag [] "section" $ loop doc

          loop (Literal str) = xmlStr (return str)
          loop (Footnote str) = xmlShortTag [] "footnote" (return str)
          loop (Style sty doc) = xmlShortTag [] sty $ loop doc


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