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

          loop :: Document -> Document -> (Document, [String])
          loop (Heading _ str) (Heading _ sty) =
              let strLns = lines str
                  styLns = lines sty
                  errs = if length strLns == length styLns then
                             []
                         else
                             [msg "Heading" str "does not match style" sty]
                  (withStyLns, noStyLns) = splitAt (length styLns) strLns
                  hds = [ Heading (Just sty) str | str <- withStyLns | sty <- styLns ]
                        ++
                        case noStyLns of
                          [] -> []
                          _ -> [Heading Nothing $ unlines noStyLns]
              in
                case hds of
                  [] -> error "weaveStyle: loop: headings are empty"
                  [hd] -> (hd, errs)
                  _ -> (Content Nothing hds, errs)

          loop (Paragraph _ str) (Paragraph _ sty) =
              -- currently, paragraph newlines are striped before getting here...
              let errs | length (lines sty) == 1 = []
                       | otherwise = [msg "Paragraph" str "paragraph styles must be one line" sty] in
              (Paragraph (Just (init sty)) str, [])

          loop (Content _ docs1) (Content _ docs2) =
              let (docsSty, docsNoSty) = splitAt (length docs2) docs1
                  (docsSty', errss) = unzip [ loop doc1 doc2 | doc1 <- docsSty | doc2 <- docs2 ] in
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


docToXml :: Maybe Document -> Document -> String
docToXml _ doc =
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


docToLatex :: Maybe Document -> Document -> String
docToLatex mstyle doc =
    let ps = properties doc 
        title = lookup "Title" ps
        author = lookup "Author" ps
        date = lookup "Date" ps
        abstract = lookup "Abstract" ps
        maketitle = case mstyle of
                      Nothing -> ""
                      _ -> def "maketitle"
    in
      seq [comArgs "documentclass" ["a4paper"] "article",
           comArgs "usepackage" ["utf8"] "inputenc",
           prop (com "title") title,
           prop (com "author") author,
           prop (com "date") date,
           env "document" $ seq [maketitle,
                                 prop (env "abstract") abstract,
                                 loop 0 doc]]
    where properties (Heading msty str) = maybe [] (\sty -> [(sty, str)]) msty
          properties (Paragraph msty str) = maybe [] (\sty -> [(sty, str)]) msty
          properties (Content _ docs) = concatMap properties docs
          properties (Section _ doc) = properties doc
          
          lit str =
              concatMap lit' str
              where lit' '#' = "\\#"
                    lit' c = [c]

          nls str =
              concatMap nls' str
              where nls' '\n' = "\\\\"
                    nls' c = [c]

          def id = "\\" ++ lit id
          com id str = "\\" ++ lit id ++ "{" ++ lit str ++ "}"
          comArgs id args str = "\\" ++ lit id ++ "[" ++ (intercalate "," $ map lit args) ++ "]{" ++ lit str ++ "}"
          env id str = "\\begin{" ++ lit id ++ "}\n" ++ lit str ++ "\n\\end{" ++ lit id ++ "}"

          prop fn mp = maybe "" (\str -> fn $ lit str) mp

          sec lvl str
              | lvl < 3 = com (concat (replicate lvl "sub") ++ "section") $ nls str
              | lvl == 3 = com "paragraph" $ nls str
              | lvl == 4 = com "subparagraph" $ nls str

          par = lit

          seq = intercalate "\n\n" . filter (\ln -> trim ln /= "")

          loop lvl (Heading Nothing str) = sec lvl str
          loop _ (Paragraph Nothing str) = par str
          loop lvl (Content _ docs) = seq $ map (loop lvl) docs
          loop lvl (Section _ doc) = loop (lvl + 1) doc
          loop _ _ = ""


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
    -- | Output to 'stdout' in XML format.
    | OutputXml
    -- | Display usage information.
    | Help
    | Style String
      deriving (Eq, Show)


formatFn :: Flag -> Maybe Document -> Document -> String
formatFn OutputDoc = const show
formatFn OutputLatex = docToLatex
formatFn OutputPdf = formatFn OutputLatex
formatFn OutputXml = docToXml
formatFn fmt = error $ "unhandled case: " ++ show fmt


formatH :: Flag -> Either Handle FilePath -> String -> IO ()
formatH OutputPdf (Left _) = error "cannot use stdin with PDF output format"
formatH OutputPdf (Right fp) = pdflatex fp
formatH fmt (Left hOut) = hPutStrLn hOut
formatH fmt (Right fp) = \str -> withFile fp WriteMode $ \hOut -> formatH fmt (Left hOut) str


fmark :: Flag -> String -> Maybe String -> (String, [String])
fmark fmt contents Nothing =
    (formatFn fmt Nothing $ docify $ classify contents, [])

fmark fmt contents mstyle@(Just style) =
    let doc = docify $ classify contents in
    let styleDoc = docify $ classify style in
    let (doc', errs) = weaveStyle doc styleDoc in
    (formatFn fmt (Just styleDoc) doc', errs)


fmarkH :: Flag -> Handle -> Either Handle FilePath -> Maybe Handle -> IO ()
fmarkH fmt hIn eOut mstyle =
    do contents <- hGetContents hIn
       mstyle <- maybe (return Nothing)
                       (\hStyle -> hGetContents hStyle >>= return . Just)
                       mstyle
       let (str, errs) = fmark fmt contents mstyle
       formatH fmt eOut str
       case errs of
         [] -> return ()
         _ -> do hPutStrLn stderr "Style warnings:"
                 mapM_ (hPutStrLn stderr) errs


-- | Command line options.
options = [Option ['d'] ["doc"] (NoArg OutputDoc) "Output doc",
           Option ['l'] ["latex"] (NoArg OutputLatex) "Output latex",
           Option ['p'] ["pdf"] (NoArg OutputPdf) "Output PDF",
           Option ['s'] ["style"] (ReqArg Style "style-name") "Style",
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
              styleFn $ \mstyle -> (inFn $ \hIn -> fmarkH fmt hIn eOut mstyle)
              where revOpts = reverse opts
                    
                    fmt = fmt' revOpts
                        where fmt' [] = OutputDoc
                              fmt' (Help:opts) = fmt' opts
                              fmt' ((Style _):opts) = fmt' opts
                              fmt' (flag:opts) = flag

                    styleFn = style' revOpts
                        where style' [] = \fn -> fn Nothing
                              style' ((Style fp):opts) = \fn -> withFile fp ReadMode $ \h -> fn $ Just h
                              style' (_:opts) = style' opts

                    inFn = case nonOpts of
                             [] -> \fn -> fn stdin
                             _ -> withFile (last nonOpts) ReadMode

                    eOut = case nonOpts of
                             [] | fmt == OutputPdf -> error "cannot use stdin with PDF output format"
                             _ | fmt == OutputPdf -> Right $ last nonOpts
                             _ -> Left stdout