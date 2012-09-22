module Backend.Latex where

import Prelude hiding (seq)

import Data.Functor ((<$>))
import Data.List (intercalate)

import Utils (trim)
import Data.Document
import Data.Text


fulltitle :: Maybe String -> Maybe String -> String
fulltitle Nothing Nothing = ""
fulltitle (Just t) Nothing = com "title" t
fulltitle Nothing (Just s) = com "title" $ com "large" s
fulltitle (Just t) (Just s) = com "title" $ nls $ t ++ "\n" ++ com "large" s


lit = concatMap lit'
    where lit' '#' = "\\#"
          lit' c = [c]


nls = concatMap nls'
    where nls' '\n' = "\\\\"
          nls' c = [c]


def id = '\\':lit id
com id str = "\\" ++ lit id ++ "{" ++ lit str ++ "}"
comArgs id args str = "\\" ++ lit id ++ "[" ++ intercalate "," (map lit args) ++ "]{" ++ lit str ++ "}"
env id str = "\\begin{" ++ lit id ++ "}\n" ++ lit str ++ "\n\\end{" ++ lit id ++ "}"
envArg id arg str = "\\begin{" ++ lit id ++ "}{" ++ arg ++ "}\n" ++ lit str ++ "\n\\end{" ++ lit id ++ "}"


prop fn = maybe "" $ fn . lit


sec lvl str
    | lvl < 3 = com (concat (replicate lvl "sub") ++ "section") $ nls str
    | lvl == 3 = com "paragraph" $ nls str
    | lvl == 4 = com "subparagraph" $ nls str


par = lit


seq = intercalate "\n" . filter (\ln -> trim ln /= "")


loopText :: Text -> String
loopText (Footnote str) = com "footnote" str
loopText (Plain str) = lit str


properties :: Document -> [(String, String)]
properties (Heading _ _) = []
properties (Paragraph _ _) = []
properties (Content docs) = concatMap properties docs
properties (Section doc) = properties doc
properties (Style _ sty lns) = [(sty, intercalate "\n" $ [ concatMap loopText txts | txts <- lns ])]


loop :: Int -> Document -> String
loop lvl (Heading _ lns) = sec lvl $ intercalate "\n" [ concatMap loopText txts | txts <- lns ]
loop _ (Paragraph _ txts) = concatMap loopText txts
loop lvl (Content docs) = seq $ map (loop lvl) docs
loop lvl (Section doc) = loop (lvl + 1) doc
loop _ Style {} = ""


-- | 'docToLatex' @mstyle doc@ formats a styled 'Document' @doc@ into
-- a LaTeX 'String', where @mstyle@ specifies the style 'Document'
-- used to stylize @doc@.
docToLatexArticle :: Maybe Document -> Document -> String
docToLatexArticle mstyle doc =
    let ps = properties doc 
        title = lookup "Title" ps
        subtitle = lookup "Subtitle" ps
        author = lookup "Author" ps
        date = lookup "Date" ps
        abstract = lookup "Abstract" ps
        maketitle = case (title, author, date) of
                      (Just _, Just _, Just _) -> def "maketitle"
                      _ -> ""
    in
      seq [comArgs "documentclass" ["a4paper"] "article",
           comArgs "usepackage" ["utf8"] "inputenc",
           fulltitle title subtitle,
           prop (com "author") author,
           prop (com "date") date,
           env "document" $ seq [maketitle,
                                 prop (env "abstract") abstract,
                                 loop 0 doc]]


docToLatex :: Maybe Document -> Document -> String
docToLatex mstyle doc =
    let ps = properties doc 
        signature = lookup "Signature" ps
        address = lookup "Address" ps
        date = lookup "Date" ps
        opening = lookup "Opening" ps
        letter = lookup "Letter" ps
        closing = lookup "Closing" ps
        makeOpening = case opening of
                        Nothing -> ""
                        Just str -> com "opening" str
        makeLetter = case letter of
                        Nothing -> ""
                        Just str -> str
        makeClosing = case closing of
                        Nothing -> ""
                        Just str -> com "closing" str
    in
      seq [comArgs "documentclass" ["a4paper"] "letter",
           comArgs "usepackage" ["utf8"] "inputenc",
           prop (com "signature" . nls) signature,
           prop (com "address" . nls) address,
           prop (com "date") date,
           env "document" $
               envArg "letter" "" $
                   seq [makeOpening, makeLetter, makeClosing]]