module Data.Document where

import Data.Char (isPunctuation)
import Data.List (intercalate)

import Data.Text


type Srcloc = (Int, String)


-- | 'Document' is a structured representation of the input.
data Document
    -- | 'Heading' is a 'Document' part with a style 'String' and
    -- heading content.
    = Heading Srcloc [[Text]]
    -- | 'Paragraph' is a 'Document' part with a style 'String' and
    -- paragraph content.
    | Paragraph Srcloc [Text]
    -- | 'Content' is a 'Document' part that represents a sequence of
    -- 'Document's.
    | Content [Document]
    -- | 'Section' is a 'Document' part that represents a subsection.
    | Section Document

    | Style Srcloc String Text

instance Show Document where
    show (Heading _ docs) = "Heading = " ++ show docs
    show (Paragraph _ doc) = "Paragraph = " ++ show doc
    show (Content docs) = intercalate "\n" $ map show docs
    show (Section doc) = "begin\n" ++ show doc ++ "\nend"

    show (Style _ sty str) = "(" ++ sty ++ ") = " ++ show str


ensureDocument :: [Document] -> Document
ensureDocument [doc] = doc
ensureDocument docs = Content docs


rangeloc :: Document -> (Srcloc, Srcloc)
rangeloc (Heading loc _) = (loc, loc)
rangeloc (Paragraph loc _) = (loc, loc)
rangeloc (Content (doc:docs)) = (fst $ rangeloc doc, snd $ rangeloc $ last docs)
rangeloc (Section doc) = rangeloc doc
rangeloc (Style loc _ _) = (loc, loc)