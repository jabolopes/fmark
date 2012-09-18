module Data.Document where

import Data.Char (isPunctuation)
import Data.List (intercalate)

import Data.Text


-- | 'Srcloc' represents an association between 'Document' parts and
-- 'Token' elements.
type Srcloc = (Int, String)


-- | 'Document' is a structured representation of the input.
data Document
    -- | 'Heading' is a 'Document' part with a source location and
    -- heading content.
    = Heading Srcloc [[Text]]
    -- | 'Paragraph' is a 'Document' part with a source location and
    -- paragraph content.
    | Paragraph Srcloc [Text]
    -- | 'Content' is a 'Document' part that represents a sequence of
    -- 'Document's.
    | Content [Document]
    -- | 'Section' is a 'Document' part that represents a subsection.
    | Section Document
    -- | 'Style' is a 'Document' part that represents a style 'Text'
    -- element, with a source location and a style 'String'.
    | Style Srcloc String Text

instance Show Document where
    show (Heading _ docs) = "Heading = " ++ show docs
    show (Paragraph _ doc) = "Paragraph = " ++ show doc
    show (Content docs) = intercalate "\n" $ map show docs
    show (Section doc) = "begin\n" ++ show doc ++ "\nend"
    show (Style _ sty str) = "(" ++ sty ++ ") = " ++ show str


-- | 'ensureDocument' @docs@ the first elements of @docs@ if @docs@ is
-- a singleton list, otherwise, it returns a 'Content docs'
ensureDocument :: [Document] -> Document
ensureDocument [doc] = doc
ensureDocument docs = Content docs


-- | 'rangeLoc' @doc@ returns the pair of surrounding 'Srcloc's of
-- @doc@ or its children.
rangeloc :: Document -> (Srcloc, Srcloc)
rangeloc (Heading loc _) = (loc, loc)
rangeloc (Paragraph loc _) = (loc, loc)
rangeloc (Content (doc:docs)) = (fst $ rangeloc doc, snd $ rangeloc $ last docs)
rangeloc (Section doc) = rangeloc doc
rangeloc (Style loc _ _) = (loc, loc)