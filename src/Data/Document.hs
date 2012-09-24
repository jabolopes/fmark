module Data.Document where

import Data.Char (isPunctuation)
import Data.List (intercalate)

--import Data.Text
import Data.Token


-- -- | 'Document' is a structured representation of the input.
-- data Document
--     -- | 'Heading' is a 'Document' part with a source location and
--     -- heading content.
--     = Heading Srcloc [[Text]]
--     -- | 'Paragraph' is a 'Document' part with a source location and
--     -- paragraph content.
--     | Paragraph Srcloc [Text]
--     -- | 'Content' is a 'Document' part that represents a sequence of
--     -- 'Document's.
--     | Content [Document]
--     -- | 'Section' is a 'Document' part that represents a subsection.
--     | Section Document
--     -- | 'Style' is a 'Document' part that represents a style 'Text'
--     -- element, with a source location and a style 'String'.
--     | Style Srcloc String [[Text]]

--     | Unordered [Document]


data Element
    = Content
    | Enumeration
    | Heading
    | Item
    | Paragraph
    | Plain String
    | Section
    | Span String
    | Style String
      deriving (Show)


data Document = Document Srcloc Element [Document]
                deriving (Show)


mkContent :: [Document] -> Document
mkContent = Document (0, "") Content


mkEnumeration :: [Document] -> Document
mkEnumeration = Document (0, "") Enumeration


mkHeading :: [[Document]] -> Document
mkHeading = Document (0, "") Heading . map mkContent


mkItem :: [Document] -> Document
mkItem = Document (0, "") Item


mkParagraph :: [Document] -> Document
mkParagraph = Document (0, "") Paragraph


mkPlain :: String -> Document
mkPlain str = Document (0, "") (Plain str) []


mkSpan :: String -> [Document] -> Document
mkSpan sty docs = Document (0, "") (Span sty) docs


isEnumeration :: Document -> Bool
isEnumeration (Document _ Enumeration _) = True
isEnumeration _ = False

isItem :: Document -> Bool
isItem (Document _ Item _) = True
isItem _ = False

-- instance Show Document where
--     show (Heading _ lns) = "Heading = " ++ show lns
--     show (Paragraph _ txts) = "Paragraph = " ++ show txts
--     show (Content docs) = intercalate "\n" $ map show docs
--     show (Section doc) = "begin\n" ++ show doc ++ "\nend"
--     show (Style _ sty lns) = sty ++ " = " ++ show lns
    
--     show (Unordered docs) = "Unordered = " ++ intercalate "," (map show docs)


-- isContent :: Document -> Bool
-- isContent (Content _) = True
-- isContent _ = False


-- | 'ensureDocument' @docs@ the first elements of @docs@ if @docs@ is
-- a singleton list, otherwise, it returns a 'Content docs'
-- ensureDocument :: [Document] -> Document
-- ensureDocument [doc] = doc
-- ensureDocument docs = Content docs


-- | 'rangeLoc' @doc@ returns the pair of surrounding 'Srcloc's of
-- @doc@ or its children.
-- rangeloc :: Document -> (Srcloc, Srcloc)
-- rangeloc (Heading loc _) = (loc, loc)
-- rangeloc (Paragraph loc _) = (loc, loc)
-- rangeloc (Content (doc:docs)) = (fst $ rangeloc doc, snd $ rangeloc $ last docs)
-- rangeloc (Section doc) = rangeloc doc
-- rangeloc (Style loc _ _) = (loc, loc)