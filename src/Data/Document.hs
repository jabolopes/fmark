module Data.Document where

import Data.Token (Srcloc)


data ItemT
    = HeadingT
    | ParagraphT
    | UnorderedT
      deriving (Eq, Show)


data Element
    = Block String
    | Content
    | Enumeration
    | Heading
    | Item ItemT
    | Paragraph
    | Plain String
--    | Section
    | Span String
      deriving (Eq, Show)


data Document = Document Srcloc Element [Document]
                deriving (Show)


mkDocument = Document (0, [], "")


mkBlock :: String -> [Document] -> Document
mkBlock sty docs = Document (0, [], "") (Block sty) docs


mkContent :: [Document] -> Document
mkContent = Document (0, [], "") Content


mkEnumeration :: [Document] -> Document
mkEnumeration = Document (0, [], "") Enumeration


mkHeading :: [Document] -> Document
mkHeading = Document (0, [], "") Heading


mkItem :: ItemT -> [Document] -> Document
mkItem t = Document (0, [], "") $ Item t


mkParagraph :: [Document] -> Document
mkParagraph = Document (0, [], "") Paragraph


mkPlain :: String -> Document
mkPlain str = Document (0, [], "") (Plain str) []


-- mkSection :: [Document] -> Document
-- mkSection = Document (0, [], "") $ Section


mkSpan :: String -> [Document] -> Document
mkSpan sty docs = Document (0, [], "") (Span sty) docs


isDocument :: Element -> Document -> Bool
isDocument t1 (Document _ t2 _) = t1 == t2


isBlock :: Document -> Bool
isBlock (Document _ (Block _) _) = True
isBlock _ = False


isEnumeration :: Document -> Bool
isEnumeration = isDocument Enumeration


isHeadingItem :: Document -> Bool
isHeadingItem = isDocument (Item HeadingT)


isParagraphItem :: Document -> Bool
isParagraphItem = isDocument (Item ParagraphT)


isUnorderedItem :: Document -> Bool
isUnorderedItem = isDocument (Item UnorderedT)