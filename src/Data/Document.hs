module Data.Document where

import Data.Token (Srcloc)


data ItemT
    = HeadingT
    | ParagraphT
    | UnorderedT
      deriving (Eq, Show)


data Element
    = Content
    | Enumeration
    | Heading
    | Item ItemT
    | Paragraph
    | Plain String
    | Section
    | Span String
    | Style String
      deriving (Eq, Show)


data Document = Document Srcloc Element [Document]
                deriving (Show)


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


mkSection :: [Document] -> Document
mkSection = Document (0, [], "") Section


mkSpan :: String -> [Document] -> Document
mkSpan sty docs = Document (0, [], "") (Span sty) docs


isDocument :: Element -> Document -> Bool
isDocument t1 (Document _ t2 _) = t1 == t2


isEnumeration :: Document -> Bool
isEnumeration = isDocument Enumeration


isSection :: Document -> Bool
isSection = isDocument Section


isHeadingItem :: Document -> Bool
isHeadingItem = isDocument (Item HeadingT)


isParagraphItem :: Document -> Bool
isParagraphItem = isDocument (Item ParagraphT)


isUnorderedItem :: Document -> Bool
isUnorderedItem = isDocument (Item UnorderedT)