module Data.Document where

import Data.Token (Srcloc)


data BlockT
    = ItemT
    | QuotationT
    | SectionT
    | VerbatimT
      deriving (Eq)

instance Show BlockT where
    show ItemT = "item"
    show QuotationT = "quotation"
    show SectionT = "section"
    show VerbatimT = "verbatim"


data Element
    = Block BlockT
    | Content
    | Enumeration
    | Heading
    | Paragraph
    | Plain String
    | Span String
      deriving (Eq, Show)


data Document = Document Srcloc Element [Document]
                deriving (Show)


mkDocument :: Element -> [Document] -> Document
mkDocument = Document (0, [], "")


mkBlock :: BlockT -> [Document] -> Document
mkBlock t docs = Document (0, [], "") (Block t) docs


mkContent :: [Document] -> Document
mkContent = Document (0, [], "") Content


mkEnumeration :: [Document] -> Document
mkEnumeration = Document (0, [], "") Enumeration


mkHeading :: [Document] -> Document
mkHeading = Document (0, [], "") Heading


mkParagraph :: [Document] -> Document
mkParagraph = Document (0, [], "") Paragraph


mkPlain :: String -> Document
mkPlain str = Document (0, [], "") (Plain str) []


mkSpan :: String -> [Document] -> Document
mkSpan sty docs = Document (0, [], "") (Span sty) docs


isDocument :: Element -> Document -> Bool
isDocument t1 (Document _ t2 _) = t1 == t2


isBlock :: Document -> Bool
isBlock (Document _ (Block _) _) = True
isBlock _ = False