module Data.Document where

import Data.Token (Srcloc)


data BlockT
    = BulletItemT
    | NumberItemT Int
    | QuotationT
    | SectionT
    | VerbatimT

instance Show BlockT where
    show BulletItemT = "unordered"
    show (NumberItemT i) = "ordered " ++ show i
    show QuotationT = "quotation"
    show SectionT = "section"
    show VerbatimT = "verbatim"


data EnumerationT
    = BulletEnumerationT
    | NumberEnumerationT
      deriving (Show)


data Element
    = Block BlockT
    | Content
    | Enumeration EnumerationT
    | Heading
    | Paragraph
    | Plain String
    | Span String
      deriving (Show)


isParagraphElement :: Element -> Bool
isParagraphElement Paragraph = True
isParagraphElement _ = False


isSpanElement :: Element -> Bool
isSpanElement (Span _) = True
isSpanElement _ = False


data Document = Document Srcloc Element [Document]
                deriving (Show)


mkDocument :: Element -> [Document] -> Document
mkDocument = Document (0, [], "")


mkBlock :: BlockT -> [Document] -> Document
mkBlock t docs = Document (0, [], "") (Block t) docs


mkContent :: [Document] -> Document
mkContent = Document (0, [], "") Content


mkEnumeration :: EnumerationT -> [Document] -> Document
mkEnumeration t = Document (0, [], "") (Enumeration t)


mkHeading :: [Document] -> Document
mkHeading = Document (0, [], "") Heading


mkParagraph :: [Document] -> Document
mkParagraph = Document (0, [], "") Paragraph


mkPlain :: String -> Document
mkPlain str = Document (0, [], "") (Plain str) []


mkSpan :: String -> [Document] -> Document
mkSpan sty docs = Document (0, [], "") (Span sty) docs


isBlock :: Document -> Bool
isBlock (Document _ (Block _) _) = True
isBlock _ = False