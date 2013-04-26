module Data.Document where

import Data.Token (Srcloc)


data BlockT
    = BulletItemT
    | NumberItemT Int
    | QuotationT
    | SectionT
    | VerbatimT
      deriving (Eq)

instance Show BlockT where
    show BulletItemT = "unordered"
    show (NumberItemT i) = "ordered " ++ show i
    show QuotationT = "quotation"
    show SectionT = "section"
    show VerbatimT = "verbatim"


data EnumerationT
    = BulletEnumerationT
    | NumberEnumerationT
      deriving (Eq, Show)


data Element
    = Block BlockT
    | Content
    | Enumeration EnumerationT
    | Heading
    | Paragraph
    | Plain String
    | Span String
      deriving (Eq, Show)


isParagraphElement :: Element -> Bool
isParagraphElement Paragraph = True
isParagraphElement _ = False


isSpanElement :: Element -> Bool
isSpanElement (Span _) = True
isSpanElement _ = False


data Document
    = Document { beginLoc :: Srcloc
               , endLoc :: Srcloc
               , element :: Element
               , children :: [Document] }
                deriving (Show)


mkDocument :: Element -> [Document] -> Document
mkDocument = Document (0, [], "") (0, [], "")


mkBlock :: BlockT -> [Document] -> Document
mkBlock t docs = mkDocument (Block t) docs


mkContent :: [Document] -> Document
mkContent = mkDocument Content


mkEnumeration :: EnumerationT -> [Document] -> Document
mkEnumeration t = mkDocument (Enumeration t)


mkHeading :: [Document] -> Document
mkHeading = mkDocument Heading


mkParagraph :: Srcloc -> Srcloc -> [Document] -> Document
mkParagraph loc1 loc2 = Document loc1 loc2 Paragraph


mkPlain :: Srcloc -> Srcloc -> String -> Document
mkPlain loc1 loc2 str = Document loc1 loc2 (Plain str) []


mkSpan :: Srcloc -> Srcloc -> String -> [Document] -> Document
mkSpan loc1 loc2 sty docs = Document loc1 loc2 (Span sty) docs


isBlock :: Document -> Bool
isBlock Document { element = Block _ } = True
isBlock _ = False


isHeading :: Document -> Bool
isHeading Document { element = Heading } = True
isHeading _ = False