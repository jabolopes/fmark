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


rangeLoc :: [Document] -> (Srcloc, Srcloc)
rangeLoc docs =
    let
        Document { beginLoc = loc1 } = head docs
        Document { endLoc = loc2 } = last docs
    in
      (loc1, loc2)


mkBlock :: BlockT -> [Document] -> Document
mkBlock t docs =
    let (loc1, loc2) = rangeLoc docs in
    Document loc1 loc2 (Block t) docs


mkContent :: [Document] -> Document
mkContent docs =
    let (loc1, loc2) = rangeLoc docs in
    Document loc1 loc2 Content docs


mkEnumeration :: EnumerationT -> [Document] -> Document
mkEnumeration t docs =
    let (loc1, loc2) = rangeLoc docs in
    Document loc1 loc2 (Enumeration t) docs


mkHeading :: [Document] -> Document
mkHeading docs =
    let (loc1, loc2) = rangeLoc docs in
    Document loc1 loc2 Heading docs


mkParagraph :: [Document] -> Document
mkParagraph docs =
    let (loc1, loc2) = rangeLoc docs in
    Document loc1 loc2 Paragraph docs


mkPlain :: Srcloc -> Srcloc -> String -> Document
mkPlain loc1 loc2 str = Document loc1 loc2 (Plain str) []


mkSpan :: Srcloc -> Srcloc -> String -> [Document] -> Document
mkSpan loc1 loc2 sty docs = Document loc1 loc2 (Span sty) docs