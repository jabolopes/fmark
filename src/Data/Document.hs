module Data.Document where

import Data.Char (isPunctuation)
import Data.List (intercalate)

import Data.Token


data ItemT
    = HeadingT
    | ParagraphT
    | UnorderedT
      deriving (Show)


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
      deriving (Show)


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


isEnumeration :: Document -> Bool
isEnumeration (Document _ Enumeration _) = True
isEnumeration _ = False


-- isItem :: Document -> Bool
-- isItem (Document _ (Item _) _) = True
-- isItem _ = False


isHeadingItem :: Document -> Bool
isHeadingItem (Document _ (Item HeadingT) _) = True
isHeadingItem _ = False


isParagraphItem :: Document -> Bool
isParagraphItem (Document _ (Item ParagraphT) _) = True
isParagraphItem _ = False


isUnorderedItem :: Document -> Bool
isUnorderedItem (Document _ (Item UnorderedT) _) = True
isUnorderedItem _ = False