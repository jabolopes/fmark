module Data.Document where

import Data.Char (isPunctuation)
import Data.List (intercalate)

import Data.Token


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
mkContent = Document (0, [], "") Content


mkEnumeration :: [Document] -> Document
mkEnumeration = Document (0, [], "") Enumeration


mkHeading :: [Document] -> Document
mkHeading = Document (0, [], "") Heading


mkItem :: [Document] -> Document
mkItem = Document (0, [], "") Item


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


isItem :: Document -> Bool
isItem (Document _ Item _) = True
isItem _ = False