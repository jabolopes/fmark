{-# LANGUAGE BangPatterns #-}
module Parser where

import Control.Monad
import Data.Functor ((<$>))
import Data.Char (isPunctuation, isSpace)
import Data.List (intercalate)

import Data.Document
import Data.Token
import Utils

import Debug.Trace


isEmptyLn :: String -> Bool
isEmptyLn = all isSpace


-- 'isParagraphItemLn' @str@ decides whether @str@ is a paragraph or
-- a heading.
isParagraphItemLn :: String -> Bool
isParagraphItemLn str = last str `elem` paragraphTerminator
    where paragraphTerminator = ".!?"


isUnorderedItemLn :: String -> Bool
isUnorderedItemLn ('*':' ':_) = True
isUnorderedItemLn _ = False


-- | 'section' @idns idn@ is the 'List' of 'BeginSection' and
-- 'EndSection' 'Token's issued according the indentation stack @idns@
-- and current indentation @idn@.
section :: [Int] -> Int -> [Token]
section [] _ = error "section: idns is empty"
section (idn1:idns) idn2 =
    case compare idn1 idn2 of
      EQ -> []
      LT -> [BeginSection]
      GT -> EndSection:section (dropWhile (> idn1) idns) idn2


tokenize :: Int -> [Int] -> String -> Token
tokenize n idns ln = Literal (n, idns, ln)


-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> [Token]
reduce idns n ln = section idns idn ++ [tokenize n (push idn idns) $ trim ln]
    where idn = indentation ln


-- | 'classify' @str@ is the 'List' of 'Token's of @str@.
classify :: String -> [Token]
classify str = classify' [0] $ zip [1..] $ lines str
    where classify' :: [Int] -> [(Int, String)] -> [Token]
          classify' idns [] = replicate (length idns - 1) EndSection
          classify' idns ((_, ln):lns) | isEmptyLn ln = classify' idns lns
          classify' idns [(n, ln)] = reduce idns n ln ++ classify' (push (indentation ln) idns) []
          classify' idns ((n1, ln1):(n2, ln2):lns)
              | isEmptyLn ln2 =
                  let
                      ln1' = reduce idns n1 ln1
                      lns' = classify' (push idn1 idns) lns
                  in
                    case lns' of
                      [] -> ln1'
                      EndSection:lns'' -> ln1' ++ [EndSection, Empty] ++ lns''
                      _ -> ln1' ++ [Empty] ++ lns'
              | idn1 < idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              | idn1 > idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              | otherwise = reduce idns n1 ln1 ++ classify' (push idn2 idns) ((n2, ln2):lns)
              where idn1 = indentation ln1
                    idn2 = indentation ln2


-- 'reconstruct' @str@ produces the 'List' of 'Text' elements
-- for 'String' @str@.
reconstruct :: String -> [Document]
reconstruct = reconstruct'
    where block sty = "[" ++ sty ++ " "

          -- mkText c fn str =
          --     case span (/= c) str of
          --       (hd, []) -> [Plain hd]
          --       (hd, _:tl) -> fn hd:reconstruct' tl

          spanChar :: Char -> String -> String -> [Document]
          spanChar c sty str =
              case span (/= c) str of
                (hd, []) -> [mkPlain hd]
                (hd, _:tl) -> mkSpan sty [mkPlain hd]:reconstruct' tl

          -- isSpan sty str = take (length (block sty)) str == (block sty)

          -- mkSpanElement sty str =
          --     case span (/= ']') (drop (length (block sty)) str) of
          --       (hd, []) -> [mkPlain hd]
          --       (hd, _:tl) -> mkSpan sty (reconstruct' hd):reconstruct' tl


          reconstruct' :: String -> [Document]
          reconstruct' "" = []
          -- reconstruct' str | isSpan "bold" str = mkSpanElement "bold" str
          --                 | isSpan "italic" str = mkSpanElement "italic" str
          --                 | isSpan "underline" str = mkSpanElement "underline" str
          --                 | isSpan "footnote" str = mkSpanElement "footnote" str
          --                 | isSpan "cite" str = mkSpanElement "cite" str
          -- reconstruct' ('[':str) = mkText ']' Ref str
          -- edit: don't capture quotes in words, e.g., "don't" and "can't"
          reconstruct' ('\'':str) = spanChar '\'' "emphasis" str
          reconstruct' ('_':str) = spanChar '_' "underline" str
          reconstruct' str =
              mkPlain hd:reconstruct' tl
              where (hd, tl) = span (`notElem` "['_") str


isEnumOrUnorderedItem :: Document -> Bool
isEnumOrUnorderedItem = (||) `fmap` isEnumeration `ap` isUnorderedItem


-- Example
-- > * ...
-- > Enumeration ...
--
-- > Enumeration
-- >  Item ...
-- >  Enumeration ...
--
-- Example
-- > * ...
-- > Enumeration ...
-- > ... .
-- >
-- > Paragraph
-- >  Enumeration
-- >   Item ...
-- >   Enumeration ...
-- >  Content ... .
--
-- Example
-- > * ...
-- > Enumeration ...
-- > ...
-- >
-- > Heading
-- >  Enumeration
-- >   Item ...
-- >   Enumeration ...
-- >  Content ...
blockify :: [Either Srcloc Document] -> Document
blockify locs =
    restructure $ map spanifyEither locs
    where spanify ln | isUnorderedItemLn ln = mkItem UnorderedT $ reconstruct $ drop 2 ln
                     | isParagraphItemLn ln = mkItem ParagraphT $ reconstruct $ drop 2 ln
                     | otherwise = mkItem HeadingT $ reconstruct $ drop 2 ln

          spanifyEither (Left (_, _, str)) = spanify str
          spanifyEither (Right doc) = doc

          restructure docs | all isEnumOrUnorderedItem docs = mkEnumeration docs
          restructure docs | all isHeadingItem docs = mkHeading docs
          restructure [doc] | isSection doc = doc
          restructure docs = mkParagraph $ enumerate docs

          enumerate [] = []
          enumerate docs@(item:_) | isEnumOrUnorderedItem item =
              let (items, docs') = span isEnumOrUnorderedItem docs in
              mkEnumeration items:enumerate docs'
          enumerate (doc:docs) = doc:enumerate docs


-- Example
-- > Item ...
-- > Enumeration ...
--
-- > Enumeration
-- >  Item ...
-- >  Enumeration ...
--
-- Example
-- > Item ...
-- > Enumeration ...
-- > Content ...
-- >
-- > Section
-- >  Enumeration
-- >   Item ...
-- >   Enumeration ...
-- >  Content ...
sectionify [doc] | isEnumeration doc = doc
sectionify docs | all isEnumOrUnorderedItem docs = mkEnumeration docs
sectionify docs = mkSection docs


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks = fst $ docify' tks [] []
    where 
          shift :: Srcloc -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          shift loc tks locs docs =
              docify' tks (Left loc:locs) docs


          pushPop :: [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          pushPop tks locs docs =
              let (doc, tks') = docify' tks [] [] in
              docify' tks' (Right doc:locs) docs

          
          reduceEmpty :: [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          reduceEmpty tks locs docs =
              let doc = blockify $ reverse locs in
              docify' tks [] (doc:docs)

          
          reduceSection :: [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          reduceSection tks locs docs =
              let doc = blockify $ reverse locs in
              (sectionify $ reverse $ doc:docs, tks)


          docify' :: [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          docify' [] [] docs = (mkContent $ reverse docs, [])
          docify' [] locs docs = reduceEmpty [] locs docs
          docify' (Literal loc:tks) locs docs = shift loc tks locs docs
          docify' (BeginSection:tks) locs docs = pushPop tks locs docs
          docify' (EndSection:tks) locs docs = reduceSection tks locs docs
          docify' (Empty:tks) locs docs = reduceEmpty tks locs docs