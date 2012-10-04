{-# LANGUAGE BangPatterns #-}
module Parser where

import Data.Document
import Data.Token


-- 'isParagraphItemLn' @str@ decides whether @str@ is a paragraph or
-- a heading.
isHeadingItemLn :: String -> Bool
isHeadingItemLn str = last str `notElem` paragraphTerminator
    where paragraphTerminator = ".!?"


isUnorderedItemLn :: String -> Bool
isUnorderedItemLn ('*':' ':_) = True
isUnorderedItemLn _ = False


isEnumOrUnorderedItem :: Document -> Bool
isEnumOrUnorderedItem doc = isEnumeration doc || isUnorderedItem doc


-- 'reconstruct' @str@ produces the 'List' of 'Text' elements
-- for 'String' @str@.
reconstruct :: String -> [Document]
reconstruct = reconstructFirst
    where spanChar c sty str =
              case span' str of
                (hd, tl, True) -> mkSpan sty [mkPlain hd]:reconstructTail tl
                (hd, tl, _) -> [mkPlain (c:hd)]
              where span' str =
                        case span (/= c) str of
                          (hd, []) -> (hd, "", False)
                          (hd, [c']) | c == c' -> (hd, "", True)
                          (hd, c':' ':tl) | c == c' -> (hd, ' ':tl, True)
                          -- info: '_' has to be equal to 'c'
                          (hd, _:tl) -> let (hd', tl', b) = span' tl in
                                        (hd ++ [c] ++ hd', tl', b)

          spanStarters = "'_"
          spanStyle '\'' = "emphasis"
          spanStyle '_' = "underline"

          plain str =
              case span (/= ' ') str of
                ([], c:tl) -> mkPlain [c]:reconstructTail tl
                (hd, tl) -> mkPlain hd:reconstructTail tl

          reconstructFirst "" = []
          reconstructFirst (c:str) | c `elem` spanStarters = spanChar c (spanStyle c) str
          reconstructFirst str = plain str

          reconstructTail "" = []
          reconstructTail (' ':str) = mkPlain " ":reconstructFirst str
          reconstructTail str = plain str


          -- block sty = "[" ++ sty ++ " "

          -- mkText c fn str =
          --     case span (/= c) str of
          --       (hd, []) -> [Plain hd]
          --       (hd, _:tl) -> fn hd:reconstructTail tl

          -- isSpan sty str = take (length (block sty)) str == (block sty)

          -- mkSpanElement sty str =
          --     case span (/= ']') (drop (length (block sty)) str) of
          --       (hd, []) -> [mkPlain hd]
          --       (hd, _:tl) -> mkSpan sty (reconstructTail hd):reconstructTail tl


          -- reconstructTail str | isSpan "bold" str = mkSpanElement "bold" str
          --                 | isSpan "italic" str = mkSpanElement "italic" str
          --                 | isSpan "underline" str = mkSpanElement "underline" str
          --                 | isSpan "footnote" str = mkSpanElement "footnote" str
          --                 | isSpan "cite" str = mkSpanElement "cite" str
          -- reconstructTail ('[':str) = mkText ']' Ref str
          -- edit: don't capture quotes in words, e.g., "don't" and "can't"
          -- reconstructTail ('\'':str) = spanChar '\'' "emphasis" str


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
blockify locs = restructure $ map spanifyEither locs
    where spanify ln | isUnorderedItemLn ln = mkItem UnorderedT $ reconstruct $ drop 2 ln
                     | isHeadingItemLn ln = mkItem HeadingT $ reconstruct ln
                     | otherwise = mkItem ParagraphT $ reconstruct ln

          spanifyEither (Left (_, _, str)) = spanify str
          spanifyEither (Right doc) = doc

          enumerate [] = []
          enumerate docs@(item:_) | isEnumOrUnorderedItem item =
              let (items, docs') = span isEnumOrUnorderedItem docs in
              mkEnumeration items:enumerate docs'
          enumerate (doc:docs) = doc:enumerate docs

          -- restructure [doc] | isSection doc || isEnumeration doc = doc
          restructure [doc] | isBlock doc || isEnumeration doc = doc
          restructure docs | all isEnumOrUnorderedItem docs = mkEnumeration docs
          restructure docs | all isHeadingItem docs = mkHeading docs
          restructure docs = mkParagraph $ enumerate docs


-- Example
-- > Enumeration
-- >
-- > Enumeration
--
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
-- > ...
-- >
-- > Section
-- >  Enumeration
-- >   Item ...
-- >   Enumeration ...
-- >  ...
sectionify _ [doc] | isEnumeration doc = doc
sectionify _ docs | all isEnumOrUnorderedItem docs && any isUnorderedItem docs = mkEnumeration docs
sectionify sty docs = mkBlock sty docs


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks = fst $ docify' "section" tks [] []
    where 
          shift :: String -> Srcloc -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          shift sty loc tks locs docs =
              docify' sty tks (Left loc:locs) docs


          pushPop :: String -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          pushPop sty tks locs docs =
              let (doc, tks') = docify' sty tks [] [] in
              docify' sty tks' (Right doc:locs) docs

          
          reduceEmpty :: String -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          reduceEmpty sty tks locs docs =
              let doc = blockify $ reverse locs in
              docify' sty tks [] (doc:docs)

          
          reduceSection :: String -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          reduceSection sty tks locs docs =
              let doc = blockify $ reverse locs in
              (sectionify sty $ reverse $ doc:docs, tks)


          sectionT '"' = "quotation"
          sectionT '|' = "section"
          sectionT '>' = "verbatim"
          sectionT c = show c

          docify' :: String -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          docify' _ [] [] docs = (mkContent $ reverse docs, [])
          docify' sty [] locs docs = reduceEmpty sty [] locs docs
          docify' sty (Literal loc:tks) locs docs = shift sty loc tks locs docs
          docify' _ (BeginSection t:tks) locs docs = pushPop (sectionT t) tks locs docs
          docify' sty (EndSection:tks) locs docs = reduceSection sty tks locs docs
          docify' sty (Empty:tks) locs docs = reduceEmpty sty tks locs docs