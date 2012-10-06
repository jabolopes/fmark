{-# LANGUAGE BangPatterns #-}
module Parser where

import Data.List (intercalate)

import Data.Document
import Data.Token


-- 'isParagraphItemLn' @str@ decides whether @str@ is a paragraph or
-- a heading.
isHeadingLn :: String -> Bool
isHeadingLn str = last str `notElem` paragraphTerminator
    where paragraphTerminator = ".!?"


isItemBlock :: Document -> Bool
isItemBlock (Document _ (Block ItemT) _) = True
isItemBlock _ = False


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
-- > string1 ...
-- > string2 ...
--
-- > Heading
-- >  Doc (string1 ...)
-- >  Doc (string2 ...)
--
-- Example
-- > string1 ...
-- > string2 ... '.'
--
-- > Paragraph
-- >  Doc (string1 ... ' ' string2 ... '.')
spanify :: [String] -> Document
spanify lns
    | all isHeadingLn lns = mkHeading $ map (Document (0, [], "") (Span "line") . reconstruct) lns
    | otherwise = mkParagraph $ reconstruct $ intercalate " " lns


-- Example
-- > string1 ...
-- > string2 ...
-- >
-- > string3 ...
-- > string4 ... '.'
--
-- > Heading
-- >  Doc (string1 ...)
-- >  Doc (string2 ...)
-- > Paragraph
-- >  Doc (string3 ... ' ' string4 ... '.')
--
-- Example
-- > string1 ...
-- > string2 ...
-- > Block ItemT
-- >  string3 ...
-- >  string4 ...
-- >
-- >  string5 ...
-- >  string6 ... '.'
-- > Block ItemT
-- >  string7 ...
-- >  string8 ... '.'
--
-- > Heading
-- >  Doc (string1 ...)
-- >  Doc (string2 ...)
-- > Enumeration
-- >  Block ItemT
-- >   Heading
-- >    Doc (string3 ...)
-- >    Doc (string4 ...)
-- >   Paragraph
-- >    Doc (string5 ... ' ' string6 ... '.')
-- >  Block ItemT
-- >   Paragraph
-- >    Doc (string7 ... ' ' string8 ... '.')
blockify :: [Either Srcloc Document] -> [Document]
blockify [] = []
blockify es@(Left loc:_) =
    let (locs', docs) = span (either (const True) (const False)) es in
    spanify (map (\(Left (_, _, str)) -> str) locs'):blockify docs
blockify es@(Right doc:_) | isItemBlock doc =
    let (items, locs) = span (either (const False) isItemBlock) es in
    mkEnumeration (map (\(Right doc) -> doc) items):blockify locs
blockify (Right doc:docs) = doc:blockify docs


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks = fst $ docify' SectionT tks [] []
    where 
          shift :: BlockT -> Srcloc -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          shift sty loc tks locs docs =
              docify' sty tks (Left loc:locs) docs

          pushPop :: BlockT -> BlockT -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          pushPop sty1 sty2 tks locs docs =
              let (doc, tks') = docify' sty2 tks [] [] in
              docify' sty1 tks' (Right doc:locs) docs
          
          reduceEmpty :: BlockT -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          reduceEmpty sty tks locs docs =
              let docs' = blockify $ reverse locs in
              docify' sty tks [] (reverse docs' ++ docs)
          
          reduceSection :: BlockT -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          reduceSection sty tks locs docs =
              let docs' = blockify $ reverse locs in
              (mkBlock sty $ reverse docs ++ docs', tks)

          sectionT :: Char -> BlockT
          sectionT '*' = ItemT
          sectionT '"' = QuotationT
          sectionT '|' = SectionT
          sectionT '>' = VerbatimT

          docify' :: BlockT -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          docify' _ [] [] docs = (mkContent $ reverse docs, [])
          docify' sty [] locs docs = reduceEmpty sty [] locs docs
          docify' sty (Literal loc:tks) locs docs = shift sty loc tks locs docs
          docify' sty (BeginSection t:tks) locs docs = pushPop sty (sectionT t) tks locs docs
          docify' sty (EndSection:tks) locs docs = reduceSection sty tks locs docs
          docify' sty (Empty:tks) locs docs = reduceEmpty sty tks locs docs