{-# LANGUAGE BangPatterns #-}
module Parser where

import Data.Char
import Data.List (intercalate)

import Data.Document
import Data.Token
import Utils


-- 'isParagraphItemLn' @str@ decides whether @str@ is a heading or a
-- paragraph.
isHeadingLn :: String -> Bool
isHeadingLn str = last str `notElem` paragraphTerminator
    where paragraphTerminator = ".!?"


-- 'isBulletItem' @doc@
isBulletItem :: Document -> Bool
isBulletItem (Document _ (Block BulletItemT) _) = True
isBulletItem _ = False


interleaveAppend :: String -> String -> String
interleaveAppend xs1 [] = xs1
interleaveAppend [] xs2 = xs2
interleaveAppend xs1 xs2 = xs1 ++ " " ++ xs2


spanQuote :: String -> (String, String)
spanQuote str =
    case span (/= '\'') str of
      (hd, "") -> ("", hd)
      (hd, '\'':tl)
          | null tl -> (hd, "")
          | isSpace (head tl) -> (hd, tl)
          | otherwise -> case spanQuote tl of
                           ("", _) -> (hd, tl)
                           (hd', tl') -> (hd ++ "'" ++ hd', tl')


spanUnderline :: String -> (String, String)
spanUnderline str =
    case span ((/= '_') &&. (not . isSpace)) str of
      (hd, "") -> ("", hd)
      (hd, "_") -> (hd, "")

      (hd, c1:c2:tl) | c1 == '_' && c2 == '_' -> (hd, '_':tl)

      (hd, '_':tl)
          | isAlphaNum (head tl) || head tl == '\'' ->
              let (hd', tl') = spanUnderline tl in
              (interleaveAppend hd hd', tl')
          | otherwise -> (hd, tl)

      (hd, tl)
          | isSpace (head tl) -> ("", str)


spanPlain :: String -> (String, String)
spanPlain str =
    case span ((not . isSpace) &&. (/= '_')) str of
      (hd, "") -> (hd, "")

      (hd, c:tl)
          | isSpace c && head tl == '\'' ->
              case spanQuote (tail tl) of
                ("", _) -> (str, "")
                _ -> (hd ++ [c], tl)
          | isSpace c && head tl == '_' ->
              case spanUnderline (tail tl) of
                ("", _) -> (str, "")
                _ -> (hd ++ [c], tl)
          | isSpace c ->
              let (hd', tl') = spanPlain tl in
              (hd ++ [c] ++ hd', tl')

      (hd, tl)
          | head tl == '_' ->
              case spanUnderline (tail tl) of
                ("", _) -> (str, "")
                _ -> (hd, tl)


-- Example
-- > _hello_world_
--
-- > ("hello world", "")
--
-- Example
-- > _hello_world
--
-- > ("hello", "world")
--
-- Example
-- > _hello world
--
-- > ("", "_hello world")
--
-- Example
-- > _hello_world_, goodbye
--
-- > ("hello world", ", goodbye")
--
-- Example
-- > _hello_world, goodbye
--
-- > ("hello", "world, goodbye")
--
-- Example
-- > _hello, world goodbye
--
-- > ("", "_hello, world goodbye")
--
-- Example
-- > 'hello' 'world
--
-- > ("hello", " 'world")
spanChar :: String -> (String, String)
spanChar (c:str) =
    case findFn c str of
      ([], tl) -> ([], c:tl)
      (hd, tl) -> (hd, tl)
    where findFn '\'' = spanQuote
          findFn '_' = spanUnderline


spanStarters :: String
spanStarters = "'_"


reconstructStyle :: String -> [Document]
reconstructStyle str@(c:_) =
    case spanChar str of
      ([], _) -> reconstructPlain str
      (hd, tl) -> mkSpan (spanStyle c) [mkPlain hd]:reconstruct tl
    where spanStyle '\'' = "emphasis"
          spanStyle '_' = "underline"


reconstructPlain :: String -> [Document]
reconstructPlain str =
    case spanPlain str of
      (hd, tl) -> mkPlain hd:reconstruct tl


-- 'reconstruct' @str@ produces the 'List' of 'Text' elements
-- for 'String' @str@.
--
-- Example
-- > How 'are' _you_?
--
-- > Plain "How "
-- > Span "emphasis" "are"
-- > Plain " "
-- > Span "underline" "you"
-- > Plain "?"
reconstruct :: String -> [Document]
reconstruct str
    | null str = []
    | head str `elem` spanStarters = reconstructStyle str
    | otherwise = reconstructPlain str


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

blockify es@(Left _:_) =
    let (locs, docs) = span (either (const True) (const False)) es in
    spanify (map (\(Left (_, _, str)) -> str) locs):blockify docs

blockify es@(Right doc:_) | isBulletItem doc =
    let  (items, locs) = span (either (const False) isBulletItem) es in
    mkEnumeration BulletEnumerationT (map (\(Right doc) -> doc) items):blockify locs

blockify (Right item@(Document _ (Block (NumberItemT n)) _):es) =
    let (items, locs) = spanOrderedItems n es in
    mkEnumeration NumberEnumerationT (item:items):blockify locs
        where spanOrderedItems _ xs@[] =  ([], xs)
              spanOrderedItems n1 xs@(Right x@(Document _ (Block (NumberItemT n2)) _):xs')
                  | n1 == n2 - 1 = let (ys,zs) = spanOrderedItems n2 xs' in (x:ys,zs)
                  | otherwise = ([], xs)
              spanOrderedItems _ xs = ([], xs)

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

          sectionT :: String -> BlockT
          sectionT "*" = BulletItemT
          sectionT "\"" = QuotationT
          sectionT "|" = SectionT
          sectionT ">" = VerbatimT
          sectionT ds = NumberItemT (read ds :: Int)

          docify' :: BlockT -> [Token] -> [Either Srcloc Document] -> [Document] -> (Document, [Token])
          docify' _ [] [] docs = (mkContent $ reverse docs, [])
          docify' sty [] locs docs = reduceEmpty sty [] locs docs
          docify' sty (Literal loc:tks) locs docs = shift sty loc tks locs docs
          docify' sty (BeginSection t:tks) locs docs = pushPop sty (sectionT t) tks locs docs
          docify' sty (EndSection:tks) locs docs = reduceSection sty tks locs docs
          docify' sty (Empty:tks) locs docs = reduceEmpty sty tks locs docs