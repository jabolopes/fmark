{-# LANGUAGE BangPatterns #-}
module Parser where

import Control.Monad.State
import Data.Char
import Data.Either (either)
import Data.Functor ((<$>))
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


data ParserState =
    ParserState { input :: String
                , stack :: [Either String Document]
                , isEmphasis :: Bool
                , isUnderline :: Bool }

type ParserM a = State ParserState a


skipM :: ParserM ()
skipM = modify $ \s -> s { input = tail (input s) }


shiftM :: ParserM ()
shiftM =
    do s <- get
       let c = head (input s)
       case head (stack s) of
         Left str -> put s { input = tail (input s)
                           , stack = Left (c:str):tail (stack s) }
         Right _ -> put s { input = tail (input s)
                          , stack = Left [c]:stack s }


gotoM :: (String -> ParserM a) -> ParserM a
gotoM m = m =<< input <$> get


reduceEmphasisM :: ParserM ()
reduceEmphasisM =
    do s <- get
       let sty = case head (stack s) of
                   Left str -> mkSpan "emphasis" [mkPlain (tail (reverse str))]
                   Right doc -> mkSpan "emphasis" [doc]
       put s { stack = Right sty:tail (stack s) }


reduceUnderlineM :: ParserM ()
reduceUnderlineM =
    do s <- get
       let sty = case head (stack s) of
                   Left str -> mkSpan "underline" [mkPlain (tail (reverse str))]
                   Right doc -> mkSpan "underline" [doc]
       put s { stack = Right sty:tail (stack s) }


reducePlainM :: ParserM ()
reducePlainM =
    do s <- get
       let sty = case head (stack s) of
                   Left str -> mkPlain (reverse str)
                   Right doc -> error "reducePlainM"
       put s { stack = Right sty:tail (stack s) }


reconstructEmphasis :: String -> ParserM ()
reconstructEmphasis str
    | null str = reducePlainM
    | head str == '\'' =
        do skipM
           reduceEmphasisM
    | head str == '_' =
        do underline <- isUnderline <$> get
           unless underline $ do
             shiftM
             reconstructUnderlineM
             reconstructEmphasisM
    | otherwise =
        do shiftM
           reconstructEmphasisM


reconstructUnderline :: String -> ParserM ()
reconstructUnderline str
    | null str = reducePlainM
    | head str == '\'' =
        do emphasis <- isEmphasis <$> get
           unless emphasis $ do
             shiftM
             reconstructEmphasisM
             reconstructUnderlineM
    | head str == '_' =
        do skipM
           reduceUnderlineM
    | otherwise =
        do shiftM
           reconstructUnderlineM


reconstructEmphasisM :: ParserM ()
reconstructEmphasisM =
    do modify $ \s -> s { isEmphasis = True }
       gotoM reconstructEmphasis
       modify $ \s -> s { isEmphasis = False }


reconstructUnderlineM :: ParserM ()
reconstructUnderlineM =
    do modify $ \s -> s { isUnderline = True }
       gotoM reconstructUnderline
       modify $ \s -> s { isUnderline = False }


reconstructM :: String -> ParserM ()
reconstructM str
    | null str = reducePlainM
    | head str == '\'' = shiftM >> reconstructEmphasisM
    | head str == '_' = shiftM >> reconstructUnderlineM
    | otherwise = shiftM >> gotoM reconstructM


reconstruct :: String -> [Document]
reconstruct str =
    let (_, s) = runState (gotoM reconstructM) ParserState { input = str
                                                           , stack = [Left ""]
                                                           , isEmphasis = False
                                                           , isUnderline = False } in
    [either (error "reconstruct") id (head (stack s))]


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