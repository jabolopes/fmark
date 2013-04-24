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

import Debug.Trace


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
                , stack :: [[Either Char Document]]
                , isEmphasis :: Bool
                , isPlain :: Bool
                , isUnderline :: Bool }

type ParserM a = State ParserState a


skipM :: ParserM ()
skipM = modify $ \s -> s { input = tail (input s) }


shiftM :: ParserM ()
shiftM =
    do s <- get
       let c = head (input s)
           top = Left c:head (stack s)
       put s { input = tail (input s)
             , stack = top:tail (stack s) }


reduceM :: ([Either Char Document] -> Document) -> ParserM ()
reduceM fn =
    do stack <- stack <$> get
       let top = reverse (head stack)
           doc = fn top
           stack' = tail stack
           top' = head stack'
       modify $ \s -> s { stack = (Right doc:top'):tail stack' }


gotoM :: (String -> ParserM a) -> ParserM a
gotoM m = m =<< input <$> get


pushM :: ParserM ()
pushM = modify $ \s -> s { stack = []:stack s }


popM :: ParserM ()
popM =
    do stack <- stack <$> get
       let top = head stack
           stack' = tail stack
           top' = head stack'
       modify $ \s -> s { stack = (top ++ top'):tail stack' }


enterM
    :: (Bool -> ParserState -> ParserState)
    -> (String -> ParserM a)
    -> (String -> ParserM b)
    -> ParserM ()
enterM stateFn grammarM reduceActionM =
    do modify (stateFn True)
       pushM
       shiftM
       gotoM grammarM
       str <- input <$> get
       reduceActionM str
       modify (stateFn False)


mergeLefts :: [Either Char Document] -> [Either String Document]
mergeLefts = mergeLefts' . map (either (Left . (:[])) Right)
    where mergeLefts' :: [Either String Document] -> [Either String Document]
          mergeLefts' [] = []
          mergeLefts' (Left str1:Left str2:xs) =
              mergeLefts' (Left (str1 ++ str2):xs)
          mergeLefts' (x:xs) = x:mergeLefts' xs


reducePlainM :: ParserM ()
reducePlainM = reduceM reducePlain
    where reducePlain =
              mkSpan "plain" . map (either mkPlain id) . mergeLefts


reconstructEmphasisM :: a -> ParserM ()
reconstructEmphasisM _ =
    enterM stateFn grammarM reduceActionM
    where stateFn t s = s { isEmphasis = t }

          grammarM "" = return ()
          grammarM ('\'':_) = return ()
          grammarM ('_':_) =
              do underline <- isUnderline <$> get
                 unless underline $ do
                   gotoM reconstructUnderlineM
                   gotoM grammarM
          grammarM _ = shiftM >> gotoM grammarM

          reduceFn = mkSpan "emphasis" . map (either mkPlain id) . mergeLefts . tail

          reduceActionM "" = reducePlainM
          reduceActionM ('\'':_) = skipM >> reduceM reduceFn
          reduceActionM ('_':_) = popM


reconstructUnderlineM :: a -> ParserM ()
reconstructUnderlineM _ =
    enterM stateFn grammarM reduceActionM
    where stateFn t s = s { isUnderline = t }

          grammarM "" = return ()
          grammarM ('\'':_) =
              do emphasis <- isEmphasis <$> get
                 unless emphasis $ do
                   gotoM reconstructEmphasisM
                   gotoM grammarM
          grammarM ('_':_) = return ()
          grammarM _ = shiftM >> gotoM grammarM

          reduceFn =  mkSpan "underline" . map (either mkPlain id) . mergeLefts . tail

          reduceActionM "" = reducePlainM
          reduceActionM ('\'':_) = popM
          reduceActionM ('_':_) = skipM >> reduceM reduceFn


reconstructPlain :: String -> ParserM ()
reconstructPlain str
    | null str =
        do modify $ \s -> s { isPlain = False }
           reducePlainM
    | head str == '\'' =
        do emphasis <- isEmphasis <$> get
           if emphasis
           then do
             modify $ \s -> s { isPlain = False }
             popM
           else do
             gotoM reconstructEmphasisM
             gotoM reconstructPlain
    | head str == '_' =
        do underline <- isUnderline <$> get
           if underline
           then do
             modify $ \s -> s { isPlain = False }
             popM
           else do
             gotoM reconstructUnderlineM
             gotoM reconstructPlain
    | otherwise =
        do plain <- isPlain <$> get
           unless plain $ do
             modify $ \s -> s { isPlain = True }
             pushM
           shiftM
           gotoM reconstructPlain


reconstructM :: String -> ParserM ()
reconstructM str
    | null str =
        modify $ \s -> s { stack = reverse (head (stack s)):tail (stack s) }
    | head str == '\'' =
        do gotoM reconstructEmphasisM
           gotoM reconstructM
    | head str == '_' =
        do gotoM reconstructUnderlineM
           gotoM reconstructM
    | otherwise =
        do gotoM reconstructPlain
           gotoM reconstructM


-- 'reconstruct' @str@ produces the 'List' of 'Element's for 'String'
-- @str@.
--
-- Example
-- > How 'are' _you_?
--
-- > Span "plain"     "How "
-- > Span "emphasis"  "are"
-- > Span "plain"     " "
-- > Span "underline" "you"
-- > Span "plain"     "?"
reconstruct :: String -> [Document]
reconstruct str =
    let
        state = ParserState { input = str
                            , stack = []
                            , isEmphasis = False
                            , isPlain = False
                            , isUnderline = False }
        (_, state') = runState (pushM >> reconstructM str) state
    in
      case stack state' of
        [docs] -> map (either (error "reconstruct") id) docs
        x -> error $ "reconstruct " ++ show x


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