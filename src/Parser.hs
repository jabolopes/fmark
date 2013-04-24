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
                , stack :: [[Either String Document]]
                , isEmphasis :: Bool
                , isPlain :: Bool
                , isUnderline :: Bool }

type ParserM a = State ParserState a


skipM :: ParserM ()
skipM = modify $ \s -> s { input = tail (input s) }


shiftM :: ParserM ()
shiftM =
    do s <- get
       when (null (head (stack s))) $ do
         modify $ \s -> s { stack = [Left ""]:tail (stack s) }

       s <- get
       let c = head (input s)
       case head $ head $ stack s of
         Left str -> do let top = head (stack s)
                            top' = Left (c:str):tail top
                            stack' = top':tail (stack s)
                        put s { input = tail (input s)
                              , stack = stack' }
         Right _ -> do let top = head (stack s)
                           top' = Left [c]:top
                           stack' = top':tail (stack s)
                       put s { input = tail (input s)
                             , stack = stack' }


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
       modify $ \s -> s { stack = (appendLefts top top'):tail stack' }
    where appendLefts [Left str1] (Left str2:ys) =
              Left (str1 ++ str2):ys
          appendLefts [] ys = ys
          appendLefts (x:xs) ys = x:appendLefts xs ys


reduceM :: ([Either String Document] -> Document) -> ParserM ()
reduceM fn =
    do stack <- stack <$> get
       let top = reverse (head stack)
           doc = fn top
           stack' = tail stack
           top' = head stack'
       modify $ \s -> s { stack = (Right doc:top'):tail stack' }


reduceEmphasisM :: ParserM ()
reduceEmphasisM = reduceM reduceEmphasis
    where reduceEmphasis =
              mkSpan "emphasis" . map (either (mkPlain . tail . reverse) id)


reduceUnderlineM :: ParserM ()
reduceUnderlineM = reduceM reduceUnderline
    where reduceUnderline =
              mkSpan "underline" . map (either (mkPlain . tail . reverse) id)


reducePlainM :: ParserM ()
reducePlainM = reduceM reducePlain
    where reducePlain =
              mkSpan "plain" . map (either (mkPlain . reverse) id)


reconstructEmphasis :: String -> ParserM ()
reconstructEmphasis str
    | null str =
        do modify $ \s -> s { isEmphasis = False }
           reducePlainM
    | head str == '\'' =
        do emphasis <- isEmphasis <$> get
           if emphasis
           then do
             modify $ \s -> s { isEmphasis = False }
             skipM
             reduceEmphasisM
           else do
             modify $ \s -> s { isEmphasis = True }
             pushM
             shiftM
             gotoM reconstructEmphasis
    | head str == '_' =
        do underline <- isUnderline <$> get
           unless underline $ do
             gotoM reconstructUnderline
             gotoM reconstructEmphasis
    | otherwise =
        do shiftM
           gotoM reconstructEmphasis


reconstructUnderline :: String -> ParserM ()
reconstructUnderline str
    | null str =
        do modify $ \s -> s { isUnderline = False }
           reducePlainM
    | head str == '\'' =
        do emphasis <- isEmphasis <$> get
           if emphasis
           then do
             modify $ \s -> s { isUnderline = False }
             popM
           else do
             gotoM reconstructEmphasis
             gotoM reconstructUnderline
    | head str == '_' =
        do underline <- isUnderline <$> get
           if underline
           then do
             modify $ \s -> s { isUnderline = False }
             skipM
             reduceUnderlineM
           else do
             modify $ \s -> s { isUnderline = True }
             pushM
             shiftM
             gotoM reconstructUnderline
    | otherwise =
        do shiftM
           gotoM reconstructUnderline


reconstructPlain :: String -> ParserM ()
reconstructPlain str
    | null str =
        do modify $ \s -> s { isPlain = False }
           reducePlainM
    | head str == '\'' =
        do emphasis <- isEmphasis <$> get
           unless emphasis $ do
             gotoM reconstructEmphasis
             gotoM reconstructPlain
    | head str == '_' =
        do underline <- isUnderline <$> get
           unless underline $ do
             gotoM reconstructUnderline
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
        do gotoM reconstructEmphasis
           gotoM reconstructM
    | head str == '_' =
        do gotoM reconstructUnderline
           gotoM reconstructM
    | otherwise =
        do gotoM reconstructPlain
           gotoM reconstructM


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
        [] -> error "stack is empty"
        [[]] -> error "stack of stack is empty"
        [docs] -> map (either (error "reconstruct") id) docs
        x -> error $ "problematic " ++ show x


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