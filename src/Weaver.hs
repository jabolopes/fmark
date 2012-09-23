-- | 'Weaver' combines content and style to produced styled
-- 'Document's.
module Weaver where

import Control.Monad (zipWithM)
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Document
import Data.Text
import Data.Token
import Parser
import Utils


-- | 'msgLine' @title cntLoc styLoc@ produces a warning message for a
-- given content and style 'Srcloc'.
msgLine :: String -> Srcloc -> String -> String
msgLine title (n, cnt) sty =
    intercalate "\n" ["In line " ++ show n ++ ", " ++ title,
                      prefix "  " cnt,
                      "does not match style",
                      prefix "  " sty]


-- | 'msgLines' @title cntLoc1 cntLoc2 styLoc1 styLoc2@ produces a
-- warning message that spans multiple lines of content and style.
-- This warning message is adjusted to possibly equal ranges of
-- content and style lines.
msgLines :: String -> Srcloc -> Srcloc -> Srcloc -> Srcloc -> String
msgLines title (n1, cnt1) (n2, cnt2) (n1', sty1) (n2', sty2) =
    let
        pre | n1 == n2 = "In line " ++ show n1
            | otherwise = "In lines " ++ show n1 ++ "-" ++ show n2
        cnt | n1 == n2 = cnt1
            | otherwise = intercalate "\n" [cnt1, "...", cnt2]
        sty | n1' == n2' = sty1
            | otherwise = intercalate "\n" [sty1, "...", sty2]
    in
      intercalate "\n" [pre ++ ", " ++ title,
                        prefix "  " cnt,
                        "does not match style",
                        prefix "  " sty]


-- | 'msgHeading' @cntLoc styLoc@ produces a suitable warning message
-- for a 'Heading' element given the 'Srcloc' of the content and the
-- 'Srcloc' of the style.
msgHeading :: Srcloc -> Srcloc -> String
msgHeading loc (_, styStr) = msgLine "heading" loc styStr


-- | 'msgPararaph' @cntLoc styLoc@ produces a suitable warning message
-- for a 'Paragraph' element given the 'Srcloc' of the content and the
-- 'Srcloc' of the style.
msgParagraph :: Srcloc -> Srcloc -> String
msgParagraph loc (_, styStr) = msgLine "paragraph" loc styStr


-- | 'weaveText' @loc txt1 txt2@ weaves 'Text' @txt1@ with style of
-- 'Text' @txt2@ where @loc@ is the 'Srloc' of the produced 'Style'
-- elements.  'Nothing' is returned if the style cannot be applied.
weaveText :: Srcloc -> Text -> Text -> Maybe Document
weaveText loc (Ref cnt) (Ref sty) =
    Just $ Style loc (trim sty) [[Plain $ trim cnt]]

weaveText loc (Plain cnt) (Plain sty) =
    Just $ Style loc (trim sty') [[Plain $ trim cnt]]
    where sty' | isParagraph sty = init sty
               | otherwise = sty
                             
weaveText loc (Span _ _) (Span _ _) = error "weaveText: not implemented for Span Span"
weaveText _ _ _ = Nothing


weaveExtraLine :: Srcloc -> [Text] -> Text -> Maybe Document
weaveExtraLine loc txts (Plain sty) = Just $ Style loc (trim sty) [txts]
weaveExtraLine loc txts (Ref sty) = Just $ Style loc (trim sty) [txts]
weaveExtraLine loc txts (Span _ _) = error "weaveExtraLine: not implemented for Span Span"


-- | 'weaveLine' @loc txts1 txts2@ weaves 'Text's @txts1@ with style
-- of @txts2@ where @loc@ is the 'Srcloc' of the produced 'Style'
-- elements.  'Nothing' is returned if the style cannot be applied.
weaveLine :: Srcloc -> [Text] -> [Text] -> Maybe [Document]
weaveLine _ txts1 txts2 | length txts1 < length txts2 = Nothing
weaveLine loc txts1 txts2 | length txts1 == length txts2 = zipWithM (weaveText loc) txts1 txts2
weaveLine loc txts1 txts2 =
     let (matTxts, unmatTxts) = splitAt (length txts2 - 1) txts1
     in do docs <- weaveLine loc matTxts $ init txts2
           doc <- weaveExtraLine loc unmatTxts $ last txts2
           return $ docs ++ [doc]


weaveExtraLines :: Srcloc -> [[Text]] -> [Text] -> Maybe Document
weaveExtraLines loc lns [Plain sty] = Just $ Style loc (trim sty) lns
weaveExtraLines loc lns [Ref sty] = Just $ Style loc (trim sty) lns
weaveExtraLines loc lns [Span _ _] = error "weaveExtraLines: not implemented for Span"
weaveExtraLines _ _ _ = Nothing


-- | 'weaveLines' @loc lns1 lns2@ weaves lines @lns1@ with style of
-- lines @lns2@ where @loc@ is the 'Srcloc' of the produced 'Style'
-- elements.  'Nothing' is returned if the style cannot be applied.
weaveLines :: Srcloc -> [[Text]] -> [[Text]] -> Maybe [[Document]]
weaveLines _ lns1 lns2 | length lns1 < length lns2 = Nothing
weaveLines loc lns1 lns2 | length lns1 == length lns2 = zipWithM (weaveLine loc) lns1 lns2
weaveLines loc lns1 lns2 =
    let (matLns, unmatLns) = splitAt (length lns2 - 1) lns1 in
    do docss <- weaveLines loc matLns $ init lns2
       doc <- weaveExtraLines loc unmatLns $ last lns2
       return $ docss ++ [[doc]]


-- | 'weaveStyle' @doc style@ combines content 'Document' @doc@ and
-- style 'Document' @style@ in a single styled 'Document'.
weave :: Document -> Document -> (Document, [String])
weave doc style =
    let (docs, errs) = weave' doc style in (ensureDocument docs, errs)
    where weave' cnt@(Heading loc1 lns1) (Heading loc2 lns2) =
              case weaveLines loc1 lns1 lns2 of
                Nothing -> ([cnt], [msgHeading loc1 loc2])
                Just docs -> (concat docs, [])

          weave' cnt@(Paragraph loc1 txts1) (Paragraph loc2 txts2) =
              case weaveLine loc1 txts1 txts2 of
                Nothing -> ([cnt], [msgParagraph loc1 loc2])
                Just docs -> (docs, [])

          weave' cnt@(Content docs1) sty@(Content docs2) | length docs1 < length docs2 =
              let
                  (cntLoc1, cntLoc2) = rangeloc cnt
                  (styLoc1, styLoc2) = rangeloc sty
              in
                ([cnt], [msgLines "content" cntLoc1 cntLoc2 styLoc1 styLoc2])

          weave' (Content docs1) (Content docs2) =
              let
                  (matDocs, unmatDocs) = splitAt (length docs2) docs1
                  (docss, errss) = unzip $ zipWith weave' matDocs docs2
              in ([Content (concat docss ++ unmatDocs)], concat errss)

          weave' (Section doc1) (Section doc2) =
              let (doc', errs) = weave' doc1 doc2 in
              ([Section $ ensureDocument doc'], errs)

          weave' cnt sty =
              let
                  (cntLoc1, cntLoc2) = rangeloc cnt
                  (styLoc1, styLoc2) = rangeloc sty
              in
                ([cnt], [msgLines "document" cntLoc1 cntLoc2 styLoc1 styLoc2])