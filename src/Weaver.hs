{-# LANGUAGE ParallelListComp #-}
module Weaver where

import Control.Monad (zipWithM)
import Data.List (intercalate)

import Data.Document
import Data.Text
import Parser
import Utils


msgLine :: String -> Srcloc -> String -> String
msgLine title (n, cnt) sty =
    intercalate "\n" ["In line " ++ show n ++ ", " ++ title,
                      prefix "  " cnt,
                      "does not match style",
                      prefix "  " sty]


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


msgHeading :: Srcloc -> Srcloc -> String
msgHeading loc (_, styStr) = msgLine "heading" loc styStr


msgParagraph :: Srcloc -> Srcloc -> String
msgParagraph loc (_, styStr) = msgLine "paragraph" loc styStr


weaveText :: Srcloc -> Text -> Text -> Maybe Document
weaveText loc (Footnote cnt) (Footnote sty) =
    Just $ Style loc (trim sty) $ Plain $ trim cnt

weaveText loc text@(Plain cnt) (Plain sty) =
    Just $ Style loc (trim sty') $ Plain $ trim cnt
    where sty' | isParagraph sty = init sty
               | otherwise = sty

weaveText _ _ _ = Nothing


weaveLine :: Srcloc -> [Text] -> [Text] -> Maybe [Document]
weaveLine _ txts1 txts2 | length txts1 /= length txts2 = Nothing
weaveLine loc txts1 txts2 = zipWithM (weaveText loc) txts1 txts2


weaveLines :: Srcloc -> [[Text]] -> [[Text]] -> Maybe [[Document]]
weaveLines _ lns1 lns2 | length lns1 /= length lns2 = Nothing
weaveLines loc lns1 lns2 = zipWithM (weaveLine loc) lns1 lns2


-- | 'weaveStyle' @doc style@ combines content 'Document' @doc@ and
-- style 'Document' @style@ in a single 'Document'.
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
              let (docss, errss) = unzip [ weave' doc1 doc2 | doc1 <- docs1 | doc2 <- docs2 ]
              in ([Content (concat docss)], concat errss)

          weave' (Section doc1) (Section doc2) =
              let (doc', errs) = weave' doc1 doc2 in
              ([Section $ ensureDocument doc'], errs)

          weave' cnt sty =
              let
                  (cntLoc1, cntLoc2) = rangeloc cnt
                  (styLoc1, styLoc2) = rangeloc sty
              in
                ([cnt], [msgLines "document" cntLoc1 cntLoc2 styLoc1 styLoc2])