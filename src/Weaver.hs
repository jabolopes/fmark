{-# LANGUAGE ParallelListComp #-}
module Weaver where

import Control.Monad (zipWithM)
import Data.List (intercalate)

import Data.Document
import Data.Text
import Parser
import Utils


msgLine :: Int -> String -> String -> String -> String
msgLine n title cnt sty =
    intercalate "\n" ["In line " ++ show n ++ ", " ++ title,
                      prefix "  " cnt,
                      "does not match style",
                      prefix "  " sty]


msgLines :: Int -> Int -> String -> String -> String -> String -> String -> String
msgLines n1 n2 title cnt1 cnt2 sty1 sty2 =
    intercalate "\n" ["Between lines " ++ show n1 ++ " and " ++ show n2 ++ ", " ++ title,
                      prefix "  " cnt1,
                      "  ...",
                      prefix "  " cnt2,
                      "does not match style",
                      prefix "  " sty1,
                      "  ...",
                      prefix "  " sty2]


headingMsg :: Srcloc -> Srcloc -> String
headingMsg (n, cntStr) (_, styStr) = msgLine n "heading" cntStr styStr


paragraphMsg :: Srcloc -> Srcloc -> String
paragraphMsg (n, cntStr) (_, styStr) = msgLine n "paragraph" cntStr styStr


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
                Nothing -> ([cnt], [headingMsg loc1 loc2])
                Just docs -> (concat docs, [])

          weave' cnt@(Paragraph loc1 txts1) (Paragraph loc2 txts2) =
              case weaveLine loc1 txts1 txts2 of
                Nothing -> ([cnt], [paragraphMsg loc1 loc2])
                Just docs -> (docs, [])

          weave' cnt@(Content docs1) sty@(Content docs2) | length docs1 < length docs2 =
              let
                  ((n1, cnt1), (n2, cnt2)) = rangeloc cnt
                  ((_, sty1), (_, sty2)) = rangeloc sty
              in
                ([cnt], [msgLines n1 n2 "content" cnt1 cnt2 sty1 sty2])

          weave' (Content docs1) (Content docs2) =
              let errs | length docs1 < length docs2 = ["content: " ++ show docs1 ++ " does not match style " ++ show docs2]
                       | otherwise = []
                  (matDocs, unmatDocs) = splitAt (length docs2) docs1
                  (docsSty', errss) = unzip [ weave' doc1 doc2 | doc1 <- matDocs | doc2 <- docs2 ]
              in
                ([Content (concat docsSty' ++ unmatDocs)], errs ++ concat errss)

          weave' (Section doc1) (Section doc2) =
              let (doc', errs) = weave' doc1 doc2 in
              ([Section $ ensureDocument doc'], errs)

          weave' doc _ = ([doc], [show doc])