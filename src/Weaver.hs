{-# LANGUAGE ParallelListComp #-}
module Weaver where

import Control.Monad (zipWithM)
import Data.List (intercalate)

import Data.Document
import Data.Text
import Parser
import Utils


-- | 'weaveStyle' @doc style@ combines content 'Document' @doc@ and
-- style 'Document' @style@ in a single 'Document'.
weaveStyle :: Document -> Document -> (Document, [String])
weaveStyle doc style =
    let (docs, errs) = loop doc style in (ensureDocument docs, errs)
    where msg n cnt title desc sty =
              intercalate "\n" ["In line " ++ show n ++ ", " ++ title,
                                prefix "  " cnt,
                                desc,
                                prefix "  " sty]

          msgArrow :: Int -> Int -> String -> String -> String -> String
          msgArrow n i title cnt sty =
              intercalate "\n" ["In line " ++ show (n + i - 1) ++ ", in " ++ title,
                                prefixArrow i "  " cnt,
                                "does not match style",
                                prefixArrow i "  " sty]


          weaveText :: Text -> Text -> Maybe Document
          weaveText (Footnote cnt) (Footnote sty) = Just $ Style (trim sty) $ Plain $ trim cnt
          weaveText text@(Plain cnt) (Plain sty) = Just $ Style (trim sty') $ Plain $ trim cnt
              where sty' | isParagraph sty = init sty
                         | otherwise = sty
          weaveText text1 text2 = Nothing


          weaveLine :: [Text] -> [Text] -> Maybe [Document]
          weaveLine txts1 txts2 | length txts1 /= length txts2 = Nothing
          weaveLine txts1 txts2 = zipWithM weaveText txts1 txts2


          weaveLines :: [[Text]] -> [[Text]] -> Maybe [[Document]]
          weaveLines lns1 lns2 | length lns1 /= length lns2 = Nothing
          weaveLines lns1 lns2 = zipWithM weaveLine lns1 lns2


          headingMsg (n, cntStr) (_, styStr) =
              msg n cntStr "heading" "does not match style" styStr


          paragraphMsg (n, cntStr) (_, styStr) =
              msg n cntStr "paragraph" "does not match style" styStr


          loop :: Document -> Document -> ([Document], [String])
          loop cnt@(Heading loc1 lns1) (Heading loc2 lns2) =
              case weaveLines lns1 lns2 of
                Nothing -> ([cnt], [headingMsg loc1 loc2])
                Just docs -> (concat docs, [])

          loop cnt@(Paragraph loc1 txts1) (Paragraph loc2 txts2) =
              case weaveLine txts1 txts2 of
                Nothing -> ([cnt], [paragraphMsg loc1 loc2])
                Just docs -> (docs, [])

          loop (Content docs1) (Content docs2) =
              let errs | length docs1 < length docs2 = ["content: " ++ show docs1 ++ " does not match style " ++ show docs2]
                       | otherwise = []
                  (matDocs, unmatDocs) = splitAt (length docs2) docs1
                  (docsSty', errss) = unzip [ loop doc1 doc2 | doc1 <- matDocs | doc2 <- docs2 ]
              in
                ([Content (concat docsSty' ++ unmatDocs)], errs ++ concat errss)

          loop (Section doc1) (Section doc2) =
              let (doc', errs) = loop doc1 doc2 in
              ([Section $ ensureDocument doc'], errs)

          loop doc _ = ([doc], [show doc])