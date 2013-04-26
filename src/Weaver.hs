-- | 'Weaver' combines content and style to produced styled
-- 'Document's.
module Weaver where

import Control.Monad (zipWithM)
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Document
import Data.Token
import Parser
import Utils


-- 'msgLine' @title cntLoc styLoc@ produces a warning message for a
-- given content and style 'Srcloc'.
msgLine :: String -> Srcloc -> Srcloc -> String -> String
msgLine title (n1, _, cnt1) (n2, _, cnt2) sty =
    let
        pre | n1 == n2 = "In line " ++ show n1
            | otherwise = "In lines " ++ show n1 ++ "-" ++ show n2
        cnt | n1 == n2 = cnt1
            | otherwise = intercalate "\n" [cnt1, "...", cnt2]
    in
      intercalate "\n" [pre ++ ", " ++ title,
                        prefix "  " cnt,
                        "does not match style",
                        prefix "  " sty]


-- | 'msgLines' @title cntLoc1 cntLoc2 styLoc1 styLoc2@ produces a
-- warning message that spans multiple lines of content and style.
-- This warning message is adjusted to possibly equal ranges of
-- content and style lines.
msgLines :: String -> Srcloc -> Srcloc -> Srcloc -> Srcloc -> String
msgLines title (n1, _, cnt1) (n2, _, cnt2) (n1', _, sty1) (n2', _, sty2) =
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


weaveExtraDocuments :: Srcloc -> Srcloc -> [Document] -> Document -> ([Document], [String])
weaveExtraDocuments loc1 loc2 lns Document { element = Plain sty } =
    ([Document loc1 loc2 (Span (trim sty)) lns], [])

-- weaveExtraDocuments loc lns [Ref sty] = Just $ Document loc (trim sty) lns
weaveExtraDocuments loc1 loc2 lns Document { element = Span _ } =
    error "weaveExtraDocuments: not implemented for Span"

-- edit: remove 'show'
weaveExtraDocuments loc1 loc2 lns sty = (lns, [msgLine "EDIT" loc1 loc2 (show sty)])


-- | 'ensureDocument' @docs@ the first elements of @docs@ if @docs@ is
-- a singleton list, otherwise, it returns a 'Content docs'
ensureDocument :: [Document] -> Document
ensureDocument [doc] = doc
-- edit: fix srcloc
ensureDocument docs = mkDocument Content docs


-- 'weaveStyle' @doc style@ combines content 'Document' @doc@ and
-- style 'Document' @style@ in a single styled 'Document'.
weave :: Document -> Document -> (Document, [String])
weave cnt@(Document loc1 loc2 (Plain _) []) Document { element = Plain sty } =
    (cnt { element = Span sty, children = [cnt] }, [])

weave cnt@(Document cntLoc1 cntLoc2 el1 docs1) sty@(Document styLoc1 styLoc2 el2 docs2)
    | el1 /= el2 =
        (cnt, [msgLines "EDIT" cntLoc1 cntLoc2 styLoc1 styLoc2])
    | length docs1 < length docs2 =
        (cnt, [msgLines "EDIT" cntLoc1 cntLoc2 styLoc1 styLoc2])
    | length docs1 == length docs2 =
        let (docs, errss) = unzip $ zipWith weave docs1 docs2 in
        (cnt { children = docs }, concat errss)
    | otherwise =
        let
            (matDocs, unmatDocs) = splitAt (length docs2 - 1) docs1
            (docs, errss) = unzip $ zipWith weave matDocs (init docs2)
            (docs', errs) = weaveExtraDocuments cntLoc1 cntLoc2 unmatDocs (last docs2)
        in
          (ensureDocument (docs ++ docs'), concat errss ++ errs)