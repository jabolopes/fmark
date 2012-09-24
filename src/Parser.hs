module Parser where

import Data.Char (isPunctuation, isSpace)
import Data.List (intercalate)

import Data.Document
--import Data.Text
import Data.Token
import Utils

import Debug.Trace


paragraphTerminator :: [Char]
paragraphTerminator = ".!?"


-- | 'isParagraph' @str@ decides whether @str@ is a paragraph or a
-- heading.
isParagraph :: String -> Bool
isParagraph str = last str `elem` paragraphTerminator


-- | 'section' @idns idn@ is the 'List' of 'BeginSection' and
-- 'EndSection' 'Token's issued according the indentation stack @idns@
-- and current indentation @idn@.
section :: [Int] -> Int -> [Token]
section [] _ = error "section: idns is empty"
section (idn1:idns) idn2 =
    case compare idn1 idn2 of
      EQ -> []
      LT -> [BeginSection]
      GT -> EndSection:section (dropWhile (> idn1) idns) idn2


tokenize :: Int -> String -> Token
tokenize n ln = Literal (n, ln)


-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> [Token]
reduce idns n ln = section idns (indentation ln) ++ [tokenize n $ trim ln]


-- | 'classify' @str@ is the 'List' of 'Token's of @str@.
classify :: String -> [Token]
classify str =
    if isEmpty $ last tks then
        init tks
    else
        tks
    where tks = classify' [0] $ zip [1..] $ lines str

          classify' :: [Int] -> [(Int, String)] -> [Token]
          classify' _ [] = []
          classify' _ [(_, ln)] | all isSpace ln = []
          classify' idns ((_, ln1):lns) | all isSpace ln1 = classify' idns lns
          classify' idns [(n, ln)] = reduce idns n ln
          classify' idns ((n1, ln1):(n2, ln2):lns)
              | all isSpace ln2 = reduce idns n1 ln1 ++ [Empty] ++ classify' (push idn1 (dropWhile (> idn1) idns)) lns
              | idn1 < idn2 = reduce idns n1 ln1 ++ classify' (push idn1 (dropWhile (> idn1) idns)) ((n2, ln2):lns)
              | idn1 > idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              -- | otherwise = classify' idns ((n1, join ln1 ln2):lns)
              | otherwise = reduce idns n1 ln1 ++ classify' (push idn1 (dropWhile (> idn1) idns)) ((n2, ln2):lns)
              where idn1 = indentation ln1
                    idn2 = indentation ln2


-- 'reconstruct' @str@ produces the 'List' of 'Text' elements
-- for 'String' @str@.
reconstruct :: String -> [Document]
reconstruct ln = reconstruct' ln
    where block sty = "[" ++ sty ++ " "

          -- mkText c fn str =
          --     case span (/= c) str of
          --       (hd, []) -> [Plain hd]
          --       (hd, _:tl) -> fn hd:reconstruct' tl

          spanChar :: Char -> String -> String -> [Document]
          spanChar c sty str =
              case span (/= c) str of
                (hd, []) -> [mkPlain hd]
                (hd, _:tl) -> mkSpan sty [mkPlain hd]:reconstruct' tl

          -- isSpan sty str = take (length (block sty)) str == (block sty)

          -- mkSpanElement sty str =
          --     case span (/= ']') (drop (length (block sty)) str) of
          --       (hd, []) -> [mkPlain hd]
          --       (hd, _:tl) -> mkSpan sty (reconstruct' hd):reconstruct' tl


          reconstruct' :: String -> [Document]
          reconstruct' "" = []
          -- reconstruct' str | isSpan "bold" str = mkSpanElement "bold" str
          --                 | isSpan "italic" str = mkSpanElement "italic" str
          --                 | isSpan "underline" str = mkSpanElement "underline" str
          --                 | isSpan "footnote" str = mkSpanElement "footnote" str
          --                 | isSpan "cite" str = mkSpanElement "cite" str
          -- reconstruct' ('[':str) = mkText ']' Ref str
          -- edit: don't capture quotes in words, e.g., "don't" and "can't"
          reconstruct' ('\'':str) = spanChar '\'' "emphasis" str
          reconstruct' ('_':str) = spanChar '_' "underline" str
          reconstruct' str =
              mkPlain hd:reconstruct' tl
              where (hd, tl) = span (\c -> not $ elem c "['_") str


-- data Prefix = NoPrefix
--             | UnorderedPrefix
--               deriving (Eq)


-- refactor :: Srcloc -> [String] -> [Document]
-- refactor srcloc =
--     map refactor' . groupPair (==) . map split
--     where split ('*':' ':str) = (UnorderedPrefix, str)
--           split str = (NoPrefix, str)

--           refactor' :: (Prefix, [String]) -> Document
--           refactor' (NoPrefix, strs) | isParagraph (last strs) = Paragraph srcloc $ reconstruct $ intercalate " " strs
--                                      | otherwise = Heading srcloc $ map reconstruct strs

--           refactor' (UnorderedPrefix, strs) = Unordered $ map (Paragraph srcloc . refactorLine) strs


isUnorderedItem ('*':' ':_) = True
isUnorderedItem _ = False


refactor :: [String] -> [Document]
refactor [] = []
refactor lns | isUnorderedItem $ head lns =
    let
        (items, lns') = span isUnorderedItem lns
        doc = mkEnumeration $ map (mkItem . reconstruct) items
    in
      doc:refactor lns'

refactor lns =
    let
        (strs, lns') = span (not . isUnorderedItem) lns
        doc = if isParagraph $ last strs then
                  mkParagraph $ reconstruct $ intercalate " " strs
              else
                  mkHeading $ map reconstruct strs
    in
      doc:refactor lns'


-- > Literal ...
-- > Literal ...
-- > Enumeration ...
-- > Literal ...
--
-- > Section
-- >  Item ...
-- >  Item ...
-- >  Enumeration ...
-- >  Paragraph ...
-- or
-- > Enumeration
-- >  Item ...
-- >  Item
-- >  Enumeration
restructure :: [Either Srcloc Document] -> Document
restructure locs =
    reorganize $ restructure' locs
    where restructure' [] = []          
          restructure' locs | isLeft (head locs) =
              let
                  (locs', docs) = span isLeft locs
                  locs'' = map fromLeft locs'
                  strs = map snd locs''
              in
                refactor strs ++ restructure' docs

          restructure' docs =
              let
                  (docs', locs) = span (not . isLeft) docs
                  docs'' = map fromRight docs'
              in
                docs'' ++ restructure' locs

          reorganize docs | all (\doc -> isEnumeration doc || isItem doc) docs = mkEnumeration docs
                          | otherwise = mkContent docs
                  


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False


fromLeft :: Either a b -> a
fromLeft (Left x) = x


fromRight :: Either a b -> b
fromRight (Right x) = x


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks = docify' tks [[]] [[]]
    where --docify' :: [Token] -> [[Document]] -> Document
          -- edit: this 'ensureDocument' is interfering with style weaving
          -- docify' [] [docs] = ensureDocument $ reverse docs

          -- reduceLocs locs = ensureDocument $ reconstruct (head locs) $ map snd locs


          shift :: Srcloc -> [Token] -> [[Either Srcloc Document]] -> [[Document]] -> Document
          shift srcloc tks (topTks:stTks) stDocs =
              docify' tks ((Left srcloc:topTks):stTks) stDocs

          pushTokens :: [Token] -> [[Either Srcloc Document]] -> [[Document]] -> Document
          pushTokens tks stTks stDocs =
              docify' tks ([]:stTks) stDocs

          reduceEndSection tks (locs:stTks) stDocs = undefined
              -- let
              --     doc = case reduceLocs locs of
              --             doc | isContent doc -> Section doc
              --             doc -> doc
              --     docs' = reconstruct (head locs') $ map snd locs'
              -- in
              --   docify' tks ([]:stTks) ((doc:reverse docs'):stDocs)

          reduceEmpty tks (locs:stTks) (topDocs:stDocs) =
              let doc = restructure locs in
              pushTokens tks stTks ((doc:topDocs):stDocs)


          docify' :: [Token] -> [[Either Srcloc Document]] -> [[Document]] -> Document
          docify' [] _ [docs] = mkContent $ reverse docs
          docify' [] stTks stDocs = docify' [EndSection] stTks stDocs

          docify' (Literal srcloc:tks) stTks stDocs =
              shift srcloc tks stTks stDocs

          docify' (BeginSection:tks) stTks stDocs =
              pushTokens tks stTks stDocs
 
          docify' (EndSection:tks) stTks stDocs =
              reduceEndSection tks stTks stDocs

          docify' (Empty:tks) stTks stDocs =
              reduceEmpty tks stTks stDocs