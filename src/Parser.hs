module Parser where

import Control.Monad
import Data.Char (isPunctuation, isSpace)
import Data.List (intercalate)

import Data.Document
import Data.Token
import Utils

import Debug.Trace


isEmptyLn :: String -> Bool
isEmptyLn = all isSpace


-- | 'isParagraph' @str@ decides whether @str@ is a paragraph or a
-- heading.
isParagraph :: String -> Bool
isParagraph str = last str `elem` paragraphTerminator
    where paragraphTerminator = ".!?"


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


tokenize :: Int -> [Int] -> String -> Token
tokenize n idns ln = Literal (n, idns, ln)


-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> [Token]
reduce idns n ln = section idns idn ++ [tokenize n (push idn idns) $ trim ln]
    where idn = indentation ln


-- | 'classify' @str@ is the 'List' of 'Token's of @str@.
classify :: String -> [Token]
classify str = classify' [0] $ zip [1..] $ lines str
    where classify' :: [Int] -> [(Int, String)] -> [Token]
          classify' _ [] = []
          -- edit: make this a log? enable on verbose?
          -- classify' idns (ln:_) | trace ("classify' " ++ show idns ++ "  " ++ show ln) False = undefined
          classify' _ [(_, ln)] | isEmptyLn ln = []
          classify' idns ((_, ln1):lns) | isEmptyLn ln1 = classify' idns lns
          classify' idns [(n, ln)] = reduce idns n ln
          classify' idns ((n1, ln1):(n2, ln2):lns)
              | isEmptyLn ln2 =
                  let
                      ln1' = reduce idns n1 ln1
                      lns' = classify' (push idn1 idns) lns
                  in
                    case lns' of
                      [] -> ln1'
                      BeginSection:_ -> ln1' ++ lns'
                      EndSection:lns'' -> ln1' ++ [EndSection, Empty] ++ lns''
                      _ -> ln1' ++ [Empty] ++ lns'
              | idn1 < idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              | idn1 > idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              | otherwise = reduce idns n1 ln1 ++ classify' (push idn2 idns) ((n2, ln2):lns)
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


isUnorderedItem :: String -> Bool
isUnorderedItem ('*':' ':_) = True
isUnorderedItem _ = False


-- Example
-- > * ...
--
-- > Item ...
--
-- Example
-- > ...
--
-- > Content ...
spanify :: String -> Document
spanify ln | isUnorderedItem ln = mkItem $ reconstruct $ drop 2 ln
spanify ln = mkContent $ reconstruct ln


-- > * ...
-- > * ...
-- > Enumeration ...
-- > * ...
--
-- > Enumeration
-- >  Item ...
-- >  Item ...
-- >  Enumeration ...
-- >  Item ...
--
--
-- > * ...
-- > * ...
-- > Enumeration ...
-- > ... .
-- >
-- > Paragraph
-- >  Enumeration
-- >   Item ...
-- >   Item ...
-- >   Enumeration ...
-- >  Content ... .
--
--
-- > * ...
-- > * ...
-- > Enumeration ...
-- > ...
-- >
-- > Heading
-- >  Enumeration
-- >   Item ...
-- >   Item ...
-- >   Enumeration ...
-- >  Content ...
blockify :: Bool -> [Either Srcloc Document] -> Document
blockify blocklevel locs =
    restructure $ map blockify' locs
    where blockify' (Left (_, _, str)) = spanify str
          blockify' (Right doc) = doc

          restructure docs | all (\doc -> isEnumeration doc || isItem doc) docs = mkEnumeration docs
          restructure docs | not blocklevel = mkSection $ enumerate docs
          -- edit: what about headings?
          restructure docs = mkParagraph $ enumerate docs

          enumerate [] = []
          enumerate locs@(doc:_) | isEnumeration doc || isItem doc =
              let (items, docs) = span (\doc -> isEnumeration doc || isItem doc) locs in
              mkEnumeration items:enumerate docs

          enumerate (doc:docs) = doc:enumerate docs
          
          isParagraphBlock (Document _ (Plain str) _) = isParagraph str
          isParagraphBlock (Document _ _ docs) = isParagraphBlock $ last docs


-- isLeft :: Either a b -> Bool
-- isLeft (Left _) = True
-- isLeft _ = False


-- fromLeft :: Either a b -> a
-- fromLeft (Left x) = x


-- fromRight :: Either a b -> b
-- fromRight (Right x) = x


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks = docify' tks [[]] []
    where shift :: Srcloc -> [Token] -> [[Either Srcloc Document]] -> [Document] -> Document
          shift srcloc tks (topTks:stTks) stDocs =
              docify' tks ((Left srcloc:topTks):stTks) stDocs

          goto :: [Token] -> [[Either Srcloc Document]] -> [Document] -> Document
          goto tks stTks stDocs = docify' tks ([]:stTks) stDocs

          reduceEndSection :: [Token] -> [[Either Srcloc Document]] -> [Document] -> Document
          reduceEndSection tks (locs:topLocs:stTks) stDocs =
              let doc = blockify False $ reverse locs in
              docify' tks ((Right doc:topLocs):stTks) stDocs

          reduceEndSection tks stLocs stDocs =
              error $ "\n\n\treduceEndSection: unhandled case" ++
                      "\n\n\t tks = " ++ show tks ++
                      "\n\n\t stLocs " ++ show stLocs ++
                      "\n\n\t stDocs " ++ show stDocs ++
                      "\n\n\t length tks = " ++ show (length tks) ++
                      "\n\n\t length stLocs " ++ show (length stLocs) ++
                      "\n\n\t length stDocs " ++ show (length stDocs) ++ "\n\n"

          reduceEmpty :: [Token] -> [[Either Srcloc Document]] -> [Document] -> Document
          reduceEmpty tks (locs:stLocs) stDocs =
              let doc = blockify True $ reverse locs in
              goto tks stLocs (doc:stDocs)

          reduceEmpty tks stLocs stDocs =
              error $ "\n\n\treduceEmpty: unhandled case" ++
                      "\n\n\t tks = " ++ show tks ++
                      "\n\n\t stLocs " ++ show stLocs ++
                      "\n\n\t stDocs " ++ show stDocs ++ "\n\n"

          docify' :: [Token] -> [[Either Srcloc Document]] -> [Document] -> Document
          docify' [] [[]] docs = mkContent $ reverse docs
          docify' [] stLocs stDocs | length stLocs > 1 = reduceEndSection [] stLocs stDocs
          docify' [] stLocs stDocs = reduceEmpty [] stLocs stDocs

          docify' (Literal loc:tks) stLocs stDocs =
              shift loc tks stLocs stDocs

          docify' (BeginSection:tks) stLocs stDocs =
              goto tks stLocs stDocs

          docify' (EndSection:tks) stLocs stDocs =
              reduceEndSection tks stLocs stDocs

          docify' (Empty:tks) stLocs stDocs =
              reduceEmpty tks stLocs stDocs