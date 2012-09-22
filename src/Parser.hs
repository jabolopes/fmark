module Parser where

import Data.Char (isPunctuation, isSpace)

import Data.Document
import Data.Text
import Data.Token
import Utils

import Debug.Trace


-- | 'isParagraph' @str@ decides whether @str@ is a paragraph or a
-- heading.
isParagraph :: String -> Bool
isParagraph str =
    -- isPunctuation c && (not $ c `elem` "()[]'\"")
    c `elem` ".!?"
    where c = last str


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


-- | 'reduce' @idns ln@ is the 'List' containing the 'Text' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> [Token]
reduce idns n ln = section idns (indentation ln) ++ [Literal n $ trim ln]


-- | 'classify' @str@ is the 'List' of 'Token's of @str@.
classify :: String -> [Token]
classify str =
    classify' [0] $ zip [1..] $ lines str
        where classify' _ [] = []
              classify' _ [(_, ln)] | all isSpace ln = []
              classify' idns ((_, ln1):lns) | all isSpace ln1 = classify' idns lns
              classify' idns [(n, ln)] = reduce idns n ln
              classify' idns ((n1, ln1):(n2, ln2):lns)
                  | all isSpace ln2 = reduce idns n1 ln1 ++ classify' (push idn1 (dropWhile (> idn1) idns)) lns
                  | idn1 < idn2 = reduce idns n1 ln1 ++ classify' (push idn1 (dropWhile (> idn1) idns)) ((n2, ln2):lns)
                  | idn1 > idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
                  | otherwise = classify' idns ((n1, join ln1 ln2):lns)
                  where idn1 = indentation ln1
                        idn2 = indentation ln2


-- | 'reconstruct' @str@ produces the 'List' of 'Text' elements for
-- 'String' @str@.
reconstructLine :: String -> [Text]
reconstructLine str = reconstruct str
    where block sty = "[" ++ sty ++ " "

          mkText c fn str =
              case span (/= c) str of
                (hd, []) -> [Plain hd]
                (hd, _:tl) -> fn hd:reconstruct tl
          
          isSpan sty str = take (length (block sty)) str == (block sty)

          mkSpan sty str =
              case span (/= ']') (drop (length (block sty)) str) of
                (hd, []) -> [Plain hd]
                (hd, _:tl) -> Span sty (reconstructLine hd):reconstruct tl

          reconstruct "" = []
          reconstruct str | isSpan "bold" str = mkSpan "bold" str
                          | isSpan "italic" str = mkSpan "italic" str
                          | isSpan "underline" str = mkSpan "underline" str
                          | isSpan "footnote" str = mkSpan "footnote" str
                          | isSpan "cite" str = mkSpan "cite" str
          reconstruct ('[':str) = mkText ']' Ref str
          reconstruct str =
              Plain hd:reconstruct tl
              where (hd, tl) = span (\c -> not $ elem c "[") str


-- | 'reconstructLines' @str@ produces the 'List' of 'Text' elements
-- for each line in @str@.
reconstructLines :: String -> [[Text]]
reconstructLines = map reconstructLine . lines


-- | 'docify' @tks@ parses the sequence of 'Token's @tks@ into a 'Document'.
docify :: [Token] -> Document
docify tks =
    docify' tks [[]]
    where docify' :: [Token] -> [[Document]] -> Document
          -- edit: this 'ensureDocument' is interfering with style weaving
          -- docify' [] [docs] = ensureDocument $ reverse docs
          docify' [] [docs] = Content $ reverse docs
          docify' [] st = docify' [EndSection] st

          docify' (Literal n str:tks) (top:st) =
              docify' tks ((doc:top):st)
              where doc | isParagraph str = Paragraph (n, str) $ reconstructLine $ replace ' ' str
                        | otherwise = Heading (n, str) $ reconstructLines str

          docify' (BeginSection:tks) st =
              docify' tks ([]:st)

          docify' (EndSection:tks) (top:bot:st) =
              docify' tks ((Section (ensureDocument $ reverse top):bot):st)

          docify' tks st =
              error $ "\n\n\tdocify: docify': unhandled case" ++
                      "\n\n\t tks = " ++ show tks ++
                      "\n\n\t st = " ++ show st ++ "\n\n"