module Lexer where

import Data.Char (isPunctuation, isSpace)

import Data.Token
import Utils


isEmptyLn :: String -> Bool
isEmptyLn = all isSpace


-- | 'section' @idns idn@ is the 'List' of 'BeginSection' and
-- 'EndSection' 'Token's issued according the indentation stack @idns@
-- and current indentation @idn@.
section :: Char -> [Int] -> Int -> [Token]
section _ [] _ = error "section: idns is empty"
section c (idn1:idns) idn2 =
    case compare idn1 idn2 of
      EQ -> []
      LT -> [BeginSection c]
      GT -> EndSection:section c (dropWhile (> idn1) idns) idn2


tokenize :: Int -> [Int] -> String -> Token
tokenize n idns ln = Literal (n, idns, ln)


-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> [Token]
reduce idns n ln = section '|' idns idn ++ [tokenize n (push idn idns) $ trim ln]
    where idn = indentation ln


isBlockStarter :: String -> Bool
-- isBlockStarter (c:' ':_) = c `notElem` "*" && (c `elem` ">|" || isPunctuation c)
isBlockStarter (c:' ':_) = c `elem` ">|" || isPunctuation c
isBlockStarter _ = False


-- | 'classify' @str@ is the 'List' of 'Token's of @str@.
classify :: String -> [Token]
classify str = classify' [0] $ zip [1..] $ lines str
    where classify' :: [Int] -> [(Int, String)] -> [Token]
          classify' idns [] = replicate (length idns - 1) EndSection

          classify' idns ((n, ln):lns)
              | isEmptyLn ln = classify' idns lns
              | isBlockStarter (dropWhile isSpace ln) =
                  let 
                      (pre, c:suf) = span isSpace ln
                      ln' = pre ++ " " ++ suf
                      s1 = section '|' idns $ indentation ln
                      s2 = section c (push (indentation ln) idns) $ indentation ln'
                      lns' = classify' (push (indentation ln') idns) $ (n, ln'):lns
                  in
                    case lns' of
                      [] -> s1 ++ s2 ++ lns'
                      _ -> s1 ++ s2 ++ lns'

          classify' idns [(n, ln)] =
              reduce idns n ln ++ classify' (push (indentation ln) idns) []

          classify' idns ((n1, ln1):(n2, ln2):lns)
              | isEmptyLn ln2 =
                  let
                      ln1' = reduce idns n1 ln1
                      lns' = classify' (push idn1 idns) lns
                  in
                    case lns' of
                      [] -> ln1'
                      [EndSection] -> ln1' ++ [EndSection]
                      EndSection:lns'' -> ln1' ++ [EndSection, Empty] ++ lns''
                      _ -> ln1' ++ [Empty] ++ lns'
              | idn1 < idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              | idn1 > idn2 = reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
              | otherwise = reduce idns n1 ln1 ++ classify' (push idn2 idns) ((n2, ln2):lns)
              where idn1 = indentation ln1
                    idn2 = indentation ln2