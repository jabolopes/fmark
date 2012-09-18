{-# LANGUAGE ParallelListComp #-}
module Utils where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)


-- | 'filterLines' @lns@ filters empty lines and lines containing only
-- space characters from @lns@.
filterLines :: [String] -> [String]
filterLines lns = [ ln | ln <- lns, trim ln /= "" ]


-- | 'replace' @c str@ replaces newlines in @str@ with @c@
--
-- > replace "hello\ngoodbye" == "hello,goodbye"
replace :: Char -> String -> String
replace c =
    map loop
    where loop '\n' = c
          loop c = c


-- | 'indentation' @ln@ is the number of space characters at the
-- begining of @ln@.
--
-- > indentation "\t hello" == 2
indentation :: String -> Int
indentation ln = length $ takeWhile isSpace ln


-- | 'join' @str1 str2@ appends @str1@ and @str2@ ensuring that is
-- only a single newline space character between them.
--
-- > join "hello \n" "\t goodbye" == "hello\ngoodbye"
join :: String -> String -> String
join str1 str2 = dropWhileEnd isSpace str1 ++ "\n" ++ dropWhile isSpace str2


-- | 'prefix' @pre str@ prepends all lines in 'str' with 'pre'.
--
-- > prefix "->" "hello\ngoodbye" == "->hello\n->goodbye"
prefix :: String -> String -> String
prefix pre str =
    pre ++ prefix' str
    where prefix' =
              concatMap (\c -> case c of
                                 '\n' -> '\n':pre
                                 _ -> [c])


-- | 'push' @x xs@ adds @x@ to @xs@ only if the first element in @xs@
-- is different from @x@.
--
-- > push 1 [2,3] == [1,2,3]
-- > push 1 [1,3] == [1,3]
push :: Eq a => a -> [a] -> [a]
push x (y:ys) | x /= y = x:y:ys
push _ xs = xs


-- | 'trim' @str@ removes leading and trailing space characters from
-- @str@.
--
-- > trim "\t hello \n" == "hello"
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace