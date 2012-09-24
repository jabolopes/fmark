module Data.Token where


-- | 'Srcloc' represents an association between 'Document' parts and
-- 'Token' elements.
type Srcloc = (Int, String)


-- | 'Token' is an unstructured representation of the input.
data Token
    -- | 'Literal' corresponds to successive lines of text joined
    -- together.
    = Literal Srcloc
    -- | 'BeginSection' represents the beginning of a new section,
    -- i.e., increase in indentation or an unmatched decrease in
    -- indentation.
    | BeginSection
    -- | 'EndSection' represents the end of a section, i.e., a
    -- decrease in indentation.
    | EndSection
      
    | Empty
      deriving (Show)


isEmpty :: Token -> Bool
isEmpty Empty = True
isEmpty _ = False