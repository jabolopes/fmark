module Data.Token where

-- | 'Token' is an unstructured representation of the input.
data Token
    -- | 'Text' corresponds to successive lines of text joined
    -- together.
    = Literal Int String
    -- | 'BeginSection' represents the beginning of a new section,
    -- i.e., increase in indentation or an unmatched decrease in indentation.
    | BeginSection
    -- | 'EndSection' represents the end of a section, i.e., a
    -- decrease in indentation.
    | EndSection
      deriving (Show)