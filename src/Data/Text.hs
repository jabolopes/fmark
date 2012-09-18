module Data.Text where


-- | 'Text' represents predefined styled elements.
data Text
    -- | 'Footnote' is a 'Text' element that represents a footnote.
    = Footnote String
    -- | 'Plain' is a 'Text' element without style.
    | Plain String

instance Show Text where
    show (Footnote str) = "[" ++ str ++ "]"
    show (Plain str) = str
