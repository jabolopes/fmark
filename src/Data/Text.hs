module Data.Text where


-- | 'Text' represents predefined styled elements.
data Text
    -- | 'Plain' is a 'Text' element without style.
    = Plain String
    -- | 'Footnote' is a 'Text' element that represents a footnote.
    | Ref String

    | Span String [Text]

instance Show Text where
    show (Plain str) = str
    show (Ref str) = "[" ++ str ++ "]"
    show (Span sty txts) = sty ++ " -> " ++ show txts