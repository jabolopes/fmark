module Data.Text where


data Text = Footnote String
          | Plain String

instance Show Text where
    show (Footnote str) = "[" ++ str ++ "]"
    show (Plain str) = str
