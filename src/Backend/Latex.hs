module Backend.Latex where

-- | 'docToLatex' @mstyle doc@ formats a styled 'Document' @doc@ into
-- a LaTeX 'String', where @mstyle@ specifies the style 'Document'
-- used to stylize @doc@.
-- docToLatex :: Maybe Document -> Document -> String
-- docToLatex mstyle doc =
--     let ps = properties doc 
--         title = lookup "Title" ps
--         subtitle = lookup "Subtitle" ps
--         author = lookup "Author" ps
--         date = lookup "Date" ps
--         abstract = lookup "Abstract" ps
--         maketitle = case (title, author, date) of
--                       (Just _, Just _, Just _) -> def "maketitle"
--                       _ -> ""
--     in
--       seq [comArgs "documentclass" ["a4paper"] "article",
--            comArgs "usepackage" ["utf8"] "inputenc",
--            fulltitle title subtitle,
--            prop (com "author") author,
--            prop (com "date") date,
--            env "document" $ seq [maketitle,
--                                  prop (env "abstract") abstract,
--                                  loop 0 doc]]
--     where fulltitle Nothing Nothing = ""
--           fulltitle (Just t) Nothing = com "title" t
--           fulltitle Nothing (Just s) = com "title" $ com "large" s
--           fulltitle (Just t) (Just s) = com "title" $ nls $ t ++ "\n" ++ com "large" s
          
--           properties (Heading _ docs) = concatMap properties docs
--           properties (Paragraph _ docs) = concatMap properties docs
--           properties (Content docs) = concatMap properties docs
--           properties (Section doc) = properties doc

--           properties (Footnote _) = []
--           properties (Plain _) = []
--           properties (Style str doc) = [(str, loop 0 doc)]

--           lit = concatMap lit'
--               where lit' '#' = "\\#"
--                     lit' c = [c]

--           nls = concatMap nls'
--               where nls' '\n' = "\\\\"
--                     nls' c = [c]

--           def id = '\\':lit id
--           com id str = "\\" ++ lit id ++ "{" ++ lit str ++ "}"
--           comArgs id args str = "\\" ++ lit id ++ "[" ++ intercalate "," (map lit args) ++ "]{" ++ lit str ++ "}"
--           env id str = "\\begin{" ++ lit id ++ "}\n" ++ lit str ++ "\n\\end{" ++ lit id ++ "}"

--           prop fn = maybe "" $ fn . lit

--           sec lvl str
--               | lvl < 3 = com (concat (replicate lvl "sub") ++ "section") $ nls str
--               | lvl == 3 = com "paragraph" $ nls str
--               | lvl == 4 = com "subparagraph" $ nls str

--           par = lit

--           seq = intercalate "\n" . filter (\ln -> trim ln /= "")

--           loop lvl (Heading _ _ docs) = sec lvl $ intercalate "\n" $ map (loop lvl) docs
--           loop lvl (Paragraph _ _ docs) = concat $ map (loop lvl) docs
--           loop lvl (Content docs) = seq $ map (loop lvl) docs
--           loop lvl (Section doc) = loop (lvl + 1) doc

--           loop lvl (Footnote str) = com "footnote" str
--           loop lvl (Plain str) = lit str
--           loop lvl (Style _ _) = ""