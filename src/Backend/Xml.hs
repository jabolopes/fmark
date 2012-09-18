module Backend.Xml where

import Control.Monad.State (State, evalState, get, modify)
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Document
import Data.Text


-- | 'XmlState' is the XML generator state.
data XmlState = XmlState Int Bool

-- | 'XmlM' is the type of the XML generator 'Monad'.
type XmlM a = State XmlState a


-- | 'getIdn' is a 'Monad' with the current indentation level.
getIdn :: XmlM Int
getIdn =
    do XmlState idn _ <- get
       return idn

putIdn :: Int -> XmlM ()
putIdn idn =
    modify $ \(XmlState _ pre) -> XmlState idn pre


-- | 'withIdn' @m@ is a 'Monad' that temporarily creates a deeper
-- indentation level for 'Monad' @m@.
withIdn :: XmlM a -> XmlM a
withIdn m =
    do idn <- getIdn
       putIdn $ idn + 2
       val <- m
       putIdn idn
       return val


getPrefix :: XmlM Bool
getPrefix =
    do XmlState _ pre <- get
       return pre


putPrefix :: Bool -> XmlM ()
putPrefix pre =
    modify $ \(XmlState idn _) -> XmlState idn pre


withPrefix :: Bool -> XmlM a -> XmlM a
withPrefix pre m =
    do pre' <- getPrefix
       putPrefix pre
       val <- m
       putPrefix pre'
       return val


-- | 'docToXml' @mstyle doc@ formats a styled 'Document' @doc@ into a
-- XML 'String', where @mstyle@ specifies the style 'Document' used to
-- stylize @doc@.
docToXml :: Maybe Document -> Document -> String
docToXml _ doc =
    intercalate "\n" ["<xml>", str, "</xml>"]
    where str = evalState (loop doc) (XmlState 2 True)

          xmlIndent :: XmlM String -> XmlM String
          xmlIndent m =
              do idn <- getIdn
                 pre <- getPrefix
                 if pre then
                     do str <- m
                        return $ replicate idn ' ' ++ str
                 else
                     m

          xmlAttribute Nothing = []
          xmlAttribute (Just val) = [("style", val)]

          xmlAttributes [] = ""
          xmlAttributes attrs = ' ':unwords (map (\(id, val) -> id ++ "=\"" ++ val ++ "\"") attrs)

          xmlStr = xmlIndent

          xmlShortTag :: [(String, String)] -> String -> XmlM String -> XmlM String
          xmlShortTag attrs tag m =
              concat <$> sequence [xmlIndent (return ("<" ++ tag ++ xmlAttributes attrs ++ ">")),
                                   withPrefix False m,
                                   return ("</" ++ tag ++ ">")]

          xmlLongTag attrs tag m =
              do pre <- getPrefix
                 if pre then
                     concat <$> sequence [xmlIndent (return ("<" ++ tag ++ xmlAttributes attrs ++ ">\n")),
                                          withIdn m,
                                          return "\n",
                                          xmlIndent (return ("</" ++ tag ++ ">"))]
                 else
                     xmlShortTag attrs tag m

          xmlLongTags :: [(String, String)] -> String -> [XmlM String] -> XmlM String
          xmlLongTags attrs tag ms =
              do pre <- getPrefix
                 if pre then
                     xmlLongTag attrs tag $ intercalate "\n" <$> sequence ms
                 else
                     xmlLongTag attrs tag $ concat <$> sequence ms

          loopText :: Text -> XmlM String
          loopText (Footnote str) = withPrefix False $ xmlShortTag [] "footnote" $ return str
          loopText (Plain str) = xmlStr $ return str

          loop :: Document -> XmlM String
          loop (Heading _ [txts]) = xmlShortTag [] "heading" $ concat <$> mapM loopText txts
          loop (Heading _ lns) = xmlLongTags [] "heading" [ concat <$> mapM loopText txts | txts <- lns ]
          loop (Paragraph _ txts) = xmlShortTag [] "paragraph" $ concat <$> mapM loopText txts
          loop (Content [doc]) = xmlShortTag [] "content" $ loop doc
          loop (Content docs) = xmlLongTags [] "content" $ map loop docs
          loop (Section doc) = xmlLongTag [] "section" $ loop doc
          loop (Style _ sty txt) = xmlShortTag [] sty $ loopText txt