-- | 'Backend.Xml' is the XML generator backend for 'Document's.
module Backend.Xml where

import Control.Monad.State (State, evalState, get, modify)
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Document


-- | 'XmlState' is the XML generator state that records the current
-- indentation level and a prefix values that decides whether XML tags
-- should be indented.
data XmlState = XmlState Int Bool

-- | 'XmlM' is the type of the XML generator 'Monad'.
type XmlM a = State XmlState a


-- | 'getIdn' is a 'Monad' with the current indentation level.
getIdn :: XmlM Int
getIdn =
    do XmlState idn _ <- get
       return idn


-- | 'putIdn' @idn@ is a 'Monad' that modifies the current indentation
-- level.
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


-- | 'getPrefix' is a 'Monad' with the current prefix.
getPrefix :: XmlM Bool
getPrefix =
    do XmlState _ pre <- get
       return pre


-- | 'putPrefix' @pre@ is a 'Monad' that modified the current prefix.
putPrefix :: Bool -> XmlM ()
putPrefix pre =
    modify $ \(XmlState idn _) -> XmlState idn pre


-- | 'withPrefix' @pre m@ is a 'Monad' that modified the current
-- prefix for 'Monad' @m@.
withPrefix :: Bool -> XmlM a -> XmlM a
withPrefix pre m =
    do pre' <- getPrefix
       putPrefix pre
       val <- m
       putPrefix pre'
       return val


xmlIndent :: XmlM String -> XmlM String
xmlIndent m =
  do idn <- getIdn
     getPrefix >>= pre idn
  where pre idn True = m >>= return . (++) (replicate idn ' ')
        pre _ False = m
        

xmlAttributes :: [(String, String)] -> String
xmlAttributes [] = ""
xmlAttributes attrs = ' ':unwords (map (\(id, val) -> id ++ "=\"" ++ val ++ "\"") attrs)


xmlStr :: XmlM String -> XmlM String
xmlStr = xmlIndent


xmlShortTag :: String -> [(String, String)] -> XmlM String -> XmlM String
xmlShortTag tag attrs m =
  concat <$> sequence [xmlIndent (return ("<" ++ tag ++ xmlAttributes attrs ++ ">")),
                       withPrefix False m,
                       return ("</" ++ tag ++ ">")]
  
  
xmlShortTags :: String -> [(String, String)] -> [XmlM String] -> XmlM String
xmlShortTags tag attrs ms = xmlShortTag tag attrs $ concat <$> sequence ms


xmlLongTag :: String -> [(String, String)] -> XmlM String -> XmlM String
xmlLongTag tag attrs m =
   getPrefix >>= pre
   where pre True = concat <$> sequence [xmlIndent (return ("<" ++ tag ++ xmlAttributes attrs ++ ">\n")),
                                         withIdn m,
                                         return "\n",
                                         xmlIndent (return ("</" ++ tag ++ ">"))]
         pre False = xmlShortTag tag attrs m


xmlLongTags :: String -> [(String, String)] -> [XmlM String] -> XmlM String
xmlLongTags tag attrs ms =
   getPrefix >>= pre
   where pre True = xmlLongTag tag attrs $ intercalate "\n" <$> sequence ms
         pre False = xmlLongTag tag attrs $ concat <$> sequence ms


tag :: String -> [(String, String)] -> [XmlM String] -> XmlM String
tag t attrs [m] = xmlShortTag t attrs m
tag t attrs ms = xmlLongTags t attrs ms


-- | 'docToXml' @mstyle doc@ formats a styled 'Document' @doc@ into a
-- XML 'String', where @mstyle@ specifies the style 'Document' used to
-- stylize @doc@.
docToXml :: Maybe Document -> Document -> String
docToXml _ doc =
    intercalate "\n" ["<xml>", evalState (docToXml' doc) (XmlState 2 True), "</xml>"]
    where elementTag :: Element -> ([XmlM String] -> XmlM String)
          elementTag (Block t) = xmlLongTags (show t) []
          elementTag Content = xmlLongTags "content" []
          elementTag (Enumeration t) = xmlLongTags "enumeration" [("type", show t)]
          elementTag Heading = xmlLongTags "heading" []
          elementTag Paragraph = xmlShortTags "paragraph" []
          elementTag (Plain str) = const $ xmlStr $ return str
          --elementTag Section = xmlLongTags "section" []
          elementTag (Span sty) = xmlShortTags sty []

          docToXml' :: Document -> XmlM String
          docToXml' (Document _ _ el docs) =
             elementTag el (map docToXml' docs)