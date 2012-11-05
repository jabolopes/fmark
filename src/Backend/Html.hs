module Backend.Html where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Document


data TagT = ShortT | LongT
            deriving (Eq)


data HtmlDoc = HtmlDoc TagT Element [HtmlDoc]


htmlDocTagT :: HtmlDoc -> TagT
htmlDocTagT (HtmlDoc tagT _ _) = tagT


shortTagTs :: [Element -> Bool]
shortTagTs = [isSpanElement, isParagraphElement]


mkHtmlDoc :: Document -> HtmlDoc
mkHtmlDoc doc = mkHtmlDoc' LongT doc
    where mkHtmlDoc' _ (Document _ el docs) | any (\fn -> fn el) shortTagTs =
              HtmlDoc ShortT el $ map (mkHtmlDoc' ShortT) docs

          mkHtmlDoc' _ (Document _ el []) =
              HtmlDoc ShortT el []

          mkHtmlDoc' ShortT (Document _ el [doc]) =
              HtmlDoc ShortT el [mkHtmlDoc' ShortT doc]

          mkHtmlDoc' LongT (Document _ el [doc]) =
              HtmlDoc (htmlDocTagT htmlDoc) el [htmlDoc]
              where htmlDoc = mkHtmlDoc' LongT doc

          mkHtmlDoc' parentT (Document _ el docs) =
              HtmlDoc LongT el $ map (mkHtmlDoc' LongT) docs


type HtmlM a = State Int a


withIdnM :: HtmlM a -> HtmlM a
withIdnM m =
    do idn <- get
       put $ idn + 2
       val <- m
       put idn
       return val


indentM :: TagT -> HtmlM String -> HtmlM String
indentM ShortT m = m
indentM LongT m =
    do idn <- get
       (replicate idn ' ' ++) <$> m

        
htmlAttributes :: [(String, String)] -> String
htmlAttributes [] = ""
htmlAttributes attrs = ' ':unwords (map (\(id, val) -> id ++ "=\"" ++ val ++ "\"") attrs)


strM :: HtmlM String -> HtmlM String
strM = indentM ShortT


shortTagM :: String -> [(String, String)] -> TagT -> [HtmlM String] -> HtmlM String
shortTagM tag attrs parentTagT [m] =
    concat <$> sequence [indentM parentTagT $ return ("<" ++ tag ++ htmlAttributes attrs ++ ">"),
                         m,
                         return ("</" ++ tag ++ ">")]

shortTagM tag attrs parentTagT ms =
    shortTagM tag attrs parentTagT [concat <$> sequence ms]


longTagM :: String -> [(String, String)] -> TagT -> [HtmlM String] -> HtmlM String
longTagM tag attrs parentTagT [m] =
    concat <$> sequence [indentM parentTagT (return ("<" ++ tag ++ htmlAttributes attrs ++ ">\n")),
                         withIdnM m,
                         return "\n",
                         indentM parentTagT (return ("</" ++ tag ++ ">"))]

longTagM tag attrs parentTagT ms =
   longTagM tag attrs parentTagT [intercalate "\n" <$> sequence ms]


tagM :: String -> [(String, String)] -> TagT -> TagT -> [HtmlM String] -> HtmlM String
tagM tag attrs parentTagT ShortT ms = shortTagM tag attrs parentTagT ms
tagM tag attrs parentTagT LongT ms = longTagM tag attrs parentTagT ms


docToHtml :: Maybe Document -> Document -> String
docToHtml _ doc =
    intercalate "\n" ["<html>", evalState (docToHtml' LongT (mkHtmlDoc doc)) 2, "</html>"]
    where elementTag (Block t) = tagM (show t) []
          elementTag Content = tagM "content" []
          elementTag (Enumeration BulletEnumerationT) = tagM "ul" []
          elementTag (Enumeration NumberEnumerationT) = tagM "ol" []
          elementTag Heading = tagM "h1" []
          elementTag Paragraph = tagM "p" []
          elementTag (Plain str) = \_ _ _ -> strM (return str)
          --elementTag Section = tagM "section" []
          elementTag (Span sty) = tagM sty []

          docToHtml' parentTagT (HtmlDoc tagT el docs) =
             elementTag el parentTagT tagT $ map (docToHtml' tagT) docs