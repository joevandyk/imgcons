module CommonStuff 
(
    media,
    onFocus,
    uploadDir,
    uploadDirCustom,
    myfromJust,
    indexHeader,
    indexHeaderRedirect,
    theFooter
)
where

import Text.XHtml
import Data.Maybe

{- 
 - Html attribute media and onFocus are undefined in Text.XHtml so we define them here.
-}
media :: String -> HtmlAttr
media   = strAttr "media"

onFocus :: String -> HtmlAttr
onFocus = strAttr "onFocus"

{- 
 - Upload directories, uploadDir is for original size images, uploadDirCustom for images resized to 192x192... 
-}
uploadDir :: [Char]
uploadDir         = "./uploadDir/"

uploadDirCustom :: [Char]
uploadDirCustom   = "./uploadDirCustom/"

{-
 - Because fromJust Nothing returns a error.
-
- myfromJust :: Maybe [Char] -> [Char]
- myfromJust (Just a) = a
- myfromJust Nothing  = ""
-}
myfromJust = fromMaybe ""

{-
 - Index header.
-}
indexHeader :: Html
indexHeader = thelink ! [href "default.css", rel "stylesheet", thetype "text/css", media "screen"] << "" 
                    +++ meta ! [httpequiv "content-type", content "text/html; charset=UTF-8"]
                    +++ meta ! [name "keywords", content "imgcons, images, pictures, image upload"]
                    +++ meta ! [name "description", content "A free, simple web image upload service."]
                    +++ thetitle << "imgcons image share"

{-
 - Index header used to redirect to another page.
-}
indexHeaderRedirect :: [Char] -> Html
indexHeaderRedirect redirectTo = thelink ! [href "default.css", rel "stylesheet", thetype "text/css", media "screen"] << "" 
                            +++ meta ! [httpequiv "content-type", content "text/html; charset=UTF-8"]
                            +++ meta ! [name "keywords", content "imgcons, images, pictures, image upload"]
                            +++ meta ! [name "description", content "A free, simple web image upload service."]
                            +++ meta ! [httpequiv "REFRESH", content ("3,url=" ++ redirectTo)] 
                            +++ thetitle << "imgcons image share"

{-
 - Footer.
-}
theFooter = thediv ! [identifier "footer"]
                            << ("imgcons.com coded by " +++ anchor ! [href "http://www.imgcons.com/about.html"] << "ostochast" 
                                +++ " in " +++ anchor ! [href "http://www.haskell.org"] << "Haskell")

