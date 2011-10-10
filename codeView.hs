import Network.FastCGI
import Text.XHtml
import Database.HDBC 
import Database.HDBC.Sqlite3
import CommonStuff


{-
 - Self explanatory.
-}
removeEx :: [Char] -> [Char]
removeEx x = filter (/='!') x

{-
 -  Form for resizing an image.
-}
resizeImageForm :: String -> Html
resizeImageForm fileName = form ! [action "./resize.fcgi", method "get"] 
                        << (select ! [identifier "resizeto", name "resizemenu"]
                            << selectOptions 
                            +++ input ! [thetype "text", name "customtext", enctype "text",size "36"]
                            +++ input ! [thetype "hidden", name "imagename", enctype "text", size "1", value fileName]
                            +++ input ! [thetype "submit", value "Resize"])
{-
 - Self explanatory.
-}
valueT = paragraph ! [identifier "message"] << "Resize menu not available...."

{-
 - List of resize options. 
-}
selectOptions :: Html
selectOptions =     option ! [value "custom"]<< "Custom"
                +++ option ! [value "16x16"] << "16x16 pixels"
                +++ option ! [value "24x24"] << "24x24 pixels"
                +++ option ! [value "32x32"] << "32x32 pixels"
                +++ option ! [value "48x48"] << "48x48 pixels"
                +++ option ! [value "64x64"] << "64x64 pixels"
                +++ option ! [value "72x72"] << "72x72 pixels"
                +++ option ! [value "96x96"] << "96x96 pixels"
                +++ option ! [value "128x128"] << "128x128 pixels"
                +++ option ! [value "192x192"] << "192x192 pixels"
                +++ option ! [value "256x256"] << "256x256 pixels"


{-
 - Makes the html for the page body, input is file name and html paragraphs. Resize form is commented since resize.fcgi is not complete.
-}
imageLinksUrls :: HTML b => [Char] -> b -> Html
imageLinksUrls file additionalImageResizes = 
    thediv ! [identifier "header"]
                << (h1 << anchor ! [href "index.fcgi"] << "imgcons"
                    +++ h2 << "A simple image upload site."
        +++ thediv ! [identifier "intro-box"] 
                << h3 ! [identifier "intro"] 
                        << ("If you would like to resize the image please use the form below (not available)."))
        +++ (anchor ! [href (uploadDir ++ file) ] 
            << image ! [src (uploadDirCustom ++ "192x192!" ++ file), identifier "uploadedimageseize", alt "image"])
        +++ thediv ! [identifier "fileslist"]
            << (h3 << "Image URLs:"
                            +++ (paragraph ! [theclass "sizename"] << ("Original size:")
                            +++ input ! [theclass "sizebox", name "", size "43" ,value ("www.imgcons.com" ++ (drop 1 uploadDir) ++ file) ] 
                            +++ anchor ! [theclass "arrow", href (uploadDir ++ file) ] << "→")
                    +++ additionalImageResizes)
--           +++ h3 << "Resize image to:" 
--            +++ resizeImageForm file
        +++ valueT 
        +++ theFooter



{-
 - Self explanatory.
-}
splitAtComma :: [Char] -> [[Char]]
splitAtComma [] = [""]
splitAtComma (x:xs)
	| x == ',' = "" : rest
	| otherwise = (x : head rest) : tail rest
	where rest = splitAtComma xs


{-
 - Creates a html paragraph for the provided image file and size.
-}
createImageResizeHtml :: [Char] -> [Char] -> Html
createImageResizeHtml file resize =  paragraph ! [theclass "sizename"] << (removeEx resize ++ " pixels:")
                                     +++ input ! [theclass "sizebox", name "", size "43" ,value ("www.imgcons.com" ++ (drop 1 uploadDirCustom) ++ resize ++ file) ] 
                                     +++ anchor ! [theclass "arrow", href (uploadDirCustom ++ resize ++ file) ] << "→" 



{-
 - Reads the resizedto field from the database for the provided file name, returns the list of the sizes the file was resized to.
-}
readCustomImageResizeSizes file conn = do
                                            a <- liftIO $ quickQuery' conn "SELECT resizedto FROM images WHERE name = ?" [toSql file]
                                            return $ splitAtComma ((fromSql $ (concat a) !! 0):: String)


{-
 - Reads the list of resize sizes for the provided file name and returns a list of html paragraphs for each resize size.
-}
createAdditionalImageResizeSizesHtml file conn = do  
						                             a <- liftIO $ readCustomImageResizeSizes file conn
                        						     return $ map (createImageResizeHtml file) a


{-
 - Checks if the input file name is in the database, if it isn't it returns an error page, 
 - if it is in the database it creates the html page for the image and returns it.
-} 
showFile :: [Char] -> IO Html
showFile file = do
                    conn <- connectSqlite3 "./databaseDir/imageDatabase.db"
                    a    <- liftIO $ quickQuery' conn "SELECT name FROM images WHERE name = ?" [toSql file]
                    if (a == [])
                        then do disconnect conn
                                return $ errorPage (paragraph << "image not found")
                        else do
                                additionalImageResizes <- createAdditionalImageResizeSizesHtml file conn
                                disconnect conn
                                return (imageLinksUrls file additionalImageResizes)

{-
 - Self explanatory.
-}
errorPage   :: HTML a => a -> Html
errorPage b   = header << indexHeaderRedirect "index.fcgi" +++ body << b

successPage :: HTML a => a -> Html
successPage b = header << indexHeader +++ body << b

{-
 - Checks if the input file is 24 or 25 chars long, which is the length of file name that upload.fcgi created and stored in database.
-}
checkInputFile :: [a] -> Bool                                                   
checkInputFile file = length file `elem` [24,25] 

{-
 - Main program, it expects a file as input, checks it, renders an error if check failed, otherwise it creates an html page for the image.
-}
cgiMain = do 
            mn <- getInput "file"
            let passesFileCheck = checkInputFile $ myfromJust mn
            if (not passesFileCheck) 
                then output . renderHtml $ errorPage (paragraph << "Image not found/deleted. Redirecting to index page.")
                else do
                        success <- liftIO $ showFile (myfromJust mn)
                        output . renderHtml $ successPage success

main = runFastCGI $ handleErrors cgiMain


