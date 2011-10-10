import Network.FastCGI
import Text.XHtml
import Database.HDBC 
import Database.HDBC.Sqlite3
import CommonStuff

{- 
 - Form for choosing an upload picture.
-}
inputForm :: Html
inputForm = form ! [action "./upload.fcgi", method "post", enctype "multipart/form-data" ]
                <<  (input ! [thetype "file", name "file", size "20"]
                +++ input ! [thetype "submit", value "Upload"])
                +++ "2MB max, jpg, jpeg, gif, png, tiff, bmp"

indexBody :: [String] -> Html
indexBody images = 
        thediv ! [identifier "header"]
                << (h1 << anchor ! [href "index.fcgi"] << "imgcons"
                    +++ h2 << "A simple image upload site."
        +++ thediv ! [identifier "intro-box"] 
                << h3 ! [identifier "intro"]<< "imgcons is a javascript free, anonymous web service for uplading your photos")
        +++ thediv ! [identifier "main"]
                << inputForm
                +++ h4 << "Last six uploaded photos:"
        +++ thediv ! [identifier "gallery"] 

        << map (\imgName -> image ! [src (uploadDirCustom ++ "192x192!" ++ imgName)]) images

        +++ theFooter

{- 
 - creates the final index page for display
-}
indexFile :: (HTML a, HTML a1) => a -> (t -> a1) -> t -> Html
indexFile indexHeader indexBody images = header << indexHeader +++ body << indexBody images

getLastImages :: Int -> IO [[SqlValue]]
getLastImages limit = do
                        conn <- connectSqlite3 "./databaseDir/imageDatabase.db"
                        all <- liftIO $ quickQuery' conn ("SELECT name FROM images order by number desc limit " ++ show limit)  []
                        disconnect conn 
                        return all

showLastImages :: IO [String]
showLastImages = do
                    x <- getLastImages 6
                    let y = concat x
                    let z = (map fromSql y)::[String] 
                    return z  

{- 
 - Main programs, self-explanatory
-}
cgiMain = do 
            lastSixImages <- liftIO $ showLastImages
            output $ renderHtml $ indexFile indexHeader indexBody lastSixImages
 
main = runFastCGI $ handleErrors cgiMain

nSpaceHtml 1  = spaceHtml
nSpaceHtml n  = spaceHtml +++ nSpaceHtml (n-1)



