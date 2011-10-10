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

{- 
 - creates an index body, input is a 6-tuple containing image names that are displayed
-}
indexBody :: ([Char], [Char], [Char], [Char], [Char], [Char]) -> Html
indexBody (first,second,third,fourth,fifth,sixth) = 
        thediv ! [identifier "header"]
                << (h1 << anchor ! [href "index.fcgi"] << "imgcons"
                    +++ h2 << "A simple image upload site."
        +++ thediv ! [identifier "intro-box"] 
                << h3 ! [identifier "intro"]<< "imgcons is a javascript free, anonymous web service for uplading your photos")
        +++ thediv ! [identifier "main"]
                << inputForm
                +++ h4 << "Last six uploaded photos:"
        +++ thediv ! [identifier "gallery"] 
                << (thediv ! [theclass "screenshots"]
                    << ulist << (li << anchor ! [href ("view.fcgi?file=" ++ first)] 
                                            << image ! [src (uploadDirCustom ++ "192x192!" ++ first), alt "image1"] 
                             +++ li << anchor ! [href ("view.fcgi?file=" ++ fourth)] 
                                            << image ! [src (uploadDirCustom ++ "192x192!" ++ fourth), alt "image4"])
                +++ thediv ! [theclass "screenshots"]
                    << ulist << (li << anchor ! [href ("view.fcgi?file=" ++ second)] 
                                            << image ! [src (uploadDirCustom ++ "192x192!" ++ second), alt "image2"] 
                             +++ li << anchor ! [href ("view.fcgi?file=" ++ fifth)] 
                                            << image ! [src (uploadDirCustom ++ "192x192!" ++ fifth), alt "image5"])
                +++ thediv ! [theclass "screenshots"]
                    << ulist << (li << anchor ! [href ("view.fcgi?file=" ++ third)] 
                                            << image ! [src (uploadDirCustom ++ "192x192!" ++ third), alt "image3"] 
                             +++ li << anchor ! [href ("view.fcgi?file=" ++ sixth)] 
                                            << image ! [src (uploadDirCustom ++ "192x192!" ++ sixth), alt "image6"]))
        +++ theFooter

{- 
 - creates the final index page for display
-}
indexFile :: (HTML a, HTML a1) => a -> (t -> a1) -> t -> Html
indexFile indexHeader indexBody images = header << indexHeader +++ body << indexBody images

{- 
 - returns a list of strings containing filenames of images under SqlValue retrived from the database 
 - input:   database
 - output:  [[SqlByteString "filename0.jpg"],.........,[SqlByteString "filename5.jpg"]]
-}
getLastSixImages :: IO [[SqlValue]]
getLastSixImages = do
                        conn <- connectSqlite3 "./databaseDir/imageDatabase.db"
                        all <- liftIO $ quickQuery' conn "SELECT name FROM images WHERE number >= (SELECT max(number-5) FROM images) AND number <= (SELECT max(number) FROM images) ORDER BY number DESC;" []
                        disconnect conn 
                        return all

{-
 - input: [[SqlByteString "filename0.jpg"],.........,[SqlByteString "filename5.jpg"]]
 - output: (filename0.jpg, ..... , filename5.jpg)
-}
showLastImages :: IO (String, String, String, String, String, String)
showLastImages = do
                    x <- getLastSixImages
                    let y = concat x
                    let z = (map fromSql y)::[String] 
                    let v = (a,b,c,d,e,f) 
                                where a = z !! 0
                                      b = z !! 1
                                      c = z !! 2
                                      d = z !! 3
                                      e = z !! 4
                                      f = z !! 5             
                    return v

{- 
 - Main programs, self-explanatory
-}
cgiMain = do 
            lastSixImages <- liftIO $ showLastImages
            output $ renderHtml $ indexFile indexHeader indexBody lastSixImages
 
main = runFastCGI $ handleErrors cgiMain

nSpaceHtml 1  = spaceHtml
nSpaceHtml n  = spaceHtml +++ nSpaceHtml (n-1)



