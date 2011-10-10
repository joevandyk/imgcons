import Network.FastCGI
import Text.XHtml
import qualified Data.ByteString.Lazy.Char8 as BS 
import Control.Monad (liftM)
import Data.Maybe (fromJust, fromMaybe)
import System.Random 
import Database.HDBC 
import Database.HDBC.Sqlite3
import Data.Time
import System.Cmd
import Control.Exception
import CommonStuff

{-
 - Generates a random name of length 20 containing chars from a-z, checks if
 - the database already contains that name, if calls itself again to do another
 - generation and check until it finds a unique name.
-}
getRandomName ending conn = do  
                    gen <- getStdGen  
                    let a = take 20 (randomRs ('a','z') gen) 
                    newStdGen
                    isInDatabase <- liftIO $ checkDatabase (a ++ ending) conn
                    if (not isInDatabase)
                        then return (a ++ ending)
                        else getRandomName ending conn

{-
 - Checks if the file ending ends in one of six allowed endings and returns
 - that ending.
-}
checkInputFile :: [Char] -> Maybe [Char]
checkInputFile inputFile
    | ending == ".jpg" = Just ".jpg"
    | ending == "jpeg" = Just ".jpeg"
    | ending == ".gif" = Just ".gif"
    | ending == ".png" = Just ".png"
    | ending == "tiff" = Just ".tiff"
    | ending == ".bmp" = Just ".bmp"
    | otherwise = Nothing
    where ending = getFileEnding inputFile

{-
 - Returns a string containing the last four characters of the input string.
-}

getFileEnding :: [a] -> [a]
getFileEnding inputFile = reverse $ take 4 $ reverse inputFile

{-
 - Saves the original image to uploadDir and the resized image to uploadDir192,
 - updates the database to include the image name, uploader ip address and time
 - of upload.
 - Returns a html paragraph.
-}
saveFile :: (MonadIO m, MonadCGI m) => t -> [Char] -> m Html
saveFile n ending = do 
                conn <- liftIO $ connectSqlite3 "./databaseDir/imageDatabase.db"             
                fileContents <- liftM (fromMaybe $ BS.pack "") $ getInputFPS "file"
                addressIP    <- remoteAddr
                randomName   <- liftIO $ getRandomName ending conn
                time         <- liftIO getCurrentTime                
--              liftIO $ databaseCreation conn
                let fileName  = uploadDir ++ randomName
                liftIO $ do 
                            databaseInsert randomName addressIP time conn
                            BS.writeFile fileName fileContents
                            system ("convert -resize 192x192! -quality 80 -strip " ++ fileName ++ " " ++ uploadDirCustom  ++ "192x192!" ++ randomName ++ " &")
                            disconnect conn
                let scc =  paragraph << ("Your image has been uploaded.")
                                     +++ paragraph << "Redirecting......"
                return $ successPage fileName scc


{-
 - Various completed pages, self-explanatory.
-}
errorPage :: HTML a => a -> Html
errorPage b = header << indexHeaderRedirect "index.fcgi" +++ body << b

successPage :: HTML a => [Char] -> a -> Html
successPage a b = header << indexHeaderRedirect ("view.fcgi?file=" ++ (drop 12 a)) +++ body << b

page :: HTML a => [Char] -> a -> Html
page t b = header << indexHeaderRedirect t +++ body << b

{-
 - Creates the initial database. Runs only once when uncommented in function
 - saveFile. Not needed anymore since makeAll.sh creates the initial database.
-}
databaseCreation conn = do
                    run conn "CREATE TABLE images (number INTEGER PRIMARY KEY, name VARCHAR(40), resizedto VARCHAR(40), ipaddress VARCHAR(20), time VARCHAR(40))" []
                    commit conn


{-
 - Inserts iamge name, uploader ip adress and upload time to the database.
-}
databaseInsert name ipAdress time conn = do
                                        run conn "INSERT INTO images VALUES (?,?,?,?,?)" [SqlNull, toSql name, toSql "192x192!", toSql ipAdress, toSql time]
                                        commit conn

{-
 - Checks if a name already exists in the database.
-}
checkDatabase name conn = do
                        a <- liftIO $ quickQuery conn "SELECT number FROM images WHERE name = ?" [toSql name]
                        return (a /= [])

{- 
 - Main programs, self-explanatory
-}
cgiMain = do 
            mn <- getInputFilename "file"
            let fileEnding = checkInputFile $ myfromJust mn
            if fileEnding == Nothing 
                then output . renderHtml $ errorPage "That file is not an image. Try again."
                else do
                        success <- saveFile (myfromJust mn) $ myfromJust fileEnding
                        output . renderHtml $ success
                        

main = runFastCGI $ handleErrors cgiMain



