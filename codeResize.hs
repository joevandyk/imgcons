import Network.FastCGI
import Text.XHtml
import qualified Data.ByteString.Lazy as BS 
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import System.Random 
import Database.HDBC 
import Database.HDBC.Sqlite3
import Data.Time
import System.Cmd
import Control.Exception
import Data.List
import Data.List.Split


uploadDir         = "./uploadDir/"
uploadDirCustom   = "./uploadDirCustom/"

main = runFastCGI $ handleErrors cgiMain

cgiMain = do 
            input01 <- getInput "resizemenu"
            input02 <- getInput "customtext"
            input03 <- getInput "imagename"
            let direction01     = checkMenu $ myfromJust input01
            let direction02     = checkResizeInput (myfromJust input01) (myfromJust input02)
            let image           = myfromJust input03
            let imageName       = uploadDir ++ image
            if (direction01 /= "")
                then do convertImage imageName (uploadDirCustom ++ direction01 ++ image) (myfromJust $ input01)
                        liftIO $ databaseInsertSize image direction01
                        output $ renderHtml $ successPage $ paragraph << ("You chose " ++ direction01)
                else if (direction02 /= "")
                     then do convertImage imageName (uploadDirCustom ++ direction02 ++ image) (myfromJust $ input02)
                             liftIO $ databaseInsertSize image direction02
                             output $ renderHtml 
                                    $ successPage
                                    $ redirectPage (paragraph << "The image is being resized. Please wait a few moments if it is not immediately available."
                                                                +++ paragraph << "Redirecting...")
                                                                $ "./view.fcgi?file=" ++ image
                else output $ renderHtml $ redirectPage (paragraph << "Invalid resize size. Must be in the form of YYYYxYYYY where Ys are numbers 0 to 9."
                                                                +++ paragraph << "Redirectiong...")
                                                                    $ ("./view.fcgi?file=" ++ image)

databaseInsertSize2 name size = do
                                conn <- connectSqlite3 "./databaseDir/imageDatabase.db"
                                run conn "UPDATE images SET resizedto = resizedto || ? WHERE name = ?" [toSql ("," ++ size), toSql name] 
                                commit conn
                                disconnect conn

databaseInsertSize name size = do
                                    conn <- connectSqlite3 "./databaseDir/imageDatabase.db"
                                    a    <- liftIO $ quickQuery' conn "SELECT resizedto FROM images WHERE name = ?" [toSql name]
                                    let b = intercalate "," $ map addEx $ mysort $ map removeEx $ splitOn "," (((fromSql $ (concat a) !! 0):: String) ++ "," ++ size)
                                    run conn "UPDATE images SET resizedto = ? WHERE name = ?" [toSql b, toSql name]
                                    commit conn
                                    disconnect conn



media         = strAttr "media"
successPage b = header << indexHeader +++ body << b
redirectPage b r = header << redirectHeader r +++ body << b

indexHeader   = header 
                << (thelink ! [href "default.css", rel "stylesheet", thetype "text/css", media "screen"] << "" 
                    +++ meta ! [httpequiv "content-type", content "text/html; charset=UTF-8"]
                    +++ meta ! [name "keywords", content "imgcons, images, pictures, image upload"]
                    +++ meta ! [name "description", content "A free, simple web image upload service."])

redirectHeader redirectTo = header 
                << (thelink ! [href "default.css", rel "stylesheet", thetype "text/css", media "screen"] << "" 
                    +++ meta ! [httpequiv "content-type", content "text/html; charset=UTF-8"]
                    +++ meta ! [name "keywords", content "imgcons, images, pictures, image upload"]
                    +++ meta ! [name "description", content "A free, simple web image upload service."])
                    +++ meta ! [httpequiv "REFRESH", content ("5,url=" ++ redirectTo)] 



convertImage imagePath imageDirection imageResizeSize = do
    liftIO $ system ("convert -resize " ++ imageResizeSize ++ "!" ++ " -quality 80 -strip " ++ imagePath ++ " " ++ imageDirection ++ " &") 


addEx a = a ++ "!"

myfromJust (Just a) = a
myfromJust Nothing  = ""

checkMenu name
    | name == "16x16"    = "16x16!"
    | name == "24x24"    = "24x24!"
    | name == "32x32"    = "32x32!"
    | name == "48x48"    = "48x48!"
    | name == "64x64"    = "64x64!"
    | name == "72x72"    = "72x72!"
    | name == "96x96"    = "96x96!"
    | name == "128x128"  = "128x128!"
    | name == "192x192"  = "192x192!"
    | name == "256x256"  = "256x256!"
    | otherwise = ""



dropPercentage [] = []
dropPercentage (x:xs) 
    | '%' `elem` x  = dropPercentage xs
    | otherwise = x : dropPercentage xs

dropPixelage [] = []
dropPixelage (x:xs) 
    | 'x' `elem` x = dropPixelage xs
    | otherwise = x : dropPixelage xs

giveEx x = x ++ "!"

mysort = map (intercalate "x" . myshow) . sort . map (readAsInt . splitOn "x") 
readAsInt [x,y] = [read x :: Int, read y:: Int]
myshow    [x,y] = [show x, show y]

removeEx [] = [] 
removeEx (x:xs)
    | x /= '!'  = x : removeEx xs
    | otherwise = removeEx xs

removeEx2 [] = []
removeEx2 (x:xs)
    | x == '!' ||  x == '%' = removeEx2 xs
    | otherwise = x : removeEx2 xs

splitAtComma [] = [""]
splitAtComma (x:xs)
	| x == ',' = "" : rest
	| otherwise = (x : head rest) : tail rest
	where rest = splitAtComma xs

checkIfGoodInput []     = False
checkIfGoodInput [x,[]] = False
checkIfGoodInput [[],y] = False
checkIfGoodInput [x,y]
    | length x <= 4 && isAllNumbers x && length y <= 4 && and isAllNumbers y = True
    | otherwise = False

isAllNumbers [] = []
isAllNumbers (x:xs)
    | x `elem` "0123456789" = True : isAllNumbers xs
    | otherwise = False : isAllNumbers xs

checkResizeInput menuChoice resizeSize
    | menuChoice == "custom" && (length resizeSize <= 9) && (length resizeSize > 0) && checkIfGoodInput (mysplitOnX resizeSize) = resizeSize ++ "!"
    | otherwise  = ""


mysplitOnX x
    | length (splitOn "x" x) < 2 = ["",""]
    | otherwise = splitOn "x" x
