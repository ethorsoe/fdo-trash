import Network.URL(encString,decString, ok_url)
import System.Posix.Env(getEnv,getEnvDefault)
import System.FilePath.Posix((</>),(<.>),dropExtension)
import System.Directory(getDirectoryContents,doesDirectoryExist)
import Data.Maybe(fromJust,maybe,catMaybes)
import System.Locale(iso8601DateFormat,defaultTimeLocale)
import Text.ParserCombinators.Parsec(parse,many,try,(<|>),string,noneOf,oneOf,many)
import Data.Time(getCurrentTimeZone,getCurrentTime,parseTime,localTimeToUTC,UTCTime)
import Data.Either(partitionEithers)
import Control.Monad(when)
import Data.Algorithm.Diff(getDiff,DI(..))
import Data.List(sort)
--import System.IO(IOMode(..),withFile,hFileSize)
import System.Posix.Files(fileSize,getSymbolicLinkStatus,isRegularFile)

data TrashFile = TrashFile {
    infoPath   :: FilePath,
    dataPath   :: FilePath,
    origPath   :: FilePath,
    deleteTime :: UTCTime,
    totalSize  :: Integer
    } deriving (Show)

headerLine = string "[Trash Info]\n"

dateLine = do
    _ <- string "DeletionDate="
    dateString <- many $ oneOf "0123456789-T:"
    _ <- many $ noneOf "\n"
    _ <- string "\n"
    return (parseTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") dateString) >>=
        maybe (fail "Invalid date format") (return.Left)

nameLine = do
    _ <- string "Path="
    n <- many $ noneOf " \n\t"
    _ <- many $ noneOf "\n"
    _ <- string "\n"
    return (decString False n) >>=
        maybe (fail "Invalid url-encoded filename") (return.Right)

searchHeader = (try headerLine) <|>
    (ignoreLine >> searchHeader)

line = (try nameLine) <|>
    (try dateLine) <|>
    (ignoreLine >> line)

ignoreLine = many (noneOf "\n") >> string "\n"

infoFile timeZone = do
    _ <- searchHeader
    (dates,names) <- fmap partitionEithers $ many line
    when (length dates /= 1 || length names /= 1) $ fail "Exactly one name and date not found."
    return (localTimeToUTC timeZone (head dates), head names) 

genFile riPath rdPath timeZone name = do
    let iPath = riPath </> name <.> "trashinfo"
    let dPath = rdPath </> name
    size <- getPathSize dPath
    parsed <- readFile iPath >>=
         (\x -> return (parse (infoFile timeZone) "" x))
    either (\x -> print iPath >> print x >> return Nothing)
        (\(x,y) -> return.Just $ TrashFile iPath dPath y x size)
        parsed

getOrphans iFiles dFiles = (diffFst diff, diffSnd diff)
    where diff               = getDiff iFiles dFiles
          diffFst ((F,l):xs) = l : diffFst xs
          diffFst []         = []
          diffFst (_:xs)     = diffFst xs
          diffSnd ((S,l):xs) = dropExtension l : diffSnd xs
          diffSnd []         = []
          diffSnd (_:xs)     = diffSnd xs

getPathSize path = do
    isDirectory <- doesDirectoryExist path
    if (isDirectory)
      then do 
        files <- fmap (map (path</>) . filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents path
        fmap sum (mapM getPathSize files)
      else do
        stat <- getSymbolicLinkStatus path
        if (isRegularFile stat)
          then return (fromIntegral $ fileSize stat)
          else return 0


main :: IO ()
main = do
    defaultRoot <- fmap ((</> ".local/share/").fromJust) $ getEnv "HOME"
    rootPath <- fmap (</> "Trash") $ getEnvDefault "XDG_DATA_HOME" defaultRoot
    let iPath = rootPath </> "info"
    let fPath = rootPath </> "files"
    timeZone <- getCurrentTimeZone
    infoFiles <- fmap (sort.filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents iPath
    dataFiles <- fmap (sort.filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents fPath
    let (iExtra,dExtra) = getOrphans infoFiles (sort $ map (<.>"trashinfo") dataFiles)
    print (iExtra,dExtra)
    ayx <- fmap catMaybes $ mapM (genFile iPath fPath timeZone) dataFiles
    print ayx
    
    
--print $ encString False ok_url "a% b"
--print $ fromJust (decString False "c%20d")

