module Freedesktop.Trash (
    TrashFile(..),
    trashGetOrphans,
    trashGetFiles,
    trashSortFiles,
    trashRestore,
    genTrashFile,
    moveToTrash,
    getPathSize,
    formatTrashDate,
    encodeTrashPath,
    expungeTrash,
    getTrashPaths
) where


import Network.URL(encString,decString, ok_url)
import System.Posix.Env(getEnv,getEnvDefault)
import System.FilePath.Posix((</>),(<.>),dropExtension,splitExtension)
import System.Directory(getDirectoryContents,removeDirectoryRecursive)
import Data.Maybe(fromJust,catMaybes)
import System.Locale(iso8601DateFormat,defaultTimeLocale)
import Text.ParserCombinators.Parsec(parse,many,try,(<|>),string,noneOf,oneOf,many)
import Data.Time(getCurrentTimeZone,parseTime,localTimeToUTC,UTCTime,formatTime,utcToLocalTime,FormatTime)
import Data.Either(partitionEithers)
import Control.Monad(when)
import Data.Algorithm.Diff(getDiff,DI(..))
import Data.List(sort)
import System.Posix.Files(fileSize,getSymbolicLinkStatus,isRegularFile,isDirectory,rename,removeLink,fileExist)
import qualified System.IO.Error as E

data TrashFile = TrashFile {
    infoPath   :: FilePath,
    dataPath   :: FilePath,
    origPath   :: FilePath,
    deleteTime :: UTCTime,
    totalSize  :: Integer
    } deriving (Show)

trashHeaderString = "[Trash Info]\n"

headerLine = string trashHeaderString

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

genTrashFile riPath rdPath timeZone name = do
    let iPath = riPath </> name <.> "trashinfo"
    let dPath = rdPath </> name
    size <- getPathSize dPath
    parsed <- readFile iPath >>=
         (\x -> return (parse (infoFile timeZone) "" x))
    either (\x -> print iPath >> print x >> return Nothing)
        (\(x,y) -> return.Just $ TrashFile iPath dPath y x size)
        parsed

trashSortFiles iPath fPath= do
    timeZone <- getCurrentTimeZone
    iFiles <- fmap (sort.filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents iPath
    dataFiles <- fmap (sort.filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents fPath
    let  dFiles = sort $ map (<.>"trashinfo") dataFiles
         diff   = getDiff iFiles dFiles
    files <- fmap catMaybes $ mapM (genTrashFile iPath fPath timeZone) (diffBth diff)
    return (files, (diffFst diff, map dropExtension $ diffSnd diff))
    where diffFst ((F,l):xs) = l : diffFst xs
          diffFst []         = []
          diffFst (_:xs)     = diffFst xs
          diffSnd ((S,l):xs) = dropExtension l : diffSnd xs
          diffSnd []         = []
          diffSnd (_:xs)     = diffSnd xs
          diffBth ((B,l):xs) = dropExtension l : diffBth xs
          diffBth []         = []
          diffBth (_:xs)     = diffBth xs

trashGetOrphans iPath fPath = fmap snd $ trashSortFiles iPath fPath

trashGetFiles iPath fPath = fmap fst $ trashSortFiles iPath fPath

formatTrashDate :: FormatTime a => a -> String
formatTrashDate = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")

encodeTrashPath = encString False ok_url

doRemoveFile file = E.catch (removeDirectoryRecursive file)
    (\_ -> E.try (removeLink file) >> return ())
    >> return ()

expungeTrash file = do
    doRemoveFile (dataPath file)
    doRemoveFile (infoPath file)

trashRestore file condName = do
    rename (dataPath file) $ maybe (origPath file) id condName
    expungeTrash file

getPathSize path = do
    stat <- getSymbolicLinkStatus path
    if (isDirectory stat)
      then do
        files <- fmap (map (path</>) . filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents path
        fmap sum (mapM getPathSize files)
      else do
        if (isRegularFile stat)
          then return (fromIntegral $ fileSize stat)
          else return 0

getTrashPaths = do
    defaultRoot <- fmap ((</> ".local/share/").fromJust) $ getEnv "HOME"
    rootPath <- fmap (</> "Trash") $ getEnvDefault "XDG_DATA_HOME" defaultRoot
    let iPath = rootPath </> "info"
    let fPath = rootPath </> "files"
    return (iPath,fPath)

getFreeTrashSlot :: TrashFile -> Maybe Int -> IO TrashFile
getFreeTrashSlot trashFile Nothing = do
    iExists <- fileExist $ infoPath trashFile
    dExists <- fileExist $ dataPath trashFile
    if (iExists || dExists)
      then getFreeTrashSlot trashFile (Just 0)
      else return trashFile
getFreeTrashSlot trashFile (Just index) = do
    let (iPath',iExt2)     = splitExtension $ infoPath trashFile
        (iPath,iExt1)      = splitExtension $ iPath'
        (dPath,dExt)       = splitExtension $ dataPath trashFile
        iTry               = iPath <.> show index <.> iExt1 <.> iExt2
        dTry               = dPath <.> show index <.> dExt

    iExists <- fileExist iTry
    dExists <- fileExist dTry
    if (iExists || dExists)
      then getFreeTrashSlot trashFile (Just $ index + 1)
      else return trashFile{infoPath=iTry, dataPath=dTry}

doMoveToTrash trashFile = do
    timeZone <- getCurrentTimeZone
    writeFile (infoPath trashFile)
        (  trashHeaderString
        ++ "Path=" ++ (encodeTrashPath $ origPath trashFile) ++ "\n"
        ++ "DeletionDate=" ++ formatTrashDate (utcToLocalTime timeZone $ deleteTime trashFile) ++ "\n"
        )
    rename (origPath trashFile) (dataPath trashFile)

moveToTrash trashFile = do
    yes <- fileExist $ origPath trashFile
    target <- getFreeTrashSlot trashFile Nothing
    when yes $ doMoveToTrash target

