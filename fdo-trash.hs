import System.Environment(getArgs,getProgName)
import System.Console.GetOpt(getOpt,ArgOrder(..),OptDescr(..),ArgDescr(..))
import System.FilePath.Posix((</>),(<.>))
import System.Directory(getDirectoryContents)
import Data.Maybe(catMaybes)
import Data.Time(getCurrentTimeZone,getCurrentTime,utcToLocalTime)
import Data.List(sort)
import Freedesktop.Trash(TrashFile(..),genTrashFile,trashGetOrphans,getTrashPaths,formatTrashDate,encodeTrashPath)


actions =
    [ ("purge", fdoPurge)
    , ("rm", fdoRm)
    , ("unrm", fdoUnRm)
    ]

rmFile realPath trashFile = do
    timeZone <- getCurrentTimeZone
    putStrLn "rename:"
    print (encodeTrashPath realPath)
    print trashFile
    print $ formatTrashDate (utcToLocalTime timeZone $ deleteTime trashFile)

data RmFlag = Version | Time Int
    deriving(Show)

rmOptions =
    [ Option ['V'] ["version"] (NoArg Version) "show version number"
    , Option ['t'] ["time"] (ReqArg (Time . read) "blah") "Specify time offset"
    ]

fdoRm args = do
    now <- getCurrentTime
    let (opts, realArgs, _) = getOpt Permute rmOptions args
        time = now
    print opts
    (iPath,fPath) <- getTrashPaths
    let file = TrashFile
            (iPath </> head args)
            (fPath </> head args)
            (head realArgs)
            time
            0
    rmFile (head realArgs) file

fdoPurge args = do
    (iPath,fPath) <- getTrashPaths
    timeZone <- getCurrentTimeZone
    infoFiles <- fmap (sort.filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents iPath
    dataFiles <- fmap (sort.filter (\x -> x /= ".." && x /= ".")) $ getDirectoryContents fPath
    let (iExtra,dExtra) = trashGetOrphans infoFiles (sort $ map (<.>"trashinfo") dataFiles)
    print (iExtra,dExtra)
    ayx <- fmap catMaybes $ mapM (genTrashFile iPath fPath timeZone) dataFiles
    print args
    print ayx

fdoUnRm args = do
    putStrLn "TODO"

main :: IO ()
main = do
    args <- getArgs
    exe <- getProgName
    let thisAction = maybe
            ( if (null args)
                then Nothing
                else maybe
                    (Nothing)
                    (\x -> Just (tail args, x))
                    (lookup (args !! 0) actions) )
            (\x -> Just (args,x))
            (lookup (drop 4 exe) actions)

    maybe
        (putStrLn "No action specified")
        (\(a,f) -> f a)
        thisAction

