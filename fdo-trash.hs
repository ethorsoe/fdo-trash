import System.Environment(getArgs,getProgName)
import System.Console.GetOpt(getOpt,ArgOrder(..),OptDescr(..),ArgDescr(..),usageInfo)
import System.FilePath.Posix((</>),isAbsolute,takeFileName)
import Data.Time(getCurrentTimeZone,getCurrentTime,utcToLocalTime)
import Data.List(sort,intercalate)
import Freedesktop.Trash(TrashFile(..),genTrashFile,trashGetOrphans,getTrashPaths,formatTrashDate,encodeTrashPath,trashGetFiles,trashRestore)
import Control.Monad(when)
import System.Exit(exitSuccess)

actions =
    [ ("purge", fdoPurge)
    , ("rm", fdoRm)
    , ("unrm", fdoUnRm)
    ]

--compilerOpts :: [String] -> IO (Options, [String])
parseOpts defaultOptions options exe argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: " ++ exe ++ " [OPTION...] parameters..."

--Rm
rmFile realPath trashFile = do
    timeZone <- getCurrentTimeZone
    putStrLn "rename:"
    print (encodeTrashPath realPath)
    print trashFile
    print $ formatTrashDate (utcToLocalTime timeZone $ deleteTime trashFile)

data RmOptions = RmOptions
    { rmTimeOffset :: Double
    , rmVersion    :: Bool
    , rmHelp       :: Bool
    , rmTrash      :: Maybe String
    } deriving(Show)

rmDefaults = RmOptions
    { rmTimeOffset = 0
    , rmVersion = False
    , rmHelp = False
    , rmTrash = Nothing
    }

rmOptions =
    [ Option ['V'] ["version"] (NoArg (\opts -> opts{rmVersion=True})) "Show version number"
    , Option ['h'] ["help"] (NoArg (\opts -> opts{rmHelp=True})) "Print help"
    , Option ['t'] ["time"] (ReqArg  (\secs opts -> opts{rmTimeOffset=read secs}) "secs")
        ("Specify time offset, default: " ++ (show $ rmTimeOffset rmDefaults))
    , Option ['T'] ["trash-path"] (ReqArg (\path opts -> opts{rmTrash=Just path}) "path")
        "Override Trash path autodetection."
    ]

fdoRm args = do
    (myOpts, realArgs) <- parseOpts rmDefaults rmOptions "fdo-rm" args
    when (rmHelp myOpts) $ putStrLn
        (usageInfo "Usage: fdo-rm [OPTION...] parameters..." rmOptions)
        >> exitSuccess
    (iPath,fPath) <- maybe
        getTrashPaths
        (\p -> return (p </> "info", p </> "files"))
        (rmTrash myOpts)
    print myOpts
    when (null realArgs) $ ioError (userError "No files specified")

    now <- getCurrentTime
    let time = now
    let file = TrashFile
            (iPath </> head args)
            (fPath </> head args)
            (head realArgs)
            time
            0
    rmFile (head realArgs) file

--Purge
data PurgeOptions = PurgeOptions
    { purgeThreshold :: Double
    , purgeAgePow    :: Double
    , purgeSizePow   :: Double
    , purgeVersion   :: Bool
    , purgeHelp      :: Bool
    , purgeTrash     :: Maybe String
    } deriving(Show)

purgeDefaults = PurgeOptions
    { purgeThreshold = 10**6
    , purgeAgePow = 1
    , purgeSizePow = 0.1
    , purgeHelp = False
    , purgeVersion = False
    , purgeTrash = Nothing
    }

purgeOptions =
    [ Option ['V'] ["version"] (NoArg (\opts -> opts{purgeVersion=True})) "Show version number"
    , Option ['h'] ["help"] (NoArg (\opts -> opts{purgeHelp=True})) "Print help"
    , Option ['a'] ["age"] (ReqArg  (\secs opts -> opts{purgeThreshold=read secs}) "secs")
        ("Specify maximium file age default: " ++ (show $ purgeThreshold purgeDefaults))
    , Option ['A'] ["age-power"] (ReqArg (\pow opts -> opts{purgeAgePow=read pow}) "pow")
        ("Specify age power for threshold formula size^sizepow*age^agepow, default: " ++
        (show $ purgeAgePow purgeDefaults))
    , Option ['S'] ["size-power"] (ReqArg (\pow opts -> opts{purgeSizePow=read pow}) "pow")
        ("Specify size power for threshold formula size^sizepow*age^agepow, default: "
        ++ (show $ purgeSizePow purgeDefaults))
    , Option ['T'] ["trash-path"] (ReqArg (\path opts -> opts{purgeTrash=Just path}) "path")
        "Override Trash path autodetection."
    ]

fdoPurge args = do
    (myOpts, _) <- parseOpts purgeDefaults purgeOptions "fdo-purge" args
    when (purgeHelp myOpts) $ putStrLn
        (usageInfo "Usage: fdo-purge [OPTION...] parameters..." purgeOptions)
        >> exitSuccess
    (iPath,fPath) <- maybe
        getTrashPaths
        (\p -> return (p </> "info", p </> "files"))
        (purgeTrash myOpts)
    timeZone <- getCurrentTimeZone
    (iExtra,dExtra) <- trashGetOrphans iPath fPath
    ayx <- trashGetFiles iPath fPath
    print (iExtra,dExtra)
    print myOpts
    print args
    print ayx

--Unrm
data UnRmOptions = UnRmOptions
    { unRmOrigDir  :: Bool
    , unRmVersion  :: Bool
    , unRmHelp     :: Bool
    , unRmOutFile  :: Maybe String
    , unRmSelect   :: Maybe Int
    , unRmTrash    :: Maybe String
    } deriving(Show)

unRmDefaults = UnRmOptions
    { unRmOrigDir = False
    , unRmHelp    = False
    , unRmVersion = False
    , unRmOutFile = Nothing
    , unRmSelect  = Nothing
    , unRmTrash   = Nothing
    }

unRmOptions =
    [ Option ['V'] ["version"] (NoArg (\opts -> opts{unRmVersion=True})) "Show version number"
    , Option ['h'] ["help"] (NoArg (\opts -> opts{unRmHelp=True})) "Print help"
    , Option ['O'] ["original-name"] (NoArg  (\opts -> opts{unRmOrigDir=True}))
        "output file to original path, default: ., conflicts with -o"
    , Option ['o'] ["output-file"] (ReqArg (\out opts -> opts{unRmOutFile=Just out}) "filepath")
        "Specify output file, conflicts with -O"
    , Option ['s'] ["select"] (ReqArg (\index opts -> opts{unRmSelect=Just $ read index}) "index")
        "Select file with index if multiple files match"
    , Option ['T'] ["trash-path"] (ReqArg (\path opts -> opts{unRmTrash=Just path}) "path")
        "Override Trash path autodetection."
    ]

doRestore file opts saveFile = maybe
    (if (unRmOrigDir opts)
        then trashRestore file Nothing
        else trashRestore file (Just saveFile))
    (\out -> trashRestore file (Just out))
    (unRmOutFile opts)

doUnRm files opts saveFile = do
    case (length files') of
        0 -> putStrLn $ "No such file: " ++ saveFile
        1 -> doRestore (head files') opts saveFile
        _ -> maybe
            (putStrLn $ "Multiple matches:\n" ++ unlines (zipWith (++) (map (\x -> show x ++ ": ") [0..])  (map origPath files') ))
            (\index -> if (index < length files' && index >= 0)
                then doRestore (files' !! index) opts saveFile
                else putStrLn $ "Index " ++ show index ++ " out of bounds!")
            (unRmSelect opts)
    where files' = if (isAbsolute saveFile)
            then filter (\x -> origPath x == saveFile) files
            else filter (\x -> takeFileName (origPath x) == takeFileName saveFile) files

fdoUnRm args = do
    (myOpts, realArgs) <- parseOpts unRmDefaults unRmOptions "fdo-unrm" args
    when (unRmHelp myOpts) $ putStrLn
        (usageInfo "Usage: fdo-rm [OPTION...] parameters..." unRmOptions)
        >> exitSuccess
    (iPath,fPath) <- maybe
        getTrashPaths
        (\p -> return (p </> "info", p </> "files"))
        (unRmTrash myOpts)
    files <- trashGetFiles iPath fPath
    mapM_ (doUnRm files myOpts) realArgs

--Main
main :: IO ()
main = do
    args <- getArgs
    exe <- getProgName
    let actionsStr = intercalate "|" $ map fst actions
        thisAction = maybe
            ( if (null args)
                then Nothing
                else maybe
                    (Nothing)
                    (\x -> Just (tail args, x))
                    (lookup (args !! 0) actions) )
            (\x -> Just (args,x))
            (lookup (drop 4 exe) actions)

    maybe
        (putStrLn $ "No action specified\nUsage: " ++ exe ++ " <" ++ actionsStr ++ "> params")
        (\(a,f) -> f a)
        thisAction

