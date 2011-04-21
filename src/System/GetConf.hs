{-# LANGUAGE PackageImports, StandaloneDeriving, DeriveFunctor, TupleSections #-}

module System.GetConf where

import System.Environment
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils
import Data.Char
import Data.Either
import Control.Arrow
import Control.Applicative
import "mtl" Control.Monad.Error

import System.GetConf.SimpleIni

deriving instance Functor ArgDescr
deriving instance Functor OptDescr


--data OptDescr a = Option [Char] [String] (ArgDescr a) String

for = flip map
infixl 4 <$$>
(<$$>) = flip map
unlessM p m = p >>= \p -> unless p m


-- | From a list of key/value pairs and a list of option descriptions
-- extract the given options
optFromList :: [(String, String)] -> [OptDescr a] -> [a]
optFromList argMap optDescs = catMaybes . flip concatMap optDescs $ \(Option shorts longs desc _) ->
  for longs $ \long ->
    case lookup long argMap of
      Nothing -> Nothing
      Just val -> case desc of
        NoArg x -> if isYes val then Just x else Nothing
        ReqArg f _ -> Just $ f val
        OptArg f _ -> Just . f $ if null val then Nothing else Just val
  where
    isYes string = (map toLower . strip $ string) `elem` ["yes","on","true",""]

-- | Like optFromList, but when a given argument can not be parsed 
-- return an error condition
optFromList' :: [(String, String)] -> [OptDescr a] -> [Either String a]
optFromList' argMap optDescs = 
  let optList = optDescs >>= optToPair in
  for argMap $ \(argkey,argval) ->
    case lookup argkey optList  of
      Nothing -> Left $ "Unrecognized option: " ++ argkey
      Just arg -> case arg of
        NoArg x -> if null argval 
                   then Right x 
                   else Left $ "Option doesn't take argument: " ++ argkey
        ReqArg f desc -> if null argval 
                           then Left $ "Option requires " ++ 
                                desc ++ " as Argument: " ++ argkey 
                           else (Right $ f argval)
        OptArg f _ -> Right . f $ if null argval then Nothing else Just argval
  where
    isYes string = (map toLower . strip $ string) `elem` ["yes","on","true",""]
    optToPair (Option shorts longs args descr) = (,args) <$> longs

-- | look for conffile in the following places:
-- $XDG_CONFIG_HOME
-- $XDG_CONFIG_HOME/name
-- getAppUserDataDirectory name
-- $HOME/.name.d
-- $HOME
-- defaultConfSites           (/etc, ...)
-- defaultConfSites/name 
findConfFileByName name conffile defaultConfSites = do
  maybeConfdir <- maybeToList <$> maybeGetEnv "XDG_CONFIG_HOME"
  appdir <- getAppUserDataDirectory name
  homedir <- getHomeDirectory
  confFiles <- filterM doesFileExist . map (</> conffile) $
               maybeConfdir ++
               ((</> name) <$> maybeConfdir)  ++
               [ appdir
               , (homedir </> ("." ++ name ++ ".d"))
               , homedir
               ] ++
               defaultConfSites ++
               ((</> name) <$> defaultConfSites)
               
  return confFiles
  where
    maybeGetEnv varname = catch (Just <$> getEnv varname)
       (\e -> if isDoesNotExistError e then return Nothing else ioError e)

-- | Get options from an .ini file
optFromSimpleIni :: FilePath -> [OptDescr a] -> ErrorT String IO [a]
optFromSimpleIni filename optDesc = do
  unlessM (liftIO $ doesFileExist filename) (fail $ "File does not exist: " ++ filename)
  unlessM (liftIO $ readable <$> getPermissions filename) (fail $ "Not readable: " ++ filename)
  iniData <- ErrorT $ parseIniFile filename
  let (errors, opts) = partitionEithers $ 
                       optFromList' (map (first combine) iniData) optDesc
  unless (null errors) $ liftIO . putStrLn . unlines $ 
    ["Errors encountered while parsing options from file " 
      ++ filename
    ] 
    ++ errors
  return opts
  where 
    combine (x,y) = if null x then y else x ++ "/" ++ y

-- | Get options from environmental variables
optFromEnvironment :: String -> [OptDescr a] -> IO [a]
optFromEnvironment prefix optDesc = 
  fmap (flip (optFromList) $ addPrefix optDesc)
                                    getEnvironment
  where
    addPrefix = if null prefix
                    then id
                    else map (\(Option ss ls args desc) ->
                                Option ss (map (prefix ++) ls) args desc)
    
-- | Combine two sets of option descriptions but keep the results seperate
combineOptions :: (Functor f) => [f a] -> [f b] -> [f (Either a b)]
combineOptions lopts ropts = map (fmap Left) lopts ++ map (fmap Right) ropts

-- | Get combined options, seperating the results
withCombinedOptions :: (Functor m) => 
     ([OptDescr (Either a b)] -> m [Either a b])
     -> [OptDescr a]
     -> [OptDescr b]
     -> m ([a], [b])
withCombinedOptions action lopts ropts = partitionEithers <$> action (combineOptions lopts ropts)

-- | Thin wrapper around getOpt, gets options from program arguments
optFromArgs :: ArgOrder a -> [OptDescr a] -> IO [a]
optFromArgs argOrder optDescs = do 
  args <- getArgs
  let (opts, noopts, errors) = getOpt argOrder optDescs args
  unless (null errors) $ do
    forM errors putStrLn
    putStrLn "recognized Options:"
    putStrLn $ usageInfo "" optDescs
    exitFailure
  return opts
    

catRights [] = []
catRights ((Left _) : xs ) = catRights xs
catRights ((Right x) : xs ) = x : catRights xs

data ConfOptions a = ConfOptions 
  { argOrder     :: ArgOrder (Either String a) -- ^ Argument order for getOpt
  , defaultConf  :: Maybe String -- ^ default config filename
  , includeEnv   :: Bool -- ^ Read options from environment?
  , envPrefix    :: String -- ^ Global Prefix for environmental variables
  , argOnly      :: [OptDescr a] -- ^ Options that are only used with arguments
  , envOnly      :: [OptDescr a] -- ^ Options that are only used with env variables
  , fileOnly     :: [OptDescr a] -- ^ Options that are only used with config files
  , confSites    :: [FilePath]  -- ^ Default sites to look for filenames
  , programName  :: String
  }

getConfDefaults = ConfOptions 
  { argOrder    = RequireOrder
  , defaultConf = Nothing
  , includeEnv  = True
  , envPrefix   = ""
  , argOnly     = []
  , envOnly     = []
  , fileOnly    = []
  , confSites   = ["/etc"]
  , programName = ""
  }  

getConf options confOpts = do
  (argConfFiles, opts) <- withCombinedOptions (optFromArgs $ argOrder confOpts)
                      [Option "" ["conf"] (ReqArg id "path") "Path to configuration file"]
                      options
  defaultConfFiles <- case defaultConf confOpts of
    Nothing -> return []
    Just filename -> findConfFileByName 
                     (programName confOpts) filename (confSites confOpts)
  let confFiles = if null argConfFiles then defaultConfFiles else argConfFiles
  fileOpts' <- runErrorT $ concat <$> (forM confFiles $ \filename -> optFromSimpleIni filename options)
  fileOpts <- case fileOpts' of 
    Left error -> putStrLn error 
                  >> exitFailure
    Right fopts -> return fopts
  envOpts <- optFromEnvironment (envPrefix confOpts) options
  return $ concat [fileOpts, envOpts, opts]
