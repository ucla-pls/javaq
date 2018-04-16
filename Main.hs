{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens               hiding (argument)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
-- import           Data.Foldable
-- import           Data.Text.IO               as Text
import           System.Console.Docopt
import           System.Environment         (getArgs)

import           Jvmhs

patterns :: Docopt
patterns = [docopt|
javaq version 0.0.1

Usage:
  javaq [options] (-h | --help)
  javaq [options] [<classname>...]

Options:
  --cp=<classpath>      The classpath to search for classess
  --stdlib              Also analyse the stdlib
  --jre=<jre>           The location of the stdlib
|]

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath  :: ClassPath
  , _cfgJre        :: Maybe FilePath
  , _cfgUseStdlib  :: Bool
  , _cfgClassNames :: [ ClassName ]
  } deriving (Show)

makeLenses 'Config

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

-- parseCommand :: Arguments -> IO Cmd
-- parseCommand args
--   | args `isPresent` command "list" =
--     return $ ListClasses
--   | args `isPresent` command "decompile" =
--     Decompile . strCls <$>
--       getArgOrExit args (argument "classname")
--   | otherwise =
--     exitWithUsageMessage patterns "Did not know this command"

parseConfig :: Arguments -> IO Config
parseConfig args = do
--  cmd <- parseCommand args
  return $ Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    , _cfgClassNames = strCls <$> getAllArgs args (argument "classname")
    }

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  cfg <- parseConfig args
  decompile cfg

-- -- | List classes
-- listClasses :: Config -> IO ()
-- listClasses cfg = do
--   classReader <- preload =<< createClassLoader cfg
--   classes <- classes classReader
--   forM_ classes  $ \(cls, _) ->
--     Text.putStrLn $ view fullyQualifiedName cls

-- | Decompile and pring a classfile to stdout
decompile :: Config -> IO ()
decompile cfg = do
  classReader <- preload =<< createClassLoader cfg
  classnames <-
    case cfg ^. cfgClassNames of
      [] -> map fst <$> classes classReader
      a -> return a
  e <- runHierarchy classReader $ do
    classnames ^!! folded.load
  case e of
    Left msg -> error (show msg)
    Right ls ->
      BS.putStrLn $ encode ls

-- | Create a class loader from the config
createClassLoader :: Config -> IO ClassLoader
createClassLoader cfg
  | cfg ^. cfgUseStdlib =
    case cfg ^. cfgJre of
      Nothing ->
        fromClassPath (cfg ^. cfgClassPath)
      Just jre ->
        fromJreFolder (cfg ^. cfgClassPath) jre
  | otherwise =
    return $ ClassLoader [] [] (cfg ^. cfgClassPath)
