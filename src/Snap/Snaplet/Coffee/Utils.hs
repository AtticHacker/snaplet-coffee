module Snap.Snaplet.Coffee.Utils
       ( CompileMode(..)
       , CoffeeScript(..)
       , allCoffeeFiles
       , destDir
       , srcDir
       , createDirUnlessExists
       , getCompilerMode
       , getDestDir
       , requestedCoffeeFile
       ) where

import Control.Monad
import System.Directory
import System.FilePath

data CompileMode = Development | Production deriving (Show, Eq)

-- | The CoffeeScript Snaplet Configuration
data CoffeeScript = CoffeeScript
    { snapletFilePath :: FilePath
    , compiler        :: Maybe String
    , compileMode     :: CompileMode
    , destinationDir  :: FilePath
    } deriving (Show)

allCoffeeFiles :: CoffeeScript -> IO [FilePath]
allCoffeeFiles cfg = do
    let p = srcDir cfg
    a <- getDirectoryContents p
    return $ map (</> srcDir cfg) $ filterCoffee a

takeReverse :: Int -> [a] -> [a]
takeReverse i = reverse . take i . reverse

filterCoffee :: [String] -> [String]
filterCoffee = filter ((==) ".coffee" . takeReverse 7)

srcDir :: CoffeeScript -> FilePath
srcDir = (</> "coffee") . snapletFilePath

destDir :: CoffeeScript -> FilePath
destDir = (</> "js") . snapletFilePath

createDirUnlessExists :: FilePath -> IO ()
createDirUnlessExists fp = do
    dirExists <- doesDirectoryExist fp
    unless dirExists $ createDirectory fp

getCompilerMode :: Maybe String -> CompileMode
getCompilerMode Nothing              = Production
getCompilerMode (Just "Development") = Development
getCompilerMode (Just "Production")  = Production
getCompilerMode (Just x)             = error $ x ++ " is not a valid Compiler mode for snaplet-coffeescript. -- devel.cfg"

getDestDir :: Maybe String -> CoffeeScript -> FilePath
getDestDir Nothing   c = destDir c
getDestDir (Just "") c = destDir c
getDestDir (Just x)  _ = x


requestedCoffeeFile :: String -> FilePath
requestedCoffeeFile =
     ("/"++) . (++"coffee") . reverse .dropWhile con2 . takeWhile con1 . reverse
  where con1 = (/= '/')
        con2 = (/= '.')