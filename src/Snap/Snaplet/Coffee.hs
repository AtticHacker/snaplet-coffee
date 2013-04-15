{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Coffee( CoffeeScript
                          , initCoffee
                          , coffeeServe
                          ) where


import qualified Data.Configurator as C
import           Snap.Snaplet
import           Snap.Core
import           Snap.Util.FileServe
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           System.Exit
import           Coffee.Bindings
import           Snap.Snaplet.Coffee.Utils

import Paths_snaplet_coffee


initCoffee :: SnapletInit c CoffeeScript
initCoffee = makeSnaplet "coffee" "description" dataDir $ do
    config <- getSnapletUserConfig
    fp     <- getSnapletFilePath
    [comp, dev, dDir] <- liftIO $ mapM (C.lookup config) configOptions

    let coffee = CoffeeScript fp comp (getCompilerMode dev) (getDestDir dDir coffee)
    liftIO $ mapM_ createDirUnlessExists [fp, srcDir coffee, destDir coffee]
    when (Production == compileMode coffee) $ liftIO $ compileFiles coffee
    return coffee
  where dataDir = Just $ liftM (++ "/resources") getDataDir
        configOptions = ["compilerPath", "compilerMode", "destinationPath"]

coffeeServe :: Handler b CoffeeScript ()
coffeeServe = do
    modifyResponse . setContentType $ "text/javascript;charset=utf-8"
    cfg <- get
    when (Development == compileMode cfg) $ liftIO $ compileFiles cfg
    serveDirectory $ destinationDir cfg

compileFiles :: MonadIO m => CoffeeScript -> m ()
compileFiles cfg = do
    let coffeeStruct = Coffee (compiler cfg) False
    fullPaths <- liftIO $ allCoffeeFiles cfg
    result <- liftIO $ coffeeCompile fullPaths
              (Just (destinationDir cfg)) coffeeStruct
    case result of
        ExitSuccess   -> return ()
        ExitFailure x -> error $ show x ++ " - Error while compiling CoffeeScript\nIs your /snaplets/coffee/devel.cfg correct?\nYou might need to restart the server after fixing the issue."