{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Coffee( CoffeeScript
                          , initCoffee
                          , coffeeServe
                          ) where

import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as BS
import           Snap.Snaplet
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Snaplet.Coffee.Utils
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Applicative
import           System.Exit
import           Coffee.Bindings
import           Paths_snaplet_coffee

initCoffee :: SnapletInit c CoffeeScript
initCoffee = makeSnaplet "coffee" "description" dataDir $ do
    config <- getSnapletUserConfig
    fp     <- getSnapletFilePath
    [comp, dev, dDir] <- liftIO $ mapM (C.lookup config) configOptions

    let coffee = CoffeeScript fp comp (getCompilerMode dev) (getDestDir dDir coffee)
    liftIO $ mapM_ createDirUnlessExists [fp, srcDir coffee, destDir coffee]

    allCoffees <- liftIO $ allCoffeeFiles coffee

    when (Production == compileMode coffee) $ liftIO $ compileFiles coffee allCoffees
    return coffee
  where dataDir = Just $ liftM (++ "/resources") getDataDir
        configOptions = ["compilerPath", "compilerMode", "destinationPath"]

coffeeServe :: Handler b CoffeeScript ()
coffeeServe = do
    modifyResponse . setContentType $ "text/javascript;charset=utf-8"
    cfg <- get
    requestedFile <- (srcDir cfg ++) . requestedCoffeeFile .
                     BS.unpack . rqURI <$> getRequest
    when (Development == compileMode cfg) $ liftIO $ compileFiles cfg [requestedFile]
    serveDirectory $ destinationDir cfg

compileFiles :: MonadIO m => CoffeeScript -> [FilePath] -> m ()
compileFiles cfg fp = do
    let coffeeStruct = Coffee (compiler cfg) False
    result    <- liftIO $ coffeeCompile fp
                 (Just (destinationDir cfg)) coffeeStruct
    case result of
        ExitSuccess   -> return ()
        ExitFailure x -> error $ show x ++ errMsg
  where msg1 = " - Error while compiling CoffeeScript\n"
        msg2 = "Does this file really exist?\n"
        msg3 = "Is your /snaplets/coffee/devel.cfg correct?\n"
        msg4 = "You might need to restart the server after fixing the issue."
        errMsg = msg1 ++ msg2 ++ msg3 ++ msg4
