{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ecom.Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Ecom.Import

import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers

import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

import Data.Acid (openLocalState)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Ecom.Handler.Home
import Ecom.Handler.Product

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "Ecom" resourcesEcom

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO Ecom
makeFoundation conf = do
    manager <- newManager def
    s       <- staticSite
    logger  <- mkLogger True stdout
    state   <- openLocalState initialEcomState
    let foundation = Ecom conf s manager state logger

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
