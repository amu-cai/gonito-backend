{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , appSelfContainedMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import           Database.Persist.Postgresql          (createPostgresqlPool,
                                                       pgConnStr, pgPoolSize,
                                                       runSqlPool)
import           Import
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.HTTP.Client.TLS              (getGlobalManager)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setHost, setOnException,
                                                       setPort)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)

import           Crypto.PasswordStore
import           Yesod.Auth.HashDB                    (defaultStrength)

import qualified Data.IntMap                          as IntMap

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.AccountReset
import           Handler.Achievements
import           Handler.Annotations
import           Handler.Announcements
import           Handler.Common
import           Handler.Course
import           Handler.CreateChallenge
import           Handler.Dashboard
import           Handler.Discussion
import           Handler.EditSubmission
import           Handler.Evaluate
import           Handler.ExtraPoints
import           Handler.Graph
import           Handler.Home
import           Handler.ListChallenges
import           Handler.MakePublic
import           Handler.Presentation
import           Handler.Query
import           Handler.Score
import           Handler.Shared
import           Handler.ShowChallenge
import           Handler.Swagger
import           Handler.Tags
import           Handler.Team
import           Handler.YourAccount
import Handler.SubmissionAbility
import Handler.DeleteSubmission
import Handler.EditSubmissionV1
import Handler.Wirus
import Handler.ViewPublicKey
import Handler.Wirus_admin

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    jobs <- newTVarIO IntMap.empty
    nextJob <- newTVarIO 1

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll >> initAdmin (appAdminUser appSettings) (appAdminPassword appSettings)) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

initAdmin Nothing Nothing = return ()
initAdmin (Just "") _ = return ()
initAdmin (Just adminUser) (Just adminPass) = do
    mUserEnt <- getBy $ UniqueUser adminUser
    case mUserEnt of
        Just _ -> return ()
        Nothing -> do
            passwordEncoded <- liftIO $ makePassword (encodeUtf8 adminPass) defaultStrength
            _ <- insert User
                { userIdent = adminUser
                , userPassword = Just $ decodeUtf8 passwordEncoded
                , userName = Nothing
                , userIsAdmin = True
                , userLocalId = Nothing
                , userIsAnonymous = False
                , userAvatar = Nothing
                , userVerificationKey = Nothing
                , userKeyExpirationDate = Nothing
                , userTriggerToken = Nothing
                , userAltRepoScheme = Nothing
                }
            return ()

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging $ myCors appPlain
    where myCors = cors (const $ Just (simpleCorsResourcePolicy {corsMethods = ["GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS"], corsRequestHeaders = ["Authorization"]}))
makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    makeApplication foundation

-- | The @main@ function for an executable running this site.
appSelfContainedMain :: IO ()
appSelfContainedMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
