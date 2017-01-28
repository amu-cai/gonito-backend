module Foundation where

import Database.Persist.Sql        (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet                 (hamletFile)
import Yesod.Auth.HashDB           (HashDBUser(..),authHashDB,authHashDBWithForm)
import Yesod.Auth.Message          (AuthMessage (InvalidLogin))
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)
import Yesod.Fay

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings          :: AppSettings
    , appStatic            :: Static -- ^ Settings for static file serving.
    , appConnPool          :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager       :: Manager
    , appLogger            :: Logger
    , appFayCommandHandler :: CommandHandler App
    , jobs                 :: TVar (IntMap (TChan (Maybe Text)))
    , nextJob              :: TVar Int
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

mkMessage "App" "messages" "en"

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

isTrustedAuthorized = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> return AuthenticationRequired
    Just (Entity _ user)
      | isTrusted user -> return Authorized
      | otherwise    -> return $ Unauthorized "???"


isTrusted :: User -> Bool
isTrusted user =
  case userIdent user of
    "ptlen@ceti.pl" -> True
    "hexin1989@gmail.com" -> True
    "romang@amu.edu.pl" -> True
    "junczys@amu.edu.pl" -> True
    "rafalj@amu.edu.pl" -> True
    _ -> True



-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend app = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        ((appVarDir $ appSettings app) </> "config/client_session_key.aes")

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        maybeUser <- maybeAuth

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized QueryFormR _ = return Authorized
    isAuthorized (QueryResultsR _) _ = return Authorized
    isAuthorized ListChallengesR _ = return Authorized

    isAuthorized (ShowChallengeR _) _ = return Authorized
    isAuthorized (ChallengeReadmeR _) _ = return Authorized
    isAuthorized (ChallengeAllSubmissionsR _) _ = return Authorized
    isAuthorized (ChallengeGraphDataR _) _ = return Authorized
    isAuthorized (ChallengeDiscussionR _) _ = return Authorized
    isAuthorized (ChallengeDiscussionFeedR _) _ = return Authorized

    isAuthorized Presentation4RealR _ = return Authorized

    isAuthorized (AvatarR _) _ = return Authorized

    -- Default to Authorized for now.
    isAuthorized _ _ = isTrustedAuthorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            Right
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodJquery App
instance YesodFay App where

    fayRoute = FaySiteR

    yesodFayCommand render command = do
        master <- getYesod
        appFayCommandHandler master render command

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        Authenticated <$> case x of
            Just (Entity uid _) -> return $ uid
            Nothing ->
                insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    , userName = Nothing
                    , userIsAdmin = False
                    , userLocalId = Nothing
                    , userIsAnonymous = False
                    , userAvatar = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins master = [authHashDBWithForm (myLoginForm master) (Just . UniqueUser)]

    authHttpManager = getHttpManager

contactEmailLabel :: App -> Text
contactEmailLabel site =
  case maybeContactMail of
     Just contactMail -> " (" ++ contactMail ++ ")"
     Nothing -> ""
   where maybeContactMail = appContactEmail $ appSettings site

myLoginForm site action = $(whamletFile "templates/auth.hamlet")

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
