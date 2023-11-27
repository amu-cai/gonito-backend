
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Database.Persist.Sql        (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet                 (hamletFile)
import Yesod.Auth.HashDB           (HashDBUser(..), authHashDBWithForm)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)

import Text.Blaze.Internal (MarkupM)

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
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

isTrustedAuthorized :: (AuthEntity (HandlerSite m) ~ User, AuthId (HandlerSite m) ~ Key User, MonadHandler m, YesodAuthPersist (HandlerSite m)) => m AuthResult
isTrustedAuthorized = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> return AuthenticationRequired
    Just (Entity _ user)
      | isTrusted user -> return Authorized
      | otherwise    -> return $ Unauthorized "???"

isAdmin :: (AuthEntity (HandlerSite m) ~ User, AuthId (HandlerSite m) ~ Key User, MonadHandler m, YesodAuthPersist (HandlerSite m)) => m AuthResult
isAdmin = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> return AuthenticationRequired
    Just (Entity _ user)
      | userIsAdmin user -> return Authorized
      | otherwise  -> return $ Unauthorized "only permitted for the admin"

isTrusted :: User -> Bool
isTrusted user =
  case userIdent user of
    "ptlen@ceti.pl" -> True
    _ -> True

data LayoutCustomization = LayoutCustomization {
  layoutCustomizationBanner :: Maybe Text,
  layoutCustomizationRightPanel :: Maybe (WidgetFor App ())
}

instance Default LayoutCustomization where
  def = LayoutCustomization {
    layoutCustomizationBanner = Nothing,
    layoutCustomizationRightPanel = Nothing }

ourBanner :: Text -> LayoutCustomization
ourBanner banner = def {
  layoutCustomizationBanner = Just ("/static/images/" <> banner <> ".jpg")
  }

defaultCustomizableLayout :: ToWidget App a => LayoutCustomization -> a -> HandlerFor App (MarkupM ())
defaultCustomizableLayout customization widget = do
  let mBanner = layoutCustomizationBanner customization
  let mRightPanel = layoutCustomizationRightPanel customization

  master <- getYesod
  mmsg <- getMessage

  -- We break up the default layout into two components:
  -- default-layout is the contents of the body tag, and
  -- default-layout-wrapper is the entire page. Since the final
  -- value passed to hamletToRepHtml cannot be a widget, this allows
  -- you to use normal widget features in default-layout.

  maybeUser <- maybeAuth

  pc <- widgetToPageContent $ do
      $(widgetFile "default-layout")
  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


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

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = defaultCustomizableLayout def widget

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized ViewPublicKeyR _ = return Authorized
    isAuthorized (EditSubmission1R _ _ _) _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized HomeR _ = regularAuthorization
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized QueryFormR _ = regularAuthorization
    isAuthorized (QueryResultsR _) _ = regularAuthorization
    isAuthorized ListChallengesR _ = regularAuthorization
    isAuthorized ListChallengesJsonR _ = regularAuthorization
    isAuthorized (ListChallengesByTagR _) _ = regularAuthorization
    isAuthorized (ListChallengesByTagJsonR _) _ = regularAuthorization
    isAuthorized (ChallengeInfoJsonR _) _ = regularAuthorization
    isAuthorized (ChallengeRepoJsonR _) _ = regularAuthorization
    isAuthorized (VersionInfoJsonR _) _ = regularAuthorization
    isAuthorized (LeaderboardJsonR _) _ = regularAuthorization
    isAuthorized (ViewVariantR _ ) _ = regularAuthorization
    isAuthorized (ViewVariantTestR _ _) _ = regularAuthorization

    isAuthorized TagsR _ = regularAuthorization
    isAuthorized AchievementsR _ = regularAuthorization
    isAuthorized (EditAchievementR _) _ = isAdmin
    isAuthorized ExtraPointsR _ = isAdmin

    isAuthorized TestAnnouncementsR _ = isAdmin
    isAuthorized (TestChallengeAnnouncementsR _) _ = isAdmin

    isAuthorized DashboardR _ = regularAuthorization

    isAuthorized (ShowChallengeR _) _ = regularAuthorization
    isAuthorized (ChallengeHowToR _) _ = regularAuthorization
    isAuthorized (ChallengeReadmeR _) _ = regularAuthorization
    isAuthorized (ChallengeAllSubmissionsR _) _ = regularAuthorization

    isAuthorized (ChallengeMySubmissionsJsonR _) _ = return Authorized
    isAuthorized (ChallengeAllSubmissionsJsonR _) _ = return Authorized
    isAuthorized AddUserR _ = return Authorized
    isAuthorized UserInfoR _ = return Authorized
    isAuthorized FullUserInfoR _ = return Authorized
    isAuthorized MyEvaluationTriggerTokenJsonR _ = return Authorized
    isAuthorized (ChallengeSubmissionJsonR _) _ = return Authorized
    isAuthorized (MakePublicJsonR _) _ = return Authorized
    isAuthorized (ChallengeReadmeInMarkdownR _) _ = regularAuthorization
    isAuthorized (QueryJsonR _) _ = regularAuthorization
    isAuthorized ListTagsJsonR _ = regularAuthorization
    isAuthorized CurrentTimeR _ = return Authorized
    isAuthorized (FormatAsLocalTimeR _) _ = return Authorized

    isAuthorized (ChallengeGraphDataR _) _ = regularAuthorization
    isAuthorized (ChallengeDiscussionR _) _ = regularAuthorization
    isAuthorized (ChallengeDiscussionFeedR _) _ = regularAuthorization

    isAuthorized ListAnnotationsR _ = isAdmin
    isAuthorized (AnnotationTaskR _) _ = regularAuthorization
    isAuthorized (AnnotationTaskDecisionR _ _ _) _ = regularAuthorization
    isAuthorized (AnnotationTaskResultsR _) _ = isAdmin

    isAuthorized Presentation4RealR _ = regularAuthorization
    isAuthorized PresentationPSNC2019R _ = regularAuthorization
    isAuthorized GonitoInClassR _ = regularAuthorization
    isAuthorized WritingPapersWithGonitoR _ = regularAuthorization

    isAuthorized (AvatarR _) _ = regularAuthorization

    isAuthorized TriggerRemotelyR _ = return Authorized
    isAuthorized (TriggerRemotelySimpleR _ _ _ _) _ = return Authorized
    isAuthorized TriggerLocallyR _ = return Authorized
    isAuthorized (TriggerByWebhookR _ _) _ = return Authorized
    isAuthorized (OpenViewProgressR _) _ = return Authorized

    isAuthorized CreateResetLinkR _ = isAdmin
    isAuthorized (ScoreR _) _ = isAdmin

    isAuthorized ListArchivedChallengesR _ = isAdmin
    isAuthorized (ArchiveR _) _ = isAdmin
    isAuthorized (UnarchiveR _) _ = isAdmin
    isAuthorized (WipeR _) _ = isAdmin
    isAuthorized (ChallengeUpdateR _) _ = isAdmin
    isAuthorized (HealR _) _ = isAdmin

    isAuthorized MyScoreR _ = regularAuthorization

    isAuthorized (ResetPasswordR _) _ = return Authorized
    isAuthorized (ToggleSubmissionTagR _) _ = regularAuthorization

    isAuthorized (ChallengeImgR _) _ = regularAuthorization

    isAuthorized (ApiTxtScoreR _) _ = return Authorized

    isAuthorized (ChallengeParamGraphDataR _ _ _) _ = regularAuthorization
    isAuthorized (IndicatorGraphDataR _) _ = regularAuthorization

    isAuthorized (CompareFormR _ _) _ = regularAuthorization

    isAuthorized (CourseR _) _ = regularAuthorization

    isAuthorized MyTeamsR _ = isTrustedAuthorized
    isAuthorized CreateTeamR _ = isTrustedAuthorized

    isAuthorized (TestProgressR _ _) _ = isTrustedAuthorized
    isAuthorized (TestProgressJsonR _ _) _ = return Authorized

    isAuthorized SwaggerR _ = return Authorized

    isAuthorized (ViewProgressWithWebSocketsR _) _ = isTrustedAuthorized

    isAuthorized (ViewProgressWithWebSocketsJsonR _) _  = return Authorized
    isAuthorized (ViewProgressLogR _) _  = return Authorized

    isAuthorized UserSubmissionAbilityR _ = regularAuthorization
    isAuthorized (DeleteSubmissionR _) _ = return Authorized
    isAuthorized WirusR _ = return Authorized
    isAuthorized WirusAdminR _ = return Authorized

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

    makeLogger = return . appLogger

regularAuthorization = do
  app <- getYesod
  mauth <- maybeAuth
  return $ defaultStatus mauth $ appIsPublic (appSettings app)
  where defaultStatus _ True = Authorized
        defaultStatus mauth False = case mauth of
          Just _ -> Authorized
          Nothing -> AuthenticationRequired

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

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
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
                    , userVerificationKey = Nothing
                    , userKeyExpirationDate = Nothing
                    , userTriggerToken = Nothing
                    , userAltRepoScheme = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins master = [authHashDBWithForm (myLoginForm master) (Just . UniqueUser)]

contactEmailLabel :: App -> Text
contactEmailLabel site =
  case maybeContactMail of
     Just contactMail -> " (" ++ contactMail ++ ")"
     Nothing -> ""
   where maybeContactMail = appContactEmail $ appSettings site

myLoginForm :: App -> Route site -> WidgetFor site ()
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
