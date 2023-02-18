{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Handler.ShowChallenge where

import Import hiding (Proxy, fromList)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import qualified Data.Text.Lazy as TL
import           Text.Markdown

import qualified Data.Text as T

import qualified Data.HashMap.Strict as HMS

import qualified Yesod.Table as Table

import Control.Concurrent.Lifted (threadDelay)

import Data.Time.LocalTime

import qualified Data.List.Utils as DLU

import Handler.Extract
import Handler.Shared
import Handler.Runner
import Handler.Tables
import Handler.TagUtils
import Handler.MakePublic
import Handler.Dashboard
import Handler.Common
import Handler.Evaluate
import Handler.JWT
import Handler.Team
import Handler.Announcements
import Handler.ListChallenges (getChallengeTags)

import Database.Persist.Sql (fromSqlKey)

import qualified Data.Map as Map

import Web.Announcements

import Data.Maybe (fromJust)

import Text.Blaze

import Data.Aeson

import Gonito.ExtractMetadata (ExtractionOptions(..),
                               extractMetadataFromRepoDir,
                               GonitoMetadata(..),
                               parseTags,
                               Link(..))

import qualified Text.Read as TR

import GEval.Core
import GEval.EvaluationScheme
import GEval.Formatting
import GEval.Common (MetricResult(..))

import PersistSHA1

import System.IO (readFile)

import Data.Text (pack, unpack)

import Data.List (nub)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import Data.Swagger hiding (get, tags, delete)
import qualified Data.Swagger as DS

import Data.Swagger.Declare
import Control.Lens hiding ((.=), (^.))
import Data.Proxy as DPR
import Data.HashMap.Strict.InsOrd (fromList)

import System.Directory

instance ToJSON Import.Tag where
  toJSON t = object
       [ "name" .= tagName t
       , "description" .= tagDescription t
       , "color" .= tagColor t
       ]

instance ToSchema Import.Tag where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    return $ NamedSchema (Just "Tag") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                     , ("description", stringSchema)
                     , ("color", stringSchema)
                    ]
        & required .~ [ "name", "color", "description" ]


instance ToJSON LeaderboardEntry where
    toJSON entry = object
        [ "submitter" .= (formatSubmitter $ leaderboardUser entry)
        , "team" .= (teamIdent <$> entityVal <$> leaderboardTeam entry)
        , "when" .= (submissionStamp $ leaderboardBestSubmission entry)
        , "version" .= (fst $ leaderboardVersion entry)
        , "phase" .= (snd $ leaderboardVersion entry)
        , "description" .= descriptionToBeShown (leaderboardBestSubmission entry)
                                                (leaderboardBestVariant entry)
                                                (leaderboardParams entry)
        , "times" .= leaderboardNumberOfSubmissions entry
        , "hash" .= (fromSHA1ToText $ submissionCommit $ leaderboardBestSubmission entry)
        , "isPublic" .= (submissionIsPublic $ leaderboardBestSubmission entry)
        , "isOwner" .= (leaderboardIsOwner entry)
        , "isReevaluable" .= (leaderboardIsReevaluable entry)
        , "isVisible" .= (leaderboardIsVisible entry)
        , "id" .= (leaderboardBestSubmissionId entry)
        , "variant" .= (leaderboardBestVariantId entry)
        ]

declareLeaderboardSwagger :: Declare (Definitions Schema) Swagger
declareLeaderboardSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  leaderboardResponse      <- declareResponse (Proxy :: Proxy LeaderboardView)

  return $ mempty
    & paths .~
        fromList [ ("/api/leaderboard/{challengeName}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "challengeName"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Returns a leaderboard for a given challenge"
                                        & at 200 ?~ Inline leaderboardResponse))
                 ]


leaderboardApi :: Swagger
leaderboardApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareLeaderboardSwagger mempty

data LeaderboardView = LeaderboardView {
  leaderboardViewTests :: [Entity Test],
  leaderboardViewEntries :: [LeaderboardEntryView]
}

instance ToJSON LeaderboardView where
    toJSON v = object
        [ "tests" .= (map getTestReference $ leaderboardViewTests v)
        , "entries" .= leaderboardViewEntries v
        ]

instance ToSchema LeaderboardView where
  declareNamedSchema _ = do
    testsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [TestReference])
    entriesSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [LeaderboardEntryView])
    return $ NamedSchema (Just "Leaderboard") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("tests", testsSchema)
                     , ("entries", entriesSchema)
                    ]
        & required .~ [ "tests", "entries" ]

getLeaderboardJsonR :: Text -> Handler Value
getLeaderboardJsonR = makeSureChallengeAccessible getLeaderboardJsonR'

getLeaderboardJsonR' :: Maybe (Entity User) -> Entity Challenge -> Handler Value
getLeaderboardJsonR' _ (Entity challengeId challenge) = do
  disclosedInfo <- fetchDisclosedInfo challenge
  leaderboardStyle <- determineLeaderboardStyle challenge
  (leaderboard, (_, tests)) <- getLeaderboardEntries 1 leaderboardStyle challengeId
  return $ toJSON $ LeaderboardView {
    leaderboardViewTests = tests,
    leaderboardViewEntries = map (toLeaderboardEntryView disclosedInfo tests) leaderboard }

data LeaderboardEntryView = LeaderboardEntryView {
  leaderboardEntryViewEntry :: LeaderboardEntry,
  leaderboardEntryViewEvaluations :: [EvaluationView]
}

addJsonKey :: Text -> Value -> Value -> Value
addJsonKey key val (Object xs) = Object $ HMS.insert key val xs
addJsonKey _ _ xs = xs


-- Helper definitions for properties used in more than one place

isVisibleSchema :: Referenced Schema
isVisibleSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy Bool)
  & description .~ Just "Whether the details of the submissions are visible (i.e. either the submission is public or the user has the right permissions)"

isPublicSchema :: Referenced Schema
isPublicSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy Bool)
  & description .~ Just "Whether the submissions is public (i.e. whether its details are available to everyone)"

hashSchema :: Referenced Schema
hashSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy String)
  & description .~ Just "Git SHA1 commit hash; could be used as an argument for queries (if the submission is visible)"
  & example .~ Just "ec41f0e2636bfedbd765c9871c813f7c5b896c51"

versionSchema :: Referenced Schema
versionSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy [Int])
  & description .~ Just "Challenge version under which the submission was done"
  & example .~ Just (toJSON [2 :: Int, 0, 1])

submitterSchema :: Referenced Schema
submitterSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy String)
  & description .~ Just ("Name of the submitter, might be a special value in square brackets, e.g. " <> anonymizedLabel <> " or " <> nameNotGivenLabel)
  & example .~ Just "John Smith"

submissionIdSchema :: Referenced Schema
submissionIdSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy Int)
  & description .~ Just "Internal database identifier of the submission"
  & example .~ Just(toJSON (42 :: Int))

variantIdSchema :: Referenced Schema
variantIdSchema = Inline $ toSchema (DPR.Proxy :: DPR.Proxy Int)
  & description .~ Just "Internal database identifier of the submission variant"
  & example .~ Just (toJSON (53 :: Int))



instance ToJSON LeaderboardEntryView where
    toJSON v = addJsonKey "evaluations"
                           (toJSON $ leaderboardEntryViewEvaluations v)
                           (toJSON $ leaderboardEntryViewEntry v)

instance ToSchema LeaderboardEntryView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    boolSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Bool)
    evaluationsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [EvaluationView])
    tagSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Import.Tag)

    return $ NamedSchema (Just "LeaderboardEntry") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("submitter", submitterSchema)
                     , ("team", stringSchema)
                     , ("when", stringSchema)
                     , ("version", versionSchema)
                     , ("phase", tagSchema)
                     , ("description", stringSchema)
                     , ("times", Inline $ toSchema (DPR.Proxy :: DPR.Proxy Int)
                                           & description .~ Just "How many times a submission from the same user/of the same tag was submitted"
                                           & minProperties .~ Just 1
                                           & example .~ Just (toJSON (2:: Int)))
                     , ("hash", hashSchema)
                     , ("evaluations", evaluationsSchema)
                     , ("isOwner", boolSchema)
                     , ("isPublic", isPublicSchema)
                     , ("isReevaluable", boolSchema)
                     , ("isVisible", isVisibleSchema)
                     , ("id", submissionIdSchema)
                     , ("variantId", variantIdSchema)
                    ]
        & required .~ [ "submitter", "when", "version", "description", "times", "hash", "evaluations" ]

toLeaderboardEntryView :: DisclosedInfo -> [(Entity Test)] -> LeaderboardEntry -> LeaderboardEntryView
toLeaderboardEntryView disclosedInfo tests entry = LeaderboardEntryView {
  leaderboardEntryViewEntry = entry,
  leaderboardEntryViewEvaluations = catMaybes $
                                    applyDisclosedInfoOnLast disclosedInfo (convertEvaluationToView (leaderboardEvaluationMap entry)) tests
  }

determineLeaderboardStyle :: Challenge -> Handler LeaderboardStyle
determineLeaderboardStyle challenge = do
  app <- getYesod
  let leaderboardStyle = appLeaderboardStyle $ appSettings app
  return $ case challengeIsCompetition challenge of
             Just True -> BySubmitter
             _ -> leaderboardStyle

makeSureChallengeAccessible :: (Maybe (Entity User) -> Entity Challenge -> Handler a) -> Text -> Handler a
makeSureChallengeAccessible rt challengeName = do
  mUserEnt <- maybeAuthPossiblyByToken
  challengeEnt@(Entity _ challenge) <- runDB $ getBy404 $ UniqueName challengeName

  isAccessible <- runDB $ isChallengeAccessibleForUser challenge (entityKey <$> mUserEnt)
  if isAccessible
    then
      rt mUserEnt challengeEnt
    else
      error $ "The challenge is inaccessible for the user"

getShowChallengeR :: Text -> Handler Html
getShowChallengeR = makeSureChallengeAccessible getShowChallengeR'

getShowChallengeR' :: Maybe (Entity User) -> Entity Challenge -> Handler Html
getShowChallengeR' _ challengeEnt@(Entity challengeId challenge) = do
  app <- getYesod
  leaderboardStyle <- determineLeaderboardStyle challenge

  isHealthy <- isChallengeHealthy challenge

  Just repo <- runDB $ get $ challengePublicRepo challenge
  (leaderboard, (entries, tests)) <- getLeaderboardEntries 1 leaderboardStyle challengeId

  showAltLeaderboard <- runDB $ hasMetricsOfSecondPriority challengeId

  (altLeaderboard, altTests) <- if showAltLeaderboard
                               then
                                do
                                 (leaderboard', (_, tests')) <- getLeaderboardEntries 3 ByTag challengeId
                                 return $ (Just leaderboard', Just tests')
                               else
                                 return (Nothing, Nothing)

  mauth <- maybeAuth

  let params = getNumericalParams entries

  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  challengeLayout True challengeEnt (showChallengeWidget mauth
                                                         challengeEnt
                                                         scheme
                                                         challengeRepo
                                                         repo
                                                         leaderboard
                                                         altLeaderboard
                                                         params
                                                         tests
                                                         altTests
                                                         isHealthy)

hasMetricsOfSecondPriority :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) => Key Challenge -> ReaderT backend m Bool
hasMetricsOfSecondPriority challengeId = do
  tests' <- selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  let tests = filter (\t -> (evaluationSchemePriority $ testMetric $ entityVal t) == 2) tests'
  return $ not (null tests)

getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR = makeSureChallengeAccessible (const getChallengeReadmeR')

getChallengeReadmeR' :: Entity Challenge -> Handler Html
getChallengeReadmeR' challengeEnt@(Entity _ challenge) = do
  readme <- challengeReadme $ challengeName challenge
  challengeLayout False challengeEnt $ toWidget readme

challengeReadmeInMarkdownApi :: Swagger
challengeReadmeInMarkdownApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareChallengeReadmeInMarkdownSwagger mempty

declareChallengeReadmeInMarkdownSwagger :: Declare (Definitions Schema) Swagger
declareChallengeReadmeInMarkdownSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        fromList [ ("/api/challenge-readme/{challengeName}/markdown",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "challengeName"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/text"]
                                        & description ?~ "Returns the challenge README in Markdown"))
                 ]


getChallengeReadmeInMarkdownR :: Text -> Handler TL.Text
getChallengeReadmeInMarkdownR = makeSureChallengeAccessible (const getChallengeReadmeInMarkdownR')

getChallengeReadmeInMarkdownR' :: Entity Challenge -> Handler TL.Text
getChallengeReadmeInMarkdownR' (Entity _ challenge) = doChallengeReadmeContents $ challengeName challenge

challengeReadme :: Text -> Handler Html
challengeReadme challengeName = do
  theContents <- doChallengeReadmeContents challengeName
  return $ markdown def theContents

-- Checks whether the directories with repos are available
isChallengeHealthy :: Challenge -> Handler Bool
isChallengeHealthy challenge = do
  publicRepoDirExists <- doesRepoExistsOnTheDisk $ challengePublicRepo challenge
  privateRepoDirExists <- doesRepoExistsOnTheDisk $ challengePrivateRepo challenge
  return $ publicRepoDirExists && privateRepoDirExists

doChallengeReadmeContents :: Text -> Handler TL.Text
doChallengeReadmeContents challengeName = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName challengeName
  let repoId = challengePublicRepo challenge
  repoDir <- getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  theContents <- liftIO $ System.IO.readFile readmeFilePath
  return $ TL.pack theContents

showChallengeWidget :: Maybe (Entity User)
                      -> Entity Challenge
                      -> RepoScheme
                      -> Repo
                      -> Repo
                      -> [LeaderboardEntry]
                      -> (Maybe [LeaderboardEntry])
                      -> [Text]
                      -> [Entity Test]
                      -> (Maybe [Entity Test])
                      -> Bool
                      -> WidgetFor App ()
showChallengeWidget mUserEnt
                    (Entity challengeId challenge)
                    scheme
                    challengeRepo
                    repo
                    leaderboard
                    mAltLeaderboard
                    params
                    tests
                    mAltTests
                    isHealthy
  =
  do
    disclosedInfo <- liftHandler $ fetchDisclosedInfo challenge
    $(widgetFile "show-challenge")
  where leaderboardWithRanks = zip [1..] leaderboard
        mAltLeaderboardWithRanks = zip [1..] <$> mAltLeaderboard
        maybeRepoLink = getRepoLink repo
        delta = Number 4
        higherTheBetterArray = getIsHigherTheBetterArray $ map entityVal tests
        mUserId = entityKey <$> mUserEnt


data GitServer = Gogs | GitLab | GitHub | Gonito
  deriving (Eq, Show)

guessGitServer :: Text -> Maybe GitServer
guessGitServer bareUrl
  | "git.wmi.amu.edu.pl" `isPrefixOf` bareUrl = Just Gogs
  | "gitlab." `isPrefixOf` bareUrl = Just GitLab
  | "git." `isPrefixOf` bareUrl = Just GitLab
  | "github." `isPrefixOf` bareUrl = Just GitHub
  | "gonito.net" `isPrefixOf` bareUrl = Just Gonito
  | otherwise = Nothing

getHttpLink :: Repo -> Maybe (Text, Text)
getHttpLink repo = case guessGitServer bareUrl of
  Just Gogs -> Just (convertToHttpLink bareUrl, "/src/" <> branch)
  Just GitLab -> Just (convertToHttpLink bareUrl, "/-/tree/" <> branch)
  Just GitHub -> Just (convertToHttpLink bareUrl, "/tree/" <> branch)
  Just Gonito -> Just (fixGonito $ convertToHttpLink bareUrl, "/" <> branch)
  Nothing -> Nothing
  where bareUrl = removeProtocol $ removeLogin rurl
        removeLogin t = r
          where (_, r) = T.breakOnEnd "@" t
        rurl = repoUrl repo
        branch = repoBranch repo
        convertToHttpLink = ("https://" <>) . (T.replace ":" "/") . (T.replace ".git" "")
        removeProtocol = stripPrefixes ["https://", "http://", "git://", "ssh://",
                                        "ssh." -- when a domain with ssh. prefix is used
                                       ]
        stripPrefixes prefixes t = foldr stripPrefixFrom t prefixes
        stripPrefixFrom pref t = if pref `isPrefixOf` t
                                   then drop (length pref) t
                                   else t
        fixGonito t = (T.replace "https://gonito.net" "https://gonito.net/gitlist" t) <> ".git"

getRepoLink :: Repo -> Maybe Text
getRepoLink repo = case getHttpLink repo of
  Just (hostname, linkRest) -> Just $ hostname <> linkRest
  Nothing -> if sitePrefix `isPrefixOf` theUrl
            then Just $ (browsableGitRepo bareRepoName) ++ "/" ++ (repoBranch repo)
            else Nothing
  where sitePrefix = "git://gonito.net/" :: Text
        sitePrefixLen = length sitePrefix
        theUrl = repoUrl repo
        bareRepoName = drop sitePrefixLen theUrl

instance ToJSON (Repo) where
    toJSON repo = object
        [ "url"  .= repoUrl repo
        , "branch" .= repoBranch repo
        , "browsableUrl" .= getRepoLink repo
        ]

instance ToSchema (Repo) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $ NamedSchema (Just "DataRepository") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [ ("url", Inline $ toSchema (DPR.Proxy :: DPR.Proxy String)
                              & description .~ Just "Git URL to be cloned (https://, git:// or ssh:// protocol)"
                              & example .~ Just (toJSON ("git://gonito.net/fiszki-ocr" :: String)))
                    , ("branch", stringSchema)
                    , ("browsableUrl", Inline $ toSchema (DPR.Proxy :: DPR.Proxy String)
                                           & description .~ Just "An URL address that your browser can open; usually, but not always available"
                                           & example .~ Just (toJSON ("https://github.com/applicaai/kleister-charity/tree/master" :: String)))

                    ]
        & required .~ [ "url", "branch" ]

getChallengeRepoJsonR :: Text -> Handler Value
getChallengeRepoJsonR chName = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName chName
  repo <- runDB $ get404 $ challengePublicRepo challenge
  return $ toJSON repo

declareChallengeRepoSwagger :: Declare (Definitions Schema) Swagger
declareChallengeRepoSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        fromList [ ("/api/challenge-repo/{challengeName}",
            mempty & DS.get ?~ (mempty
                                 & parameters .~ [ Inline $ mempty
                                                   & name .~ "challengeName"
                                                   & required ?~ True
                                                   & schema .~ ParamOther (mempty
                                                                            & in_ .~ ParamPath
                                                                            & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Return metadata for the challenge repository"))
        ]

challengeRepoApi :: Swagger
challengeRepoApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareChallengeRepoSwagger mempty


getChallengeHowToR :: Text -> Handler Html
getChallengeHowToR = makeSureChallengeAccessible getChallengeHowToR'

getChallengeHowToR' :: Maybe (Entity User) -> Entity Challenge -> Handler Html
getChallengeHowToR' maybeUser challengeEnt@(Entity _ challenge) = do
  app <- getYesod
  let settings = appSettings app

  let publicRepoId = challengePublicRepo challenge
  repo <- runDB $ get404 publicRepoId

  case maybeUser of
    Just (Entity userId user) -> do
      enableTriggerToken userId (userTriggerToken user)
    Nothing -> return ()

  let mToken = case maybeUser of
        Just (Entity _ user) -> userTriggerToken user
        Nothing -> Nothing

  let isIDSet = case maybeUser of
                  Just (Entity _ user) -> isJust $ userLocalId user
                  Nothing -> False
  isSSHUploaded <- case maybeUser of
    Just (Entity userId _) -> do
      ukeys <- runDB $ selectList [PublicKeyUser ==. userId] []
      return $ not (null ukeys)
    Nothing -> return False
  challengeLayout False challengeEnt (challengeHowTo
                                      challenge
                                      settings
                                      repo
                                      (idToBeShown challenge maybeUser)
                                      isIDSet
                                      isSSHUploaded
                                      (join $ userAltRepoScheme <$> entityVal <$> maybeUser)
                                      mToken)

idToBeShown :: p -> Maybe (Entity User) -> Text
idToBeShown _ maybeUser =
  case maybeUser of
   Just user ->  case userLocalId $ entityVal user of
                 Just localId -> localId
                 Nothing -> defaultIdToBe
   Nothing -> defaultIdToBe
  where defaultIdToBe = "YOURID" :: Text

externalRepoPlaceholder :: Text
externalRepoPlaceholder = "URL_TO_YOUR_REPO"

defaultRepo :: RepoScheme -> Text -> Challenge -> Repo -> Maybe (Entity User) -> Text
defaultRepo SelfHosted repoHost challenge _ maybeUser = repoHost ++ (idToBeShown challenge maybeUser) ++ "/" ++ (challengeName challenge)
defaultRepo Branches _ _ repo _ = repoUrl repo
defaultRepo NoInternalGitServer _ _ _ _ = externalRepoPlaceholder

defaultBranch :: IsString a => RepoScheme -> Maybe a
defaultBranch SelfHosted = Just "master"
defaultBranch Branches = Nothing
defaultBranch NoInternalGitServer = Nothing

challengeHowTo :: Challenge -> AppSettings -> Repo -> Text -> Bool -> Bool -> Maybe Text -> Maybe Text -> WidgetFor App ()
challengeHowTo challenge settings repo shownId isIDSet isSSHUploaded mAltRepoScheme mToken = $(widgetFile "challenge-how-to")
  where myBranch = "my-brilliant-branch" :: Text
        urlToYourRepo = case mAltRepoScheme of
          Just altRepoScheme -> encodeSlash (altRepoScheme <> (challengeName challenge))
          Nothing -> externalRepoPlaceholder

postHealR :: ChallengeId -> Handler TypedContent
postHealR challengeId = runViewProgress $ doHeal challengeId

doHeal :: Key Challenge -> Channel -> HandlerFor App ()
doHeal challengeId chan = do
  challenge <- runDB $ get404 challengeId
  _ <- getRepoDirOrClone (challengePrivateRepo challenge) chan
  _ <- getRepoDirOrClone (challengePublicRepo challenge) chan
  return ()

postArchiveR :: ChallengeId -> Handler Html
postArchiveR challengeId = doSetArchive True challengeId

postUnarchiveR :: ChallengeId -> Handler Html
postUnarchiveR challengeId = doSetArchive False challengeId

postWipeR :: ChallengeId -> Handler TypedContent
postWipeR challengeId = do
  runViewProgress $ doWipe challengeId

doWipe :: ChallengeId -> Channel -> Handler ()
doWipe challengeId chan = do
  challenge <- runDB $ get404 challengeId

  if challengeArchived challenge == Just True
    then
      do
        msg chan "Starting wiping…"
        msg chan "Removing comments…"
        runDB $ deleteWhere [CommentChallenge ==. challengeId]

        msg chan "Removing achievements…"
        achievements <- runDB $ selectList [AchievementChallenge ==. challengeId] []
        mapM_ (wipeAchievement chan) achievements

        msg chan "Removing test infos…"
        tests <- runDB $ selectList [TestChallenge ==. challengeId] []
        mapM_ (wipeTest chan) tests

        msg chan "Removing submissions…"
        submissions <- runDB $ selectList [SubmissionChallenge ==. challengeId] []
        mapM_ (wipeSubmission chan) submissions

        msg chan "Removing versions…"
        runDB $ deleteWhere [VersionChallenge ==. Just challengeId]

        msg chan "Removing tests…"
        runDB $ deleteWhere [TestChallenge ==. challengeId]

        msg chan "Removing tag…"
        runDB $ deleteWhere [ChallengeTagChallenge ==. challengeId]

        msg chan "Removing course links…"
        runDB $ deleteWhere [CourseChallengeChallenge ==. challengeId]

        wipeRepo chan (challengePublicRepo challenge)
        wipeRepo chan (challengePrivateRepo challenge)
        runDB $ delete challengeId

        return ()
    else
      do
        msg chan ("Only archived challenges can be wiped, archive first" :: Text)
        return ()

wipeAchievement :: Channel -> Entity Achievement -> Handler ()
wipeAchievement chan (Entity achievementId _) = do
  msg chan "Removing achievement"
  runDB $ deleteWhere [AchievementTagAchievement ==. achievementId]
  runDB $ deleteWhere [WorkingOnAchievement ==. achievementId]

  runDB $ delete achievementId
  return ()

wipeTest :: Channel -> Entity Test -> Handler ()
wipeTest chan (Entity testId _) = do
  msg chan ("Wiping test" <> (Data.Text.pack $ show $ testId))
  runDB $ deleteWhere [EvaluationTest ==. testId]
  runDB $ deleteWhere [OutTest ==. testId]
  runDB $ deleteWhere [IndicatorTest ==. testId]
  return ()

wipeSubmission :: Channel -> Entity Submission -> Handler ()
wipeSubmission chan (Entity submissionId submission) = do
  msg chan ("Wiping submission" <> (Data.Text.pack $ show $ submissionDescription submission))

  variants <- runDB $ selectList [VariantSubmission ==. submissionId] []
  mapM_ (wipeVariant chan) variants

  runDB $ deleteWhere [ExternalLinkSubmission ==. submissionId]
  runDB $ deleteWhere [ForkSource ==. submissionId]
  runDB $ deleteWhere [ForkTarget ==. submissionId]
  runDB $ deleteWhere [SubmissionTagSubmission ==. submissionId]
  runDB $ deleteWhere [WorkingOnFinalSubmission ==. Just submissionId]

  runDB $ deleteWhere [DependencySubRepoCommit ==. submissionCommit submission]
  runDB $ deleteWhere [DependencySuperRepoCommit ==. submissionCommit submission]

  wipeRepo chan (submissionRepo submission)
  runDB $ delete submissionId
  return ()

wipeVariant :: Channel -> Entity Variant -> Handler ()
wipeVariant chan (Entity variantId _) = do
  msg chan "Wiping variant"
  runDB $ deleteWhere [ParameterVariant ==. variantId]
  runDB $ delete variantId

wipeRepo :: Channel -> RepoId -> Handler ()
wipeRepo chan repoId = do
  repoDir <- getRepoDir repoId
  msg chan ("Wiping repo " <> (Data.Text.pack repoDir) <> "…")
  repoDirExists <- liftIO $ doesDirectoryExist repoDir
  if repoDirExists
  then
    do
     msg chan "Repo dir there, erasing"
     liftIO $ removePathForcibly repoDir
  else
     msg chan "Not available"
  return ()

doSetArchive :: Bool -> ChallengeId -> Handler Html
doSetArchive status challengeId = do
  runDB $ update challengeId [ChallengeArchived =. Just status]
  challenge <- runDB $ get404 challengeId
  getShowChallengeR $ challengeName challenge


archiveForm :: ChallengeId -> Form ChallengeId
archiveForm challengeId = renderBootstrap3 BootstrapBasicForm $ areq hiddenField "" (Just challengeId)

getChallengeSubmissionR :: Text -> Handler Html
getChallengeSubmissionR = makeSureChallengeAccessible getChallengeSubmissionR'

getChallengeSubmissionR' :: Maybe (Entity User) -> Entity Challenge -> Handler Html
getChallengeSubmissionR' maybeUser challengeEnt@(Entity _ challenge) = do
   let chName = challengeName challenge
   Just repo <- runDB $ get $ challengePublicRepo challenge
   app <- getYesod
   let scheme = appRepoScheme $ appSettings app
   let repoHost = appRepoHost $ appSettings app

   let defaultUrl = fromMaybe (defaultRepo scheme repoHost challenge repo maybeUser)
                              ((<> chName) <$> (join $ userAltRepoScheme <$> entityVal <$> maybeUser))

   Entity userId _ <- requireAuth

   defaultTeam <- fetchDefaultTeam userId

   (formWidget, formEnctype) <- generateFormPost $ submissionForm userId (Just defaultUrl) (defaultBranch scheme) (repoGitAnnexRemote repo) (Just defaultTeam)
   challengeLayout True challengeEnt $ challengeSubmissionWidget formWidget formEnctype challenge


declareChallengeSubmissionSwagger :: Declare (Definitions Schema) Swagger
declareChallengeSubmissionSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)
  let stringSchema = toParamSchema (Proxy :: Proxy String)

  challengeSubmissionResponse      <- declareResponse (Proxy :: Proxy Int)
  wrongSubmissionResponse      <- declareResponse (Proxy :: Proxy GonitoStatus)

  return $ mempty
    & paths .~
        fromList [ ("/api/challenge-submission/{challengeName}",
                    mempty & DS.post ?~ (mempty
                                         & parameters .~ [ Inline $ mempty
                                                           & name .~ "challengeName"
                                                           & required ?~ True
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamPath
                                                                                   & paramSchema .~ challengeNameSchema),
                                                           Inline $ mempty
                                                           & name .~ "f1"
                                                           & description .~ Just "submission description"
                                                           & required ?~ False
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamFormData
                                                                                   & paramSchema .~ stringSchema),
                                                           Inline $ mempty
                                                           & name .~ "f2"
                                                           & description .~ Just "submission tags"
                                                           & required ?~ False
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamFormData
                                                                                   & paramSchema .~ stringSchema),
                                                           Inline $ mempty
                                                           & name .~ "f3"
                                                           & description .~ Just "repo URL"
                                                           & required ?~ True
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamFormData
                                                                                   & paramSchema .~ stringSchema),
                                                                                                                      Inline $ mempty
                                                           & name .~ "f4"
                                                           & description .~ Just "repo branch"
                                                           & required ?~ True
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamFormData
                                                                                   & paramSchema .~ stringSchema),

                                                           Inline $ mempty
                                                           & name .~ "f5"
                                                           & description .~ Just "git-annex remote specification"
                                                           & required ?~ False
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamFormData
                                                                                   & paramSchema .~ stringSchema)]
                                         & produces ?~ MimeList ["application/json"]
                                         & description ?~ "Initiates a submission based on a given repo URL/branch. Returns an asynchrous job ID."
                                         & at 200 ?~ Inline challengeSubmissionResponse
                                         & at 422 ?~ Inline wrongSubmissionResponse))
                 ]

challengeSubmissionApi :: Swagger
challengeSubmissionApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareChallengeSubmissionSwagger mempty


declareMakePublicSwagger :: Declare (Definitions Schema) Swagger
declareMakePublicSwagger = do
  -- param schemas
  let idSchema = toParamSchema (Proxy :: Proxy Int)

  asyncJobResponse      <- declareResponse (Proxy :: Proxy Int)
  wrongSubmissionResponse      <- declareResponse (Proxy :: Proxy GonitoStatus)

  return $ mempty
    & paths .~
        fromList [ ("/api/make-public/{submissionId}",
                    mempty & DS.get ?~  (mempty
                                         & parameters .~ [ Inline $ mempty
                                                           & name .~ "submissionId"
                                                           & required ?~ True
                                                           & schema .~ ParamOther (mempty
                                                                                   & in_ .~ ParamPath
                                                                                   & paramSchema .~ idSchema)]
                                         & produces ?~ MimeList ["application/json"]
                                         & description ?~ "Initiates opening a submission. Returns an asynchrous job ID."
                                         & at 200 ?~ Inline asyncJobResponse
                                         & at 422 ?~ Inline wrongSubmissionResponse))
                 ]

makePublicApi :: Swagger
makePublicApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareMakePublicSwagger mempty


data ChallangeSubmissionStatus = SubmissionOK | SubmissionWrong Text
  deriving (Eq, Show)

data GonitoStatus = GonitoStatus {
  detail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON GonitoStatus
instance ToSchema GonitoStatus

postChallengeSubmissionJsonR :: Text -> Handler Value
postChallengeSubmissionJsonR = makeSureChallengeAccessible postChallengeSubmissionJsonR'

postChallengeSubmissionJsonR' :: Maybe (Entity User) -> Entity Challenge -> Handler Value
postChallengeSubmissionJsonR' mUserEnt challengeEnt@(Entity challengeId _) = do
    let Just (Entity userId _) = mUserEnt
    ((result, _), _) <- runFormPostNoToken $ submissionForm userId Nothing Nothing Nothing Nothing
    let submissionData' = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just submissionData = submissionData'

    status <- checkSubmission challengeEnt submissionData

    case status of
      SubmissionOK -> runViewProgressAsynchronously $ doCreateSubmission userId challengeId submissionData
      SubmissionWrong errorMsg -> sendResponseStatus status422 $ toJSON (GonitoStatus errorMsg)

checkSubmission :: Entity Challenge -> ChallengeSubmissionData -> Handler ChallangeSubmissionStatus
checkSubmission (Entity _ challenge) submissionData = do
  let repo = challengeSubmissionDataRepo submissionData
  if (null $ repoSpecUrl repo)
  then
    return $ SubmissionWrong "empty URL"
  else
    do
     if (null $ repoSpecBranch repo)
     then
       return $ SubmissionWrong "empty branch"
     else
       do
        if (willClone challenge submissionData)
        then
         do
          return SubmissionOK
        else
         do
          return $ SubmissionWrong "Refusing to clone the submission from this URL."

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR = makeSureChallengeAccessible postChallengeSubmissionR'


postChallengeSubmissionR' :: Maybe (Entity User) -> Entity Challenge -> Handler TypedContent
postChallengeSubmissionR' mUserEnt (Entity challengeId _) = do
    let Just (Entity userId _) = mUserEnt
    ((result, _), _) <- runFormPost $ submissionForm userId Nothing Nothing Nothing Nothing
    let submissionData' = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just submissionData = submissionData'

    runViewProgress $ doCreateSubmission userId challengeId submissionData

postTriggerLocallyR :: Handler TypedContent
postTriggerLocallyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just localId) <- lookupPostParam "user"
  mBranch <- lookupPostParam "branch"
  mGitAnnexRemote <- lookupPostParam "git-annex-remote"
  [Entity userId _] <- runDB $ selectList [UserLocalId ==. Just localId] []

  app <- getYesod
  let repoHost = appRepoHost $ appSettings app

  let localRepo = repoHost ++ localId ++ "/" ++ challengeName
  trigger userId challengeName localRepo mBranch mGitAnnexRemote

postTriggerRemotelyR :: Handler TypedContent
postTriggerRemotelyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just theUrl) <- lookupPostParam "url"
  (Just token) <- lookupPostParam "token"
  mBranch <- lookupPostParam "branch"
  mGitAnnexRemote <- lookupPostParam "git-annex-remote"
  doTrigger token challengeName theUrl mBranch mGitAnnexRemote

postTriggerRemotelySimpleR :: Text -> Text -> Text -> Text -> Handler TypedContent
postTriggerRemotelySimpleR token challengeName theUrl branch =
  doTrigger token challengeName (decodeSlash theUrl) (Just branch) Nothing

getTriggerRemotelySimpleR :: Text -> Text -> Text -> Text -> Handler TypedContent
getTriggerRemotelySimpleR token challengeName theUrl branch =
  doTrigger token challengeName (decodeSlash theUrl) (Just branch) Nothing

data GitServerPayload = GitServerPayload {
  gitServerPayloadRef :: Text,
  -- Unfortunately, the URL is given in "ssh_url" field
  -- for Gogs and "git_ssh_url" for GitLab, hence two
  -- fields here
  gitServerPayloadSshUrl :: Maybe Text,
  gitServerPayloadGitSshUrl :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON GitServerPayload where
    parseJSON (Object o) = GitServerPayload
      <$> o .: "ref"
      <*> ((o .: "repository") >>= (.:? "ssh_url"))
      <*> ((o .: "repository") >>= (.:? "git_ssh_url"))

postTriggerByWebhookR :: Text -> Text -> Handler TypedContent
postTriggerByWebhookR token challengeName = do
  payload <- requireJsonBody :: Handler GitServerPayload
  let ref = gitServerPayloadRef payload
  let refPrefix = "refs/heads/"
  if refPrefix `isPrefixOf` ref
    then
      do
        let branch = T.replace refPrefix "" ref
        let theUrl = fromMaybe (fromJust $ gitServerPayloadGitSshUrl payload)
                            (gitServerPayloadSshUrl payload)
        doTrigger token challengeName theUrl (Just branch) Nothing
    else
      error $ "unexpected ref `" ++ (T.unpack ref) ++ "`"


doTrigger :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Handler TypedContent
doTrigger token challengeName theUrl mBranch mGitAnnexRemote = do
  [Entity userId _] <- runDB $ selectList [UserTriggerToken ==. Just token] []
  trigger userId challengeName theUrl mBranch mGitAnnexRemote

trigger :: UserId -> Text -> Text -> Maybe Text -> Maybe Text -> Handler TypedContent
trigger userId challengeName theUrl mBranch mGitAnnexRemote = do
  let branch = fromMaybe "master" mBranch
  mChallengeEnt <- runDB $ getBy $ UniqueName challengeName

  let defSubmission = ChallengeSubmissionData {
        challengeSubmissionDataDescription = Nothing,
        challengeSubmissionDataTags = Nothing,
        challengeSubmissionDataRepo = RepoSpec {
            repoSpecUrl=theUrl,
            repoSpecBranch=branch,
            repoSpecGitAnnexRemote=mGitAnnexRemote},
        challengeSubmissionDataTeam = Nothing
        }

  case mChallengeEnt of
    Just (Entity challengeId _) -> runOpenViewProgress $ doCreateSubmission userId challengeId defSubmission
    Nothing -> return $ toTypedContent (("Unknown challenge `" ++ (Data.Text.unpack challengeName) ++ "`. Cannot be triggered, must be submitted manually at Gonito.net!\n") :: String)

isBefore :: UTCTime -> Maybe UTCTime -> Bool
isBefore _ Nothing = True
isBefore moment (Just deadline) = moment <= deadline

-- | An attempt to filtre out mistaken/unwanted submissions (without cloning
-- the submission repo, just by looking at the metadata)
willClone :: Challenge -> ChallengeSubmissionData -> Bool
willClone challenge submissionData =
  (challengeName challenge) `isInfixOf` theUrl && branch /= dontPeek && not (dontPeek `isInfixOf` theUrl)
  where theUrl = repoSpecUrl $ challengeSubmissionDataRepo submissionData
        branch = repoSpecBranch $ challengeSubmissionDataRepo submissionData
        dontPeek = "dont-peek"


-- | Main place where submission is done (whether manually or by trigger)
doCreateSubmission :: UserId -> Key Challenge -> ChallengeSubmissionData -> Channel -> Handler ()
doCreateSubmission userId challengeId challengeSubmissionData chan = do
  challenge <- runDB $ get404 challengeId

  theVersion <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  theNow <- liftIO getCurrentTime

  if theNow `isBefore` (versionDeadline $ entityVal theVersion)
    then
     do
      let wanted = willClone challenge challengeSubmissionData
      if wanted
        then
          doCreateSubmission' (challengeArchived challenge) userId challengeId challengeSubmissionData chan
        else
          msg chan "Refusing to clone the submission from this URL"
    else
      msg chan "Submission is past the deadline, no submission will be accepted from now on."

doCreateSubmission' :: Maybe Bool -> UserId -> Key Challenge -> ChallengeSubmissionData -> Channel -> Handler ()
doCreateSubmission' (Just True) _ _ _ chan = msg chan "This challenge is archived, you cannot submit to it. Ask the site admin to unarchive it."
doCreateSubmission' _ userId challengeId challengeSubmissionData chan = do
  let mDescription = challengeSubmissionDataDescription challengeSubmissionData
  let mTags = challengeSubmissionDataTags challengeSubmissionData
  let repoSpec = challengeSubmissionDataRepo challengeSubmissionData

  maybeRepoKey <- getSubmissionRepo userId challengeId repoSpec chan
  case maybeRepoKey of
    Just repoId -> do

      challenge <- runDB $ get404 challengeId
      user <- runDB $ get404 userId

      relevantIndicators <- getOngoingTargets challengeId


      (Entity _ currentVersion) <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
      let submittedMajorVersion = versionMajor currentVersion

      mMainEnt <- runDB $ fetchMainTest challengeId
      bestScoreSoFar <- case mMainEnt of
        Just (Entity _ mainTest) -> do
          let orderDirection = case getMetricOrdering (evaluationSchemeMetric $ testMetric mainTest) of
                TheHigherTheBetter -> E.desc
                TheLowerTheBetter -> E.asc

          bestResultSoFar <- runDB $ E.select $ E.from $ \(evaluation, submission, variant, out, test, theVersion) -> do
              E.where_ (submission ^. SubmissionChallenge E.==. E.val challengeId
                        E.&&. submission ^. SubmissionIsHidden E.==. E.val False
                        E.&&. variant ^. VariantSubmission E.==. submission ^. SubmissionId
                        E.&&. evaluation ^. EvaluationChecksum E.==. out ^. OutChecksum
                        E.&&. (E.not_ (E.isNothing (evaluation ^. EvaluationScore)))
                        E.&&. out ^. OutVariant E.==. variant ^. VariantId
                        E.&&. evaluation ^. EvaluationTest E.==. test ^. TestId
                        E.&&. test ^. TestChallenge E.==. E.val challengeId
                        E.&&. test ^. TestName E.==. E.val (testName mainTest)
                        E.&&. test ^. TestMetric E.==. E.val (testMetric mainTest)
                        E.&&. test ^. TestActive
                        E.&&. (evaluation ^. EvaluationVersion E.==. theVersion ^. VersionCommit)
                        E.&&. theVersion ^. VersionCommit E.==. test ^. TestCommit
                        E.&&. theVersion ^. VersionMajor E.>=. E.val submittedMajorVersion)
              E.orderBy [orderDirection (evaluation ^. EvaluationScore)]
              E.limit 1
              return evaluation
          let bestScoreSoFar' = join (evaluationScore <$> entityVal <$> (listToMaybe bestResultSoFar))
          return bestScoreSoFar'
        Nothing -> return Nothing

      disclosedInfo <- fetchDisclosedInfo challenge

      case bestScoreSoFar of
        Just s -> case disclosedInfo of
                   DisclosedInfo Nothing -> msg chan ("best score so far is: " ++ (Data.Text.pack $ show s))
                   _ -> return ()
        Nothing -> msg chan "first submission so far"

      repo <- runDB $ get404 repoId

      repoDir <- getRepoDirOrClone repoId chan

      gonitoMetadata <- liftIO
                       $ extractMetadataFromRepoDir repoDir (ExtractionOptions {
                                                                extractionOptionsDescription = mDescription,
                                                                extractionOptionsTags = Just $ parseTags mTags,
                                                                extractionOptionsGeneralParams = Nothing,
                                                                extractionOptionsUnwantedParams = Nothing,
                                                                extractionOptionsParamFiles = Nothing,
                                                                extractionOptionsMLRunPath = Nothing,
                                                                extractionOptionsExternalLinks = Nothing,
                                                                extractionOptionsDependencies = Nothing })

      mTeamId <- case challengeSubmissionDataTeam challengeSubmissionData of
                  Just tid -> return $ Just tid
                  Nothing -> fetchDefaultTeam userId

      submissionId <- getSubmission userId
                                   mTeamId
                                   repoId
                                   (repoCurrentCommit repo)
                                   challengeId
                                   (gonitoMetadataDescription gonitoMetadata)
                                   chan

      _ <- runDB $ mapM insert $ map (\l -> ExternalLink {
                                        externalLinkSubmission = submissionId,
                                        externalLinkTitle = linkTitle l,
                                        externalLinkUrl = linkUrl l }) $ gonitoMetadataExternalLinks gonitoMetadata

      _ <- runDB $ mapM insertUnique $ map (\s -> Dependency {
                                                  dependencySubRepoCommit = s,
                                                  dependencySuperRepoCommit = (repoCurrentCommit repo) }) $ gonitoMetadataDependencies gonitoMetadata

      outs <- getOuts False chan submissionId (gonitoMetadataGeneralParams gonitoMetadata)

      currentTagIds <- runDB $ selectList [SubmissionTagSubmission ==. submissionId] []

      runDB $ addTags submissionId (gonitoMetadataTags gonitoMetadata)  (
        map (submissionTagTag . entityVal) currentTagIds)
      msg chan "SUBMISSION CREATED"

      app <- getYesod
      let submissionLink = linkInAnnouncement app "submission" ("q/" ++ (fromSHA1ToText (repoCurrentCommit repo)))

      case mMainEnt of
        Just (Entity mainTestId mainTest) -> do
          newScores <- mapM (getScoreForOut mainTestId) outs
          let newScores' = catMaybes newScores
          let newScores'' = case getMetricOrdering (evaluationSchemeMetric $ testMetric mainTest) of
                TheHigherTheBetter -> reverse $ sort newScores'
                TheLowerTheBetter -> sort newScores'
          let compOp = case getMetricOrdering (evaluationSchemeMetric $ testMetric mainTest) of
                TheLowerTheBetter -> (<)
                TheHigherTheBetter -> (>)

          case bestScoreSoFar of
            Just b ->  case newScores'' of
                    (s:_) -> if compOp s b
                             then
                              do
                                let challengeLink = linkInAnnouncement app (challengeTitle challenge) ("challenge/"
                                                                                                             ++ (challengeName challenge))
                                let formattingOpts = getTestFormattingOpts mainTest

                                let message = [AnnouncementText "Whoa! New best result for ",
                                               challengeLink,
                                               AnnouncementText (" challenge by "
                                               ++ (fromMaybe "???" $ userName user)
                                               ++ ", "
                                               ++ (T.pack $ evaluationSchemeName $ testMetric mainTest)
                                               ++ ": "
                                               ++ (T.pack $ formatTheResult formattingOpts (SimpleRun s))
                                               ++ " ("
                                               ++ (if s > b
                                                   then "+"
                                                   else "")
                                               ++ (T.pack $ formatTheResult formattingOpts (SimpleRun (s-b)))
                                               ++ ")."
                                               ++ " See "),
                                               submissionLink,
                                               AnnouncementText ("." ++ " :clap:")]
                                case disclosedInfo of
                                  DisclosedInfo Nothing ->
                                    do
                                      msg chan $ renderAnnouncementMessage Nothing message
                                      sendChallengeAnnouncement challengeId message
                                  _ -> return ()
                             else return ()
                    [] -> return ()
            Nothing -> return ()
        Nothing -> return ()

      if appAutoOpening $ appSettings app
        then
          doMakePublic userId submissionId chan
        else
          return ()

      if not (null relevantIndicators)
        then
          checkIndicators user challengeId submissionId submissionLink relevantIndicators chan
        else
          return ()

    Nothing -> return ()

checkIndicators :: User -> ChallengeId -> SubmissionId -> AnnouncementPiece -> [IndicatorEntry] -> Channel -> Handler ()
checkIndicators user challengeId submissionId submissionLink relevantIndicators chan = do
  msg chan "Checking indicators..."
  theNow <- liftIO $ getCurrentTime
  mapM_ (\indicator -> checkIndicator theNow user challengeId submissionId submissionLink indicator chan) relevantIndicators

checkIndicator :: UTCTime -> User -> ChallengeId -> SubmissionId -> AnnouncementPiece -> IndicatorEntry -> Channel -> Handler ()
checkIndicator theNow user challengeId submissionId submissionLink indicator chan = do
  (entries, _) <- runDB $ getChallengeSubmissionInfos 1 (\(Entity sid _) -> sid == submissionId) (const True) id challengeId
  mapM_ (\t -> checkTarget theNow user submissionLink entries indicator t chan) (indicatorEntryTargets indicator)

checkTarget :: UTCTime -> User -> AnnouncementPiece -> [TableEntry] -> IndicatorEntry -> Entity Target -> Channel -> Handler ()
checkTarget theNow user submissionLink entries indicator target chan = do
  let challengeId = entityKey $ indicatorEntryChallenge indicator

  let status = getTargetStatus theNow entries indicator target
  if status == TargetPassed
    then
     do
      let message = [AnnouncementText ("Congratulations!!! The target " ++ indicatorText
                     ++ " was beaten by "
                     ++ (fromMaybe "???" $ userName user)
                     ++ ", "
                     ++ " See "),
                     submissionLink,
                     AnnouncementText ("."
                                       ++ (T.replicate 10 " :champagne: ") ++ " :mleczko: ")]
      msg chan $ renderAnnouncementMessage Nothing message
      sendChallengeAnnouncement challengeId message
    else
       return ()
  where indicatorText = prettyIndicatorEntry indicator

getScoreForOut :: (PersistQueryRead (YesodPersistBackend site), YesodPersist site, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => Key Test -> Out -> HandlerFor site (Maybe Double)
getScoreForOut mainTestId out = do
  mEvaluation <- runDB $ selectFirst [EvaluationChecksum ==. (outChecksum out),
                                     EvaluationTest ==. mainTestId]
                                    []
  return $ case mEvaluation of
             Just evaluation -> evaluationScore $ entityVal evaluation
             Nothing -> Nothing

getSubmission :: UserId -> Maybe TeamId -> Key Repo -> SHA1 -> Key Challenge -> Text -> Channel -> Handler (Key Submission)
getSubmission userId mTeamId repoId commit challengeId subDescription chan = do
  challenge <- runDB $ get404 challengeId
  maybeSubmission <- runDB $ getBy $ UniqueSubmissionRepoCommitChallenge repoId commit challengeId
  case maybeSubmission of
    Just (Entity submissionId _) -> do
      msg chan "Submission already there, re-checking"
      return submissionId
    Nothing -> do
      msg chan "Creating new submission"
      time <- liftIO getCurrentTime
      runDB $ insert $ Submission {
        submissionRepo=repoId,
        submissionCommit=commit,
        submissionChallenge=challengeId,
        submissionDescription=subDescription,
        submissionStamp=time,
        submissionSubmitter=userId,
        submissionIsPublic=False,
        submissionIsHidden=False,
        submissionVersion=challengeVersion challenge,
        submissionTeam=mTeamId }

getSubmissionRepo :: UserId -> Key Challenge -> RepoSpec -> Channel -> Handler (Maybe (Key Repo))
getSubmissionRepo userId challengeId repoSpec chan = getPossiblyExistingRepo checkRepoAvailibility userId challengeId repoSpec chan

checkRepoAvailibility :: Key Challenge -> Key Repo -> Channel -> Handler Bool
checkRepoAvailibility challengeId repoId chan = do
  maybeOtherChallengeId <- runDB $ selectFirst ( [ChallengePublicRepo ==. repoId]
                                                 ||. [ChallengePrivateRepo ==. repoId]) []
  case maybeOtherChallengeId of
    Just _ -> do
      err chan "Repository already used as a challenge repo, please use a different repo or a different branch"
      return False
    Nothing -> do
      maybeOtherSubmissionId <- runDB $ selectFirst [SubmissionRepo ==. repoId,
                                                    SubmissionChallenge !=. challengeId] []
      case maybeOtherSubmissionId of
        Just _ -> do
          err chan "Repository already used as a submission repo for a different challenge, please use a different repo or a different branch"
          return False
        Nothing -> return True

challengeSubmissionWidget :: (ToMarkup a1, ToWidget App a2) => a2 -> a1 -> Challenge -> WidgetFor App ()
challengeSubmissionWidget formWidget formEnctype challenge = $(widgetFile "challenge-submission")

externalRepoInfo :: AppSettings -> WidgetFor site ()
externalRepoInfo settings = $(widgetFile "external-repo")

data ChallengeSubmissionData = ChallengeSubmissionData {
  challengeSubmissionDataDescription :: Maybe Text,
  challengeSubmissionDataTags :: Maybe Text,
  challengeSubmissionDataRepo :: RepoSpec,
  challengeSubmissionDataTeam :: Maybe TeamId }


fetchUserTeams :: (YesodPersist site, BackendCompatible SqlBackend (YesodPersistBackend site), PersistQueryRead (YesodPersistBackend site), PersistUniqueRead (YesodPersistBackend site)) => Key User -> HandlerFor site [Entity Team]
fetchUserTeams userId = runDB $ E.select $ E.from $ \(team, teamMember) -> do
                                E.where_ (teamMember ^. TeamMemberTeam E.==. team ^. TeamId
                                          E.&&. teamMember ^. TeamMemberUser E.==. E.val userId)
                                E.orderBy [E.desc (teamMember ^. TeamMemberIsCaptain), E.asc (team ^. TeamIdent)]
                                return team

fetchDefaultTeam :: Key User -> HandlerFor App (Maybe (Key Team))
fetchDefaultTeam userId = do
  myTeams <- fetchUserTeams userId
  app <- getYesod
  let autoTeam = appAutoTeam $ appSettings app
  if autoTeam
    then
      return $ entityKey <$> listToMaybe myTeams
    else
      return Nothing


submissionForm :: UserId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Maybe TeamId) -> Form ChallengeSubmissionData
submissionForm userId defaultUrl defBranch defaultGitAnnexRemote defaultTeam = renderBootstrap3 BootstrapBasicForm $ ChallengeSubmissionData
    <$> aopt textField (fieldWithTooltip MsgSubmissionDescription MsgSubmissionDescriptionTooltip) Nothing
    <*> aopt textField (tagsfs MsgSubmissionTags) Nothing
    <*> (RepoSpec <$> areq textField (bfs MsgSubmissionUrl) defaultUrl
                  <*> areq textField (bfs MsgSubmissionBranch) defBranch
                  <*> aopt textField (bfs MsgSubmissionGitAnnexRemote) (Just defaultGitAnnexRemote))
    <*> aopt (selectField teams) (bfs MsgAsTeam) defaultTeam
    where teams = do
            myTeams <- fetchUserTeams userId
            optionsPairs $ map (\t -> (teamIdent $ entityVal t, entityKey t)) myTeams

getCurrentTimeR :: Handler Value
getCurrentTimeR = do
  theNow <- liftIO $ getCurrentTime
  return $ toJSON theNow

getFormatAsLocalTimeR :: String -> Handler Value
getFormatAsLocalTimeR t = do
  let ut = TR.read $ DLU.replace "T" " " $ DLU.replace "Z" " " t
  tz <- liftIO $ getTimeZone ut
  return $ toJSON $ utcToLocalTime tz ut


getMyEvaluationTriggerTokenJsonR :: Handler Value
getMyEvaluationTriggerTokenJsonR = do
  (Entity _ user) <- requireAuthPossiblyByToken
  return $ String $ fromMaybe "" $ userTriggerToken user

getAddUserR :: Handler Value
getAddUserR = do
  mInfo <- authorizationTokenAuth
  case mInfo of
    Just infos -> do
      let ident = jwtAuthInfoIdent infos
      x <- runDB $ getBy $ UniqueUser ident
      case x of
        Just _ -> return $ Bool False
        Nothing -> do
          -- family or given name can be used for a team name
          -- (as an ugly work-around...), hence we look at TEAM_FIELD and when
          -- it is set to "given_name" or "family_name" it is not
          -- considered a part of the user's
          -- name

          app <- getYesod
          let teamField = appTeamField $ appSettings app

          let uname = intercalate " " $ catMaybes (
                [if teamField /= (Just "given_name")
                 then jwtAuthInfoGivenName infos
                 else Nothing,
                 if teamField /= (Just "family_name")
                 then jwtAuthInfoFamilyName infos
                 else Nothing])


          let mUName = if (null uname)
                      then Nothing
                      else (Just uname)

          userId <- runDB $ insert User
            { userIdent = ident
            , userPassword = Nothing
            , userName = mUName
            , userIsAdmin = False
            , userLocalId = Nothing
            , userIsAnonymous = False
            , userAvatar = Nothing
            , userVerificationKey = Nothing
            , userKeyExpirationDate = Nothing
            , userTriggerToken = Nothing
            , userAltRepoScheme = Nothing
            }

          case teamField of
            Just teamFieldName -> do
              case jwtAuthInfoCustomField teamFieldName infos of
                Just team -> do
                  t <- runDB $ getBy $ UniqueTeam team
                  (teamId, isCaptain) <- case t of
                    Just (Entity existingTeamId _) -> return (existingTeamId, False)
                    Nothing -> do
                      newTeamId <- runDB $ insert $ Team {teamIdent = team,
                                                         teamAvatar = Nothing}
                      return (newTeamId, True)
                  runDB $ addMemberToTeam userId teamId isCaptain
                  return ()
                Nothing -> return ()
            Nothing -> return ()

          return $ Bool True
    Nothing -> return $ Bool False

addUserApi :: Swagger
addUserApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareAddUserApi mempty

declareAddUserApi :: Declare (Definitions Schema) Swagger
declareAddUserApi = do
  -- param schemas
  response <- declareResponse (Proxy :: Proxy Bool)

  return $ mempty
    & paths .~
        fromList [ ("/api/add-user",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Creates a new user"
                                        & at 200 ?~ Inline response))
                 ]

currentTimeApi :: Swagger
currentTimeApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareCurrentTimeApi mempty

declareCurrentTimeApi :: Declare (Definitions Schema) Swagger
declareCurrentTimeApi = do
  -- param schemas
  response <- declareResponse (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        fromList [ ("/api/current-time",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Returns the current time as measured on the server side"
                                        & at 200 ?~ Inline response))
                 ]


formatAsLocalTimeApi :: Swagger
formatAsLocalTimeApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareFormatAsLocalTimeApi mempty

declareFormatAsLocalTimeApi :: Declare (Definitions Schema) Swagger
declareFormatAsLocalTimeApi = do
  -- param schemas
  response <- declareResponse (Proxy :: Proxy String)
  let utcTimeSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        fromList [ ("/api/format-as-local-time/{utcTime}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Formats the given UTC time stamp as a local time"
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "utcTime"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ utcTimeSchema) ]
                                        & at 200 ?~ Inline response))
                 ]



myEvaluationTriggerTokenApi :: Swagger
myEvaluationTriggerTokenApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareMyEvaluationTriggerTokenApi mempty

declareMyEvaluationTriggerTokenApi :: Declare (Definitions Schema) Swagger
declareMyEvaluationTriggerTokenApi = do
  -- param schemas
  response <- declareResponse (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        fromList [ ("/api/my-evaluation-trigger-token",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Returns the token for triggering evaluations"
                                        & at 200 ?~ Inline response))
                 ]



declareAllSubmissionsApi :: String -> String -> Declare (Definitions Schema) Swagger
declareAllSubmissionsApi q d = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  allSubmissionsResponse <- declareResponse (Proxy :: Proxy SubmissionsView)

  return $ mempty
    & paths .~
        fromList [ ("/api/" ++ q ++ "/{challengeName}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "challengeName"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ T.pack d
                                        & at 200 ?~ Inline allSubmissionsResponse))
                 ]


allSubmissionsApi :: Swagger
allSubmissionsApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareAllSubmissionsApi "challenge-all-submissions" "Returns all submissions for a challenge") mempty

mySubmissionsApi :: Swagger
mySubmissionsApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareAllSubmissionsApi "challenge-my-submissions" "Returns all submissions for a challenge for the user") mempty

getChallengeAllSubmissionsJsonR :: Text -> Handler Value
getChallengeAllSubmissionsJsonR = makeSureChallengeAccessible (const getChallengeAllSubmissionsJsonR')

getChallengeAllSubmissionsJsonR' :: Entity Challenge -> Handler Value
getChallengeAllSubmissionsJsonR' (Entity _ challenge) = do
  v <- fetchAllSubmissionsView $ challengeName challenge
  return $ toJSON v

getChallengeMySubmissionsJsonR :: Text -> Handler Value
getChallengeMySubmissionsJsonR = makeSureChallengeAccessible (const getChallengeMySubmissionsJsonR')

getChallengeMySubmissionsJsonR' :: (Entity Challenge) -> Handler Value
getChallengeMySubmissionsJsonR' (Entity _ challenge) = do
  v <- fetchMySubmissionsView $ challengeName challenge
  return $ toJSON v

fetchAllSubmissionsView :: Text -> Handler SubmissionsView
fetchAllSubmissionsView challengeName = do
  fetchChallengeSubmissionsView (const True) challengeName

fetchMySubmissionsView :: Text -> Handler SubmissionsView
fetchMySubmissionsView challengeName = do
  Entity userId _ <- requireAuthPossiblyByToken
  fetchChallengeSubmissionsView (\(Entity _ submission) -> (submissionSubmitter submission == userId)) challengeName

convertTagInfoToView :: (Entity Import.Tag, Entity SubmissionTag) -> TagView
convertTagInfoToView tagInfo =
  TagView {
     tagViewName = tagName $ entityVal $ fst tagInfo,
     tagViewDescription = tagDescription $ entityVal $ fst tagInfo,
     tagViewColor = tagColor $ entityVal $ fst tagInfo,
     tagViewAccepted = submissionTagAccepted $ entityVal $ snd tagInfo
     }

convertEvaluationToView :: Map TestReference Evaluation -> DisclosedInfo -> Entity Test -> Maybe EvaluationView
convertEvaluationToView theMapping disclosedInfo entTest =
  case join $ evaluationScore <$> mEvaluation of
    Just s ->
      Just $ EvaluationView {
        evaluationViewScore = formatTruncatedScore disclosedInfo formattingOps mEvaluation,
        evaluationViewFullScore =
            case disclosedInfo of
              DisclosedInfo Nothing -> Just s
              _ -> Nothing,
        evaluationViewTest = testRef
        }
    Nothing -> Nothing
  where mEvaluation = Map.lookup testRef theMapping
        formattingOps = getTestFormattingOpts $ entityVal entTest
        testRef = getTestReference entTest

-- convertTableEntryToView :: Maybe UserId -> [Entity Test] -> TableEntry -> SubmissionView
convertTableEntryToView :: DisclosedInfo -> [Entity Test] -> TableEntry -> HandlerFor App SubmissionView
convertTableEntryToView disclosedInfo tests entry = do
  mUserEnt <- maybeAuthPossiblyByToken

  isReevaluable <- runDB $ canBeReevaluated $ entityKey $ tableEntrySubmission entry
  let isVisible = True

  return $ SubmissionView {
    submissionViewId = fromSqlKey $ entityKey $ tableEntrySubmission entry,
    submissionViewVariantId = fromSqlKey $ entityKey $ tableEntryVariant entry,
    submissionViewRank = tableEntryRank entry,
    submissionViewSubmitter = formatSubmitter $ entityVal $ tableEntrySubmitter entry,
    submissionViewWhen = submissionStamp submission,
    submissionViewVersion = tableEntryVersion entry,
    submissionViewDescription = descriptionToBeShown submission
                                                     (entityVal $ tableEntryVariant entry)
                                                     (map entityVal $ tableEntryParams entry),
    submissionViewTags = Import.map convertTagInfoToView $ tableEntryTagsInfo entry,
    submissionViewHash = fromSHA1ToText $ submissionCommit submission,
    submissionViewEvaluations = catMaybes $ applyDisclosedInfoOnLast disclosedInfo (convertEvaluationToView $ tableEntryMapping entry) tests,
    submissionViewIsOwner = (entityKey <$> mUserEnt) == Just (submissionSubmitter submission),
    submissionViewIsReevaluable = isReevaluable,
    submissionViewIsVisible = isVisible,
    submissionViewIsPublic = submissionIsPublic submission,
    submissionViewTeam = teamIdent <$> entityVal <$> tableEntryTeam entry
  }
  where submission = entityVal $ tableEntrySubmission entry

fetchChallengeSubmissionsView :: ((Entity Submission) -> Bool) -> Text -> Handler SubmissionsView
fetchChallengeSubmissionsView condition challengeName = do
  Entity challengeId challenge <- runDB $ getBy404 $ UniqueName challengeName
  disclosedInfo <- fetchDisclosedInfo challenge

  (evaluationMaps, tests') <- runDB $ getChallengeSubmissionInfos 1 condition (const True) id challengeId
  let tests = sortBy testComparator tests'

  submissions <- mapM (convertTableEntryToView disclosedInfo tests) evaluationMaps

  return $ SubmissionsView {
    submissionsViewSubmissions = submissions,
    submissionsViewTests = map getTestReference tests
  }


-- TODO switch to fetchChallengeSubmissionSview
getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR = makeSureChallengeAccessible getChallengeMySubmissionsR'

getChallengeMySubmissionsR' :: Maybe (Entity User) -> Entity Challenge -> Handler Html
getChallengeMySubmissionsR' mUserEnt (Entity _ challenge) = do
  let Just (Entity userId _) = mUserEnt
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) $ challengeName challenge

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR = makeSureChallengeAccessible (const getChallengeAllSubmissionsR')

getChallengeAllSubmissionsR' :: Entity Challenge -> Handler Html
getChallengeAllSubmissionsR' (Entity _ challenge) = getChallengeSubmissions (\_ -> True) $ challengeName challenge

data EvaluationView = EvaluationView {
  evaluationViewScore :: Text,
  evaluationViewFullScore :: Maybe Double,
  evaluationViewTest :: TestReference
}

instance ToJSON EvaluationView where
    toJSON e = object
        [ "score" .= evaluationViewScore e
        , "full-score" .= evaluationViewFullScore e
        , "test" .= evaluationViewTest e
        ]

instance ToSchema EvaluationView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    doubleSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Double)
    testRefSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy TestReference)
    return $ NamedSchema (Just "Evaluation") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("score", stringSchema)
                     , ("full-score", doubleSchema)
                     , ("test", testRefSchema)
                    ]
        & required .~ [ "score", "full-score", "test" ]


data TagView = TagView {
  tagViewName :: Text,
  tagViewDescription :: Maybe Text,
  tagViewColor :: Maybe Text,
  tagViewAccepted :: Maybe Bool }

instance ToJSON TagView where
  toJSON t = object
       [ "name" .= tagViewName t
       , "description" .= tagViewDescription t
       , "color" .= tagViewColor t
       , "accepted" .= tagViewAccepted t
       ]

instance ToSchema TagView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    boolSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Bool)
    return $ NamedSchema (Just "TagView") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                     , ("description", stringSchema)
                     , ("color", stringSchema)
                     , ("accepted", boolSchema)
                    ]
        & required .~ [ "name", "color", "description" ]


data SubmissionView = SubmissionView {
  submissionViewId :: Int64,
  submissionViewVariantId :: Int64,
  submissionViewRank :: Int,
  submissionViewSubmitter :: Text,
  submissionViewWhen :: UTCTime,
  submissionViewVersion :: ((Int, Int, Int), Maybe Import.Tag),
  submissionViewDescription :: Text,
  submissionViewTags :: [TagView],
  submissionViewHash :: Text,
  submissionViewEvaluations :: [EvaluationView],
  submissionViewIsOwner :: Bool,
  submissionViewIsReevaluable :: Bool,
  submissionViewIsVisible :: Bool,
  submissionViewIsPublic :: Bool,
  submissionViewTeam :: Maybe Text
}

instance ToJSON SubmissionView where
  toJSON s = object
   ["id" .= submissionViewId s
    , "variant" .= submissionViewVariantId s
    , "rank" .= submissionViewRank s
    , "submitter" .= submissionViewSubmitter s
    , "when" .= submissionViewWhen s
    , "version" .= (fst $ submissionViewVersion s)
    , "phase" .= (snd $ submissionViewVersion s)
    , "description" .= submissionViewDescription s
    , "tags" .= submissionViewTags s
    , "hash" .= submissionViewHash s
    , "evaluations" .= submissionViewEvaluations s
    , "isOwner" .= submissionViewIsOwner s
    , "isReevaluable" .= submissionViewIsReevaluable s
    , "isVisible" .= submissionViewIsVisible s
    , "isPublic" .= submissionViewIsPublic s
    , "team" .= submissionViewTeam s
    ]

instance ToSchema SubmissionView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    boolSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Bool)
    intSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Int)
    tagsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [TagView])
    evalsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [EvaluationView])
    tagSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [Import.Tag])
    return $ NamedSchema (Just "SubmissionView") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("id", submissionIdSchema)
                     , ("variant", variantIdSchema)
                     , ("rank", intSchema)
                     , ("submitter", submitterSchema)
                     , ("when", stringSchema)
                     , ("version", versionSchema)
                     , ("phase", tagSchema)
                     , ("description", stringSchema)
                     , ("tags", tagsSchema)
                     , ("hash", hashSchema)
                     , ("evaluations", evalsSchema)
                     , ("isOwner", boolSchema)
                     , ("isReevaluable", boolSchema)
                     , ("isVisible", isVisibleSchema)
                     , ("isPublic", isPublicSchema)
                     , ("team", stringSchema)
                    ]
        & required .~ [ "id", "variant", "rank", "submitter", "when", "version",
                        "description", "tags", "hash", "evaluations",
                        "isOwner", "isReevaluable", "isVisible", "isPublic" ]

data SubmissionsView = SubmissionsView {
  submissionsViewSubmissions :: [SubmissionView],
  submissionsViewTests :: [TestReference]
}

instance ToJSON SubmissionsView where
  toJSON ss = object
    [ "tests" .= submissionsViewTests ss,
      "submissions" .= submissionsViewSubmissions ss
    ]

instance ToSchema SubmissionsView where
  declareNamedSchema _ = do
    submissionViewsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [SubmissionView])
    testRefsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [TestReference])
    return $ NamedSchema (Just "SubmissionsView") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("submissions", submissionViewsSchema)
                     , ("tests", testRefsSchema)
                    ]
        & required .~ [ "tests", "submission" ]

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition challengeName = do
  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName challengeName
  (evaluationMaps, tests') <- runDB $ getChallengeSubmissionInfos 1 condition (const True) id challengeId
  let tests = sortBy testComparator tests'
  mauth <- maybeAuth
  let muserId = (\(Entity uid _) -> uid) <$> mauth

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  let params = getNumericalParams evaluationMaps

  challengeLayout True challengeEnt (challengeAllSubmissionsWidget muserId
                                                                   challenge
                                                                   scheme
                                                                   challengeRepo
                                                                   evaluationMaps
                                                                   tests
                                                                   params)

maxNumberOfParamGraphs :: Int
maxNumberOfParamGraphs = 20

getNumericalParams :: [TableEntry] -> [Text]
getNumericalParams entries =
  take maxNumberOfParamGraphs
  $ filter (isRelevantParam entries)
  $ getAllParams entries

minNumberOfValuesToBeShown :: Int
minNumberOfValuesToBeShown = 3

isRelevantParam :: [TableEntry] -> Text -> Bool
isRelevantParam entries param =
  all doesTextRepresentNumber allValues && length allValues >= minNumberOfValuesToBeShown
  where allValues = getParamValueSet entries param

getParamValueSet :: [TableEntry] -> Text -> [Text]
getParamValueSet entries param =
  nub
  $ concat
  $ map ((map parameterValue)
         . (filter (\p -> parameterName p == param))
         . (map entityVal)
         . tableEntryParams) entries

doesTextRepresentNumber :: Text -> Bool
doesTextRepresentNumber t = isJust $ ((TR.readMaybe $ T.unpack t) :: Maybe Double)

getAllParams :: [TableEntry] -> [Text]
getAllParams entries = sort
               $ nub
               $ concat
               $ map (\entry -> map (parameterName . entityVal) (tableEntryParams entry)) entries


challengeAllSubmissionsWidget :: Maybe UserId
                                -> Challenge
                                -> RepoScheme
                                -> Repo
                                -> [TableEntry]
                                -> [Entity Test]
                                -> [Text]
                                -> WidgetFor App ()
challengeAllSubmissionsWidget muserId challenge scheme challengeRepo submissions tests params =
  do
    disclosedInfo <- liftHandler $ fetchDisclosedInfo challenge
    $(widgetFile "challenge-all-submissions")
  where delta = Number 4
        higherTheBetterArray = getIsHigherTheBetterArray $ map entityVal tests

paramGraphsWidget :: Challenge -> [Entity Test] -> [Text] -> WidgetFor App ()
paramGraphsWidget challenge tests params = $(widgetFile "param-graphs")
  where chartJSs = getChartJss challenge selectedTests params
        selectedTests = reverse $ getMainTests tests

getChartJss :: Challenge -> [Entity Test] -> [Text] -> JavascriptUrl (Route App)
getChartJss challenge tests params =
  mconcat $ [(getChartJs challenge test param) | test <- tests, param <- params]

getChartJs :: Challenge
             -> Entity Test
             -> Text
             -> JavascriptUrl (Route App)
getChartJs challenge (Entity testId test) param = [julius|
$.getJSON("@{ChallengeParamGraphDataR (challengeName challenge) testId param}", function(data) {
        c3.generate({
                bindto: '#chart-' + #{toJSON param} + '-' + #{toJSON testId},
                data: data,
                axis: {
                   x: {
                     label: #{toJSON param},
                   },
                   y: {
                     label: #{toJSON testFormatted},
                   }
                }
    }) });
|]
   where testFormatted = formatTest test


challengeLayout :: Bool -> Entity Challenge -> WidgetFor App () -> HandlerFor App Html
challengeLayout withHeader (Entity challengeId challenge) widget = do
  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON
  tags <- runDB $ getChallengeTags challengeId
  theVersion <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  let versionFormatted = formatVersion ((versionMajor $ entityVal theVersion),
                                        (versionMinor $ entityVal theVersion),
                                        (versionPatch $ entityVal theVersion))
  maybeUser <- maybeAuth
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")

getTestProgressR :: Int -> Int -> Handler TypedContent
getTestProgressR m d = runViewProgress $ doTestProgress m d

getTestProgressJsonR :: Int -> Int -> Handler Value
getTestProgressJsonR m d = do
  _ <- requireAuthPossiblyByToken
  runViewProgressAsynchronously $ doTestProgress m d

declareTestProgressSwagger :: Declare (Definitions Schema) Swagger
declareTestProgressSwagger = do
  -- param schemas
  let numberSchema = toParamSchema (Proxy :: Proxy Int)

  numberResponse      <- declareResponse (Proxy :: Proxy Int)

  return $ mempty
    & paths .~
        fromList [ ("/api/test-progress/{num}/{delay}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "num"
                                                          & description ?~ "The number up to which to count"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ numberSchema),
                                                          Inline $ mempty
                                                          & name .~ "delay"
                                                          & description ?~ "Delay in seconds"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ numberSchema)
                                                          ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Counts up to a given number, returns an ID of an asynchronous job. This is just a sample end-point for testing logging of asynchronous jobs."
                                        & at 200 ?~ Inline numberResponse))
                 ]

testProgressApi :: Swagger
testProgressApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareTestProgressSwagger mempty


doTestProgress :: Int -> Int -> Channel -> Handler ()
doTestProgress m d chan = do
  _ <- forM [1..m] $ (\i -> do
                       msg chan $ (Data.Text.pack $ ("GO\n" ++ show i))
                       liftIO $ threadDelay (d * 1000000)
                       return ())
  return ()


declareViewProgressWithWebSocketsSwagger :: Declare (Definitions Schema) Swagger
declareViewProgressWithWebSocketsSwagger = do
  -- param schemas
  let numberSchema = toParamSchema (Proxy :: Proxy Int)

  numberResponse      <- declareResponse (Proxy :: Proxy Int)

  return $ mempty
    & paths .~
        fromList [ ("/api/view-progress-with-web-sockets/{jobId}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "jobId"
                                                          & description ?~ "The ID for the job to be shown"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ numberSchema)]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Initiates a web socket communication with which progress logs can be read. Returns just the Job ID (the same number as the parameter)"
                                        & at 200 ?~ Inline numberResponse))
                 ]

viewProgressWithWebSockets :: Swagger
viewProgressWithWebSockets = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareViewProgressWithWebSocketsSwagger mempty


declareViewProgressLogSwagger :: Declare (Definitions Schema) Swagger
declareViewProgressLogSwagger = do
  let numberSchema = toParamSchema (Proxy :: Proxy Int)

  numberResponse      <- declareResponse (Proxy :: Proxy Int)

  return $ mempty
    & paths .~
        fromList [ ("/api/view-progress-log/{jobId}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "jobId"
                                                          & description ?~ "The ID for the job to be shown"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ numberSchema)]
                                        & produces ?~ MimeList ["text/html"]
                                        & description ?~ "Returns HTML code with embedded JS script for showing logs via web sockets"
                                        & at 200 ?~ Inline numberResponse))
                 ]

viewProgressLog :: Swagger
viewProgressLog = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareViewProgressLogSwagger mempty
