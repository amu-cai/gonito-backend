module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)



import qualified Data.Text.Lazy as TL
import           Text.Markdown

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import qualified Yesod.Table as Table

import Handler.Extract
import Handler.Shared
import Handler.Runner
import Handler.Tables
import Handler.TagUtils
import Handler.MakePublic

import qualified Text.Read as TR

import GEval.Core
import GEval.OptionsParser
import GEval.ParseParams (parseParamsFromFilePath, OutputFileParsed(..))

import PersistSHA1

import Options.Applicative

import System.IO (readFile)

import System.FilePath (takeFileName, dropExtensions)

import Data.Attoparsec.Text

import Data.Text (pack, unpack)

import Data.Conduit.SmartSource

import Data.List (nub)

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  Just repo <- runDB $ get $ challengePublicRepo challenge
  (mainTest, leaderboard, (entries, tests)) <- getLeaderboardEntries challengeId
  mauth <- maybeAuth
  let muserId = (\(Entity uid _) -> uid) <$> mauth

  let params = getNumericalParams entries

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  challengeLayout True challenge (showChallengeWidget muserId
                                                      challenge scheme
                                                      challengeRepo
                                                      mainTest
                                                      repo
                                                      leaderboard
                                                      params
                                                      tests)

getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  readme <- challengeReadme name
  challengeLayout False challenge $ toWidget readme

challengeReadme :: Text -> HandlerFor App Html
challengeReadme name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  let repoId = challengePublicRepo challenge
  repoDir <- getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  contents <- liftIO $ System.IO.readFile readmeFilePath
  return $ markdown def $ TL.pack contents

showChallengeWidget :: Maybe UserId
                      -> Challenge
                      -> RepoScheme
                      -> Repo
                      -> Test
                      -> Repo
                      -> [LeaderboardEntry]
                      -> [Text]
                      -> [Entity Test]
                      -> WidgetFor App ()
showChallengeWidget muserId
                    challenge
                    scheme
                    challengeRepo
                    test
                    repo
                    leaderboard
                    params
                    tests
  = $(widgetFile "show-challenge")
  where leaderboardWithRanks = zip [1..] leaderboard
        maybeRepoLink = getRepoLink repo

getRepoLink :: Repo -> Maybe Text
getRepoLink repo
  | sitePrefix `isPrefixOf` url = Just $ (browsableGitRepo bareRepoName) ++ "/" ++ (repoBranch repo)
  | otherwise = Nothing
  where sitePrefix = "git://gonito.net/" :: Text
        sitePrefixLen = length sitePrefix
        url = repoUrl repo
        bareRepoName = drop sitePrefixLen url

getChallengeHowToR :: Text -> Handler Html
getChallengeHowToR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  maybeUser <- maybeAuth

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
  challengeLayout False challenge (challengeHowTo
                                   challenge
                                   settings
                                   repo
                                   (idToBeShown challenge maybeUser)
                                   isIDSet
                                   isSSHUploaded
                                   mToken)

idToBeShown :: p -> Maybe (Entity User) -> Text
idToBeShown _ maybeUser =
  case maybeUser of
   Just user ->  case userLocalId $ entityVal user of
                 Just localId -> localId
                 Nothing -> defaultIdToBe
   Nothing -> defaultIdToBe
  where defaultIdToBe = "YOURID" :: Text

defaultRepo :: RepoScheme -> Challenge -> Repo -> Maybe (Entity User) -> Text
defaultRepo SelfHosted challenge _ maybeUser = "ssh://gitolite@gonito.net/" ++ (idToBeShown challenge maybeUser) ++ "/" ++ (challengeName challenge)
defaultRepo Branches _ repo _ = repoUrl repo

defaultBranch :: IsString a => RepoScheme -> Maybe a
defaultBranch SelfHosted = Just "master"
defaultBranch Branches = Nothing

challengeHowTo challenge settings repo shownId isIDSet isSSHUploaded mToken = $(widgetFile "challenge-how-to")
  where myBranch = case appRepoScheme settings of
          SelfHosted -> "master" :: Text
          _ -> "my-brilliant-branch"

getChallengeSubmissionR :: Text -> Handler Html
getChallengeSubmissionR name = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
   maybeUser <- maybeAuth

   Just repo <- runDB $ get $ challengePublicRepo challenge
   app <- getYesod
   let scheme = appRepoScheme $ appSettings app

   (formWidget, formEnctype) <- generateFormPost $ submissionForm (Just $ defaultRepo scheme challenge repo maybeUser) (defaultBranch scheme) (repoGitAnnexRemote repo)
   challengeLayout True challenge $ challengeSubmissionWidget formWidget formEnctype challenge

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR name = do
    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName name
    ((result, _), _) <- runFormPost $ submissionForm Nothing Nothing Nothing
    let submissionData = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just (mDescription, mTags, submissionUrl, submissionBranch, submissionGitAnnexRemote) = submissionData

    userId <- requireAuthId
    runViewProgress $ doCreateSubmission userId challengeId mDescription mTags RepoSpec {
                                                                                  repoSpecUrl=submissionUrl,
                                                                                  repoSpecBranch=submissionBranch,
                                                                                  repoSpecGitAnnexRemote=submissionGitAnnexRemote}

postTriggerLocallyR :: Handler TypedContent
postTriggerLocallyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just localId) <- lookupPostParam "user"
  mBranch <- lookupPostParam "branch"
  [Entity userId _] <- runDB $ selectList [UserLocalId ==. Just localId] []
  let localRepo = gitServer ++ localId ++ "/" ++ challengeName
  trigger userId challengeName localRepo mBranch

postTriggerRemotelyR :: Handler TypedContent
postTriggerRemotelyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just url) <- lookupPostParam "url"
  (Just token) <- lookupPostParam "token"
  mBranch <- lookupPostParam "branch"
  [Entity userId _] <- runDB $ selectList [UserTriggerToken ==. Just token] []
  trigger userId challengeName url mBranch

trigger :: UserId -> Text -> Text -> Maybe Text -> Handler TypedContent
trigger userId challengeName url mBranch = do
  let branch = fromMaybe "master" mBranch
  mChallengeEnt <- runDB $ getBy $ UniqueName challengeName
  case mChallengeEnt of
    Just (Entity challengeId _) -> runOpenViewProgress $ doCreateSubmission userId challengeId
                                                                           Nothing Nothing
                                                                           RepoSpec {repoSpecUrl=url,
                                                                                     repoSpecBranch=branch,
                                                                                     repoSpecGitAnnexRemote=Nothing}
    Nothing -> return $ toTypedContent (("Unknown challenge `" ++ (Data.Text.unpack challengeName) ++ "`. Cannot be triggered, must be submitted manually at Gonito.net!\n") :: String)

doCreateSubmission :: UserId -> Key Challenge -> Maybe Text -> Maybe Text -> RepoSpec -> Channel -> Handler ()
doCreateSubmission userId challengeId mDescription mTags repoSpec chan = do
  maybeRepoKey <- getSubmissionRepo userId challengeId repoSpec chan
  case maybeRepoKey of
    Just repoId -> do
      repo <- runDB $ get404 repoId

      repoDir <- getRepoDir repoId
      commitMessage <- getLastCommitMessage repoDir chan
      let (mCommitDescription, mCommitTags) = parseCommitMessage commitMessage

      submissionId <- getSubmission userId repoId (repoCurrentCommit repo) challengeId (fromMaybe (fromMaybe "???" mCommitDescription) mDescription) chan
      _ <- getOuts chan submissionId

      runDB $ addTags submissionId (if isNothing mTags then mCommitTags else mTags) []
      msg chan "SUBMISSION CREATED"

      app <- getYesod
      if appAutoOpening $ appSettings app
        then
          doMakePublic userId submissionId chan
        else
          return ()
    Nothing -> return ()

getSubmission :: UserId -> Key Repo -> SHA1 -> Key Challenge -> Text -> Channel -> Handler (Key Submission)
getSubmission userId repoId commit challengeId description chan = do
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
        submissionDescription=description,
        submissionStamp=time,
        submissionSubmitter=userId,
        submissionIsPublic=False,
        submissionIsHidden=Just False }

parseCommitMessage :: Maybe Text -> (Maybe Text, Maybe Text)
parseCommitMessage Nothing = (Nothing, Nothing)
parseCommitMessage (Just commitMessage) =
  case parseOnly commitMessageParser commitMessage of
    Left _ -> (Nothing, Nothing)
    Right (d, ts) -> (d, ts)

commitMessageParser :: Data.Attoparsec.Text.Parser (Maybe Text, Maybe Text)
commitMessageParser = do
  skipMany emptyLine
  d <- nonEmptyLine
  mTs <- (do
          ts <- findTagsLine
          return $ Just ts) <|> (return Nothing)
  return (Just d, mTs)

findTagsLine :: Data.Attoparsec.Text.Parser Text
findTagsLine = tagsLine <|> (anyLine >> findTagsLine)

tagsLine :: Data.Attoparsec.Text.Parser Text
tagsLine = do
  _ <- (string "tags" <|> string "labels" <|> string "Tags" <|> string "Labels")
  _ <- char ':'
  skipMany space
  s <- many notEndOfLine
  endOfLine
  return $ Data.Text.pack s

commaSep :: Data.Attoparsec.Text.Parser a -> Data.Attoparsec.Text.Parser [a]
commaSep p  = p `sepBy` (skipMany space *> char ',' *> skipMany space)

nonEmptyLine :: Data.Attoparsec.Text.Parser Text
nonEmptyLine = do
  skipMany space
  l1 <- notSpace
  l <- (many notEndOfLine)
  endOfLine
  return $ Data.Text.pack (l1:l)

anyLine :: Data.Attoparsec.Text.Parser ()
anyLine = do
  skipMany notEndOfLine
  endOfLine

notSpace :: Data.Attoparsec.Text.Parser Char
notSpace = satisfy (\c -> c /= '\r' && c /= '\n' && c /= ' ' && c /= '\t')

notEndOfLine :: Data.Attoparsec.Text.Parser Char
notEndOfLine = satisfy (\c -> c /= '\r' && c /= '\n')

emptyLine :: Data.Attoparsec.Text.Parser ()
emptyLine = do
  many space *> endOfLine

getOuts :: Channel -> Key Submission -> Handler ([Out])
getOuts chan submissionId = do
  submission <- runDB $ get404 submissionId
  let challengeId = submissionChallenge submission
  repoDir <- getRepoDir $ submissionRepo submission
  activeTests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []

  outs' <- mapM (outsForTest repoDir submissionId) activeTests
  let outs = concat outs'

  mapM_ checkOrInsertOut outs
  mapM_ (checkOrInsertEvaluation repoDir chan) outs
  return outs

outFileName :: FilePath
outFileName = "out.tsv"

getOutFilePath :: FilePath -> Test -> FilePath
getOutFilePath repoDir test = repoDir </> (T.unpack $ testName test) </> outFileName

findOutFile :: FilePath -> Test -> IO (Maybe FilePath)
findOutFile repoDir test = do
  let baseOut = getOutFilePath repoDir test
  findFilePossiblyCompressed baseOut

doesOutExist :: FilePath -> Entity Test -> IO Bool
doesOutExist repoDir (Entity _ test) = do
  result <- findOutFile repoDir test
  return $ isJust result

outForTest :: MonadIO m => FilePath -> FilePath -> Key Variant -> Entity Test -> m Out
outForTest repoDir outF variantId (Entity testId test) = do
  let outPath = repoDir </> (T.unpack $ testName test) </> outF
  checksum <- liftIO $ gatherSHA1ForCollectionOfFiles [outPath]
  return Out {
    outVariant=variantId,
    outTest=testId,
    outChecksum=SHA1 checksum }

outsForTest :: FilePath -> SubmissionId -> Entity Test -> HandlerFor App [Out]
outsForTest repoDir submissionId testEnt@(Entity _ test) = do
  outFiles <- liftIO $ outFilesForTest repoDir test

  forM outFiles $ \outFile -> do
    theVariant <- getVariant submissionId outFile
    outForTest repoDir outFile theVariant testEnt

-- returns the filename (not file path)
outFilesForTest :: FilePath -> Test -> IO [FilePath]
outFilesForTest repoDir test = do
    mMultipleOuts <- checkMultipleOutsCore repoDir (Data.Text.unpack $ testName test) "out.tsv"
    case mMultipleOuts of
      Just outFiles -> return $ map takeFileName outFiles
      Nothing -> do
        mOutFile <- findOutFile repoDir test
        case mOutFile of
          Just outF -> return [takeFileName outF]
          Nothing -> return []

getVariant :: SubmissionId -> FilePath -> Handler VariantId
getVariant submissionId outFilePath = runDB $ do
  let outFile = takeFileName outFilePath
  let name = Data.Text.pack $ dropExtensions outFile
  maybeVariant <- getBy $ UniqueVariantSubmissionName submissionId name
  case maybeVariant of
    Just (Entity vid _) -> return vid
    Nothing -> do
      vid <- insert $ Variant submissionId name
      let (OutputFileParsed _ paramMap) = parseParamsFromFilePath outFile
      forM_ (M.toList paramMap) $ \(param, val) -> do
        _ <- insert $ Parameter vid param val
        return ()
      return vid

checkOrInsertOut :: Out -> Handler ()
checkOrInsertOut out = do
  maybeOut <- runDB $ getBy $ UniqueOutVariantTestChecksum (outVariant out) (outTest out) (outChecksum out)
  case maybeOut of
    Just _ -> return ()
    Nothing -> (runDB $ insert out) >> return ()

checkOrInsertEvaluation :: FilePath -> Channel -> Out -> Handler ()
checkOrInsertEvaluation repoDir chan out = do
  test <- runDB $ get404 $ outTest out
  challenge <- runDB $ get404 $ testChallenge test
  maybeEvaluation <- runDB $ getBy $ UniqueEvaluationTestChecksum (outTest out) (outChecksum out)
  case maybeEvaluation of
    Just (Entity _ evaluation) -> do
      msg chan $ concat ["Already evaluated with score ", (T.pack $ fromMaybe "???" $ show <$> evaluationScore evaluation)]
    Nothing -> do
      msg chan $ "Start evaluation..."
      challengeDir <- getRepoDir $ challengePrivateRepo challenge
      variant <- runDB $ get404 $ outVariant out
      resultOrException <- liftIO $ rawEval challengeDir (testMetric test) repoDir (testName test) ((T.unpack $ variantName variant) <.> "tsv")
      case resultOrException of
        Right (Left _) -> do
          err chan "Cannot parse options, check the challenge repo"
        Right (Right (_, Just [(_, [result])])) -> do
          msg chan $ concat [ "Evaluated! Score ", (T.pack $ show result) ]
          time <- liftIO getCurrentTime
          _ <- runDB $ insert $ Evaluation {
            evaluationTest=outTest out,
            evaluationChecksum=outChecksum out,
            evaluationScore=Just result,
            evaluationErrorMessage=Nothing,
            evaluationStamp=time }
          msg chan "Evaluation done"
        Right (Right (_, Just _)) -> do
          err chan "Unexpected multiple results (???)"
        Right (Right (_, Nothing)) -> do
          err chan "Error during the evaluation"
        Left exception -> do
          err chan $ "Evaluation failed: " ++ (T.pack $ show exception)

rawEval :: FilePath -> Metric -> FilePath -> Text -> FilePath -> IO (Either GEvalException (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe [(SourceSpec, [MetricValue])])))
rawEval challengeDir metric repoDir name outF = Import.try (runGEvalGetOptions [
                                                          "--alt-metric", (show metric),
                                                          "--expected-directory", challengeDir,
                                                          "--out-directory", repoDir,
                                                          "--out-file", outF,
                                                          "--test-name", (T.unpack name)])

getSubmissionRepo :: UserId -> Key Challenge -> RepoSpec -> Channel -> Handler (Maybe (Key Repo))
getSubmissionRepo userId challengeId repoSpec chan = do
  let url = repoSpecUrl repoSpec
  let branch = repoSpecBranch repoSpec
  let gitAnnexRemote = repoSpecGitAnnexRemote repoSpec
  maybeRepo <- runDB $ getBy $ UniqueUrlBranch url branch
  case maybeRepo of
    Just (Entity repoId _) -> do
      msg chan "Repo already there"
      available <- checkRepoAvailibility challengeId repoId chan
      if available
         then
          do
           -- this is not completely right... some other thread
           -- might update this to a different value
           runDB $ update repoId [RepoGitAnnexRemote =. gitAnnexRemote]
           updateStatus <- updateRepo repoId chan
           if updateStatus
             then
               return $ Just repoId
             else
               return Nothing
         else
           return Nothing
    Nothing -> do
      challenge <- runDB $ get404 challengeId
      let repoId = challengePublicRepo challenge
      repo <- runDB $ get404 repoId
      repoDir <- getRepoDir repoId
      let repoCloningSpec = RepoCloningSpec {
        cloningSpecRepo = repoSpec,
        cloningSpecReferenceRepo = RepoSpec {
                repoSpecUrl = (T.pack repoDir),
                repoSpecBranch = (repoBranch repo),
                repoSpecGitAnnexRemote = Nothing
                }
        }
      cloneRepo' userId repoCloningSpec chan

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

challengeSubmissionWidget formWidget formEnctype challenge = $(widgetFile "challenge-submission")

submissionForm :: Maybe Text -> Maybe Text -> Maybe Text -> Form (Maybe Text, Maybe Text, Text, Text, Maybe Text)
submissionForm defaultUrl defBranch defaultGitAnnexRemote = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> aopt textField (fieldWithTooltip MsgSubmissionDescription MsgSubmissionDescriptionTooltip) Nothing
    <*> aopt textField (tagsfs MsgSubmissionTags) Nothing
    <*> areq textField (bfs MsgSubmissionUrl) defaultUrl
    <*> areq textField (bfs MsgSubmissionBranch) defBranch
    <*> aopt textField (bfs MsgSubmissionGitAnnexRemote) (Just defaultGitAnnexRemote)

getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR name = do
  userId <- requireAuthId
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) name

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR name = getChallengeSubmissions (\_ -> True) name

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition name = do
  Entity challengeId challenge <- runDB $ getBy404 $ UniqueName name
  (evaluationMaps, tests) <- getChallengeSubmissionInfos condition challengeId
  mauth <- maybeAuth
  let muserId = (\(Entity uid _) -> uid) <$> mauth

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  let params = getNumericalParams evaluationMaps

  challengeLayout True challenge (challengeAllSubmissionsWidget muserId
                                                                challenge
                                                                scheme
                                                                challengeRepo
                                                                evaluationMaps
                                                                tests
                                                                params)

getNumericalParams :: [TableEntry] -> [Text]
getNumericalParams entries = filter (isNumericalParam entries) $ getAllParams entries

isNumericalParam :: [TableEntry] -> Text -> Bool
isNumericalParam entries param =
  all doesTextRepresentNumber
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
  $(widgetFile "challenge-all-submissions")

paramGraphsWidget :: Challenge -> [Entity Test] -> [Text] -> WidgetFor App ()
paramGraphsWidget challenge tests params = $(widgetFile "param-graphs")
  where chartJSs = getChartJss challenge selectedTests params
        selectedTests = getMainTests tests

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


challengeLayout :: Bool -> Challenge -> WidgetFor App () -> HandlerFor App Html
challengeLayout withHeader challenge widget = do
  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON
  maybeUser <- maybeAuth
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
