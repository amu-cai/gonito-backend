{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Handler.Shared where

import Import

import qualified Data.IntMap            as IntMap

import Yesod.WebSockets

import Handler.Runner
import System.Exit

import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE

import Database.Persist.Sql (fromSqlKey)

import Control.Concurrent.Lifted (threadDelay)
import Control.Concurrent (forkIO)

import qualified Crypto.Hash.SHA1 as CHS

import qualified Data.List as DL

import System.Random

import System.Directory (doesFileExist, renameDirectory, doesDirectoryExist)

import PersistSHA1

import Text.Printf

import Yesod.Form.Bootstrap3 (bfs)

import qualified Test.RandomStrings as RS

import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)

import Text.Regex.TDFA

import GEval.Core
import GEval.Common
import GEval.EvaluationScheme
import GEval.Formatting (formatTheResultWithErrorBounds)

import qualified Data.Vector as DV

import Network.HTTP.Req as R

arena :: Handler FilePath
arena = do
  app <- getYesod
  return $ (appVarDir $ appSettings app) </> "arena"

gitPath :: FilePath
gitPath = "/usr/bin/git"

browsableGitSite :: Text
browsableGitSite = "https://gonito.net/gitlist/"

serverAddress :: Text
serverAddress = "gonito.net"

gitServer :: Text
gitServer = "ssh://gitolite@" ++ serverAddress ++ "/"

gitReadOnlyServer :: Text
gitReadOnlyServer = "git://" ++ serverAddress ++ "/"


getPublicSubmissionBranch :: SubmissionId -> Text
getPublicSubmissionBranch = T.pack . (printf "submission-%05d") . fromSqlKey

getPublicSubmissionUrl :: RepoScheme -> Text -> Maybe Repo -> Text -> Text
getPublicSubmissionUrl SelfHosted repoHost _ bareRepoName = repoHost ++ bareRepoName
getPublicSubmissionUrl Branches _ (Just repo) _ = repoUrl repo

getReadOnlySubmissionUrl :: RepoScheme -> Repo -> Text -> Text
getReadOnlySubmissionUrl SelfHosted _ bareRepoName = gitReadOnlyServer ++ bareRepoName
getReadOnlySubmissionUrl Branches repo _ = repoUrl repo

browsableGitRepoBranch :: RepoScheme -> Repo -> Text -> Text -> Text
browsableGitRepoBranch SelfHosted _ bareRepoName branch = (browsableGitRepo bareRepoName) ++ "/" ++ branch ++ "/"
browsableGitRepoBranch Branches repo _ branch = sshToHttps (repoUrl repo) branch

sshToHttps :: Text -> Text -> Text
sshToHttps url branch = "https://" ++ (T.replace ".git" "" $ T.replace ":" "/" $ T.replace "ssh://" "" $ T.replace "git@" "" url) ++ "/tree/" ++ branch

browsableGitRepo :: Text -> Text
browsableGitRepo bareRepoName
  | ".git" `isSuffixOf` bareRepoName = browsableGitSite ++ bareRepoName
  | otherwise = browsableGitSite ++ bareRepoName ++ ".git"


runViewProgress :: (Channel -> Handler ()) -> Handler TypedContent
runViewProgress action = do
  app <- getYesod
  let viewingProgressStyle = appViewingProgressStyle $ appSettings app
  runViewProgress' (case viewingProgressStyle of
                       WithWebSockets -> ViewProgressWithWebSocketsR
                       WithPlainText -> ViewProgressR)
                   action

runOpenViewProgress :: (Channel -> Handler ()) -> Handler TypedContent
runOpenViewProgress = runViewProgress' OpenViewProgressR

runViewProgressWithWebSockets :: (Channel -> Handler ()) -> Handler TypedContent
runViewProgressWithWebSockets = runViewProgress' ViewProgressWithWebSocketsR

consoleApp :: Int -> WebSocketsT Handler ()
consoleApp jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atom $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> do
          sendTextData ("CANNOT FIND THE OUTPUT (ALREADY SHOWN??)" :: Text)
          sendCloseE ("" :: Text)
          return ()
        Just chan -> do
             let loop = do
                     mtext <- liftIO $ atom $ readTChan chan
                     case mtext of
                         Nothing -> sendCloseE ("" :: Text)
                         Just text -> do
                             sendTextData text
                             loop
             loop
             return ()


getViewProgressWithWebSocketsR :: Int -> Handler Html
getViewProgressWithWebSocketsR jobId = do
    webSockets $ consoleApp jobId
    defaultLayout $ do
        [whamlet|
           <div #outwindow>
             <div #output>
             <div #wait>
                ... PLEASE WAIT ...
        |]
        toWidget [lucius|
            #outwindow {
                border: 1px solid black;
                margin-bottom: 1em;
                color: white;
                background-color: black;
                padding: 10pt;
            }
            #outwindow pre {
                color: white;
                background-color: black;
            }
            #wait {
               animation: blink 1s linear infinite;
            }
            @keyframes blink {
            0% {
               opacity: 0;
            }
            50% {
               opacity: .5;
            }
            100% {
               opacity: 1;
            }
         }
        |]
        toWidget [julius|
            var url = document.URL,
                output = document.getElementById("output"),
                wait = document.getElementById("wait"),
                conn;

            var anchor_name_regex = /\#.*$/;
            url = url.replace("http:", "ws:").replace("https:", "wss:").replace(anchor_name_regex, "")


            conn = new WebSocket(url);

            conn.onmessage = function(e) {
                var p = document.createElement("pre");
                p.appendChild(document.createTextNode(e.data));
                output.appendChild(p);
            };

            conn.onclose = function(e) {
                wait.parentNode.removeChild(wait);
            };
        |]


runViewProgressAsynchronously :: (Channel -> Handler ()) -> Handler Value
runViewProgressAsynchronously action = runViewProgressGeneralized getJobIdAsJson action
--  where getJobIdAsJson jobId = return $ Number (scientific (toInteger jobId) 0)
  where getJobIdAsJson jobId = return $ String $ pack $ show jobId

runViewProgress' :: (Int -> Route App) -> (Channel -> Handler ()) -> Handler TypedContent
runViewProgress' route action = runViewProgressGeneralized doRedirection action
  where doRedirection jobId = redirect $ route jobId

runViewProgressGeneralized :: (Int -> Handler v) -> (Channel -> Handler ()) -> Handler v
runViewProgressGeneralized handler action = do
  App {..} <- getYesod
  jobId <- randomInt
  chan <- liftIO $ atom $ do
    chan <- newBroadcastTChan
    m <- readTVar jobs
    writeTVar jobs $ IntMap.insert jobId chan m
    return chan
  runInnerHandler <- handlerToIO
  _ <- liftIO $ forkIO $ runInnerHandler $ do
    liftIO $ threadDelay 1000000
    action chan
    liftIO $ atom $ do
      writeTChan chan $ Just "All done\n"
      writeTChan chan Nothing
      m <- readTVar jobs
      writeTVar jobs $ IntMap.delete jobId m
  handler jobId

data RepoCloningSpec = RepoCloningSpec {
  cloningSpecRepo :: RepoSpec,
  cloningSpecReferenceRepo :: RepoSpec
}

data RepoSpec = RepoSpec {
  repoSpecUrl :: Text,
  repoSpecBranch :: Text,
  repoSpecGitAnnexRemote :: Maybe Text
}

cloneRepo :: RepoCloningSpec -> Channel -> Handler (Maybe (Key Repo))
cloneRepo repoCloningSpec chan = do
  let url = repoSpecUrl $ cloningSpecRepo repoCloningSpec
  let branch = repoSpecBranch $ cloningSpecRepo repoCloningSpec
  maybeRepo <- runDB $ getBy $ UniqueUrlBranch url branch
  userId <- requireAuthId
  case maybeRepo of
    Just _ -> do
      err chan "Repo already there"
      return Nothing
    Nothing -> cloneRepo' userId repoCloningSpec chan

updateRepo :: Key Repo -> Channel -> Handler Bool
updateRepo repoId chan = do
  repo <- runDB $ get404 repoId
  repoDir <- getRepoDirOrClone repoId chan
  let branch = repoBranch repo
  exitCode <- runWithChannel chan $ do
     runProg (Just repoDir) gitPath ["fetch",
                                      "origin",
                                      T.unpack branch,
                                      "--progress"]
     runProg (Just repoDir) gitPath ["reset",
                                      "--hard",
                                      "FETCH_HEAD"]
     getStuffUsingGitAnnex repoDir (repoGitAnnexRemote repo)
  case exitCode of
    ExitSuccess -> do
      maybeHeadCommit <- getHeadCommit repoDir chan
      case maybeHeadCommit of
          Just headCommit -> do
            runDB $ update repoId [RepoCurrentCommit =. headCommit]
            return True
          Nothing -> return False
    _ -> return False

-- | Get a directionary with a submission.
-- It may reset a git repository which might be risky if a repository
-- is shared among a number of submissions.
getSubmissionRepoDir :: SubmissionId -> Channel -> Handler (Maybe FilePath)
getSubmissionRepoDir submissionId chan = do
  submission <- runDB $ get404 submissionId
  repoDir <- getRepoDirOrClone (submissionRepo submission) chan
  let sha1Code = submissionCommit submission
  -- this is not right... it should be fixed in the future
  -- 1. All kinds of mayhem may ensue in case of concurrency
  -- 2. ... especially if the repository is shared among a number of submissions
  -- 3. The commit might not be actually there (it might have been garbage collected).
  (exitCode, _) <- runProgram (Just repoDir) gitPath ["reset", "--hard", T.unpack $ fromSHA1ToText sha1Code] chan
  case exitCode of
    ExitSuccess -> return (Just repoDir)
    ExitFailure _ -> return Nothing

justGetSubmissionRepoDir :: SubmissionId -> Handler (Maybe FilePath)
justGetSubmissionRepoDir submissionId = do
  devNullChan <- liftIO newTChanIO
  getSubmissionRepoDir submissionId devNullChan

getHeadCommit :: FilePath -> Channel -> Handler (Maybe SHA1)
getHeadCommit repoDir chan = do
  (exitCode, out) <- runProgram (Just repoDir) gitPath ["rev-parse", "HEAD"] chan
  case exitCode of
    ExitSuccess -> do
      msg chan $ concat ["HEAD commit is ", commitId]
      return $ Just commitRaw
        where commitId = T.replace "\n" "" out
              commitRaw = fromTextToSHA1 commitId
    ExitFailure _ -> do
      err chan "cannot determine HEAD commit"
      return Nothing

getPossiblyExistingRepo :: (Key Challenge -> Key Repo -> Channel -> Handler Bool)
                          -> UserId -> Key Challenge -> RepoSpec -> Channel -> Handler (Maybe (Key Repo))
getPossiblyExistingRepo checkRepo userId challengeId repoSpec chan = do
  let url = repoSpecUrl repoSpec
  let branch = repoSpecBranch repoSpec
  let gitAnnexRemote = repoSpecGitAnnexRemote repoSpec
  maybeRepo <- runDB $ getBy $ UniqueUrlBranch url branch
  case maybeRepo of
    Just (Entity repoId _) -> do
      msg chan "Repo already there"
      available <- checkRepo challengeId repoId chan
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
      repoDir <- getRepoDirOrClone repoId chan
      let repoCloningSpec = RepoCloningSpec {
        cloningSpecRepo = repoSpec,
        cloningSpecReferenceRepo = RepoSpec {
                repoSpecUrl = (T.pack repoDir),
                repoSpecBranch = (repoBranch repo),
                repoSpecGitAnnexRemote = Nothing
                }
        }
      cloneRepo' userId repoCloningSpec chan

cloneRepoToTempDir :: RepoCloningSpec -> Channel -> Handler (ExitCode, FilePath)
cloneRepoToTempDir repoCloningSpec chan = do
  let url = repoSpecUrl $ cloningSpecRepo repoCloningSpec
  msg chan $ concat ["Preparing to clone repo ", url]
  msg chan "Cloning..."
  r <- randomInt
  arenaDir <- arena
  let tmpRepoDir = arenaDir </> ("t" ++ show r)
  exitCode <- rawClone tmpRepoDir repoCloningSpec chan
  return (exitCode, tmpRepoDir)

cloneRepo' :: UserId -> RepoCloningSpec -> Channel -> Handler (Maybe (Key Repo))
cloneRepo' userId repoCloningSpec chan = do
  let url = repoSpecUrl $ cloningSpecRepo repoCloningSpec
  (exitCode, tmpRepoDir) <- cloneRepoToTempDir repoCloningSpec chan
  case exitCode of
          ExitSuccess -> do
            maybeHeadCommit <- getHeadCommit tmpRepoDir chan
            case maybeHeadCommit of
              Just commitRaw -> do
                time <- liftIO getCurrentTime
                repoId <- runDB $ insert $ Repo {
                  repoUrl=url,
                  repoBranch=repoSpecBranch $ cloningSpecRepo repoCloningSpec,
                  repoGitAnnexRemote=repoSpecGitAnnexRemote $ cloningSpecRepo repoCloningSpec,
                  repoCurrentCommit=commitRaw,
                  repoOwner=userId,
                  repoReady=True,
                  repoStamp=time }
                repoDir <- getRepoDir repoId
                liftIO $ renameDirectory tmpRepoDir repoDir
                msg chan $ concat ["Repo is in ", (T.pack repoDir)]
                return $ Just repoId
              Nothing -> do
                return Nothing
          ExitFailure _ -> do
            err chan "git failed"
            return Nothing

-- An auxilliary function for fixing git URLs.
-- By default, this does nothing, but can be changed
-- in Gonito forks.
-- Should be used just before a raw git command is executed
-- (i.e. its changes will not be reflected in the database).
fixGitRepoUrl :: Text -> Text
fixGitRepoUrl = id

rawClone :: FilePath -> RepoCloningSpec -> Channel -> Handler ExitCode
rawClone tmpRepoDir repoCloningSpec chan = runWithChannel chan $ do
  let url = repoSpecUrl $ cloningSpecRepo repoCloningSpec
  let branch = repoSpecBranch $ cloningSpecRepo repoCloningSpec
  let referenceUrl = repoSpecUrl $ cloningSpecReferenceRepo repoCloningSpec
  let referenceBranch = repoSpecBranch $ cloningSpecReferenceRepo repoCloningSpec
  runProg Nothing gitPath ["clone",
                           "--progress",
                           "--single-branch",
                           "--branch",
                           T.unpack referenceBranch,
                           T.unpack (fixGitRepoUrl referenceUrl),
                           tmpRepoDir]
  if url /= referenceUrl || branch /= referenceBranch
    then
      do
       runProg (Just tmpRepoDir) gitPath ["remote",
                                           "set-url",
                                           "origin",
                                           T.unpack (fixGitRepoUrl url)]
       runProg (Just tmpRepoDir) gitPath ["fetch",
                                           "origin",
                                           T.unpack branch]
       runProg (Just tmpRepoDir) gitPath ["reset",
                                           "--hard",
                                           "FETCH_HEAD"]
       getStuffUsingGitAnnex tmpRepoDir (repoSpecGitAnnexRemote $ cloningSpecRepo repoCloningSpec)
    else
      return ()

getStuffUsingGitAnnex :: FilePath -> Maybe Text -> Runner ()
getStuffUsingGitAnnex _ Nothing = return ()
getStuffUsingGitAnnex tmpRepoDir (Just gitAnnexRemote) = do
  let randomRemoteNameLen = 10
  remoteName <- liftIO $ RS.randomString (RS.onlyAlpha RS.randomASCII) randomRemoteNameLen
  runGitAnnex tmpRepoDir ["init"]
  runGitAnnex tmpRepoDir (["initremote", remoteName] ++ (words $ T.unpack gitAnnexRemote))
  runGitAnnex tmpRepoDir ["get", "--from", remoteName]

runGitAnnex :: FilePath -> [String] -> Runner ()
runGitAnnex tmpRepoDir args = runProg (Just tmpRepoDir) gitPath ("annex":args)

doesRepoExistsOnTheDisk :: RepoId -> Handler Bool
doesRepoExistsOnTheDisk repoId = do
  repoDir <- getRepoDir repoId
  repoDirExists <- liftIO $ doesDirectoryExist repoDir
  return repoDirExists

-- Gets a directory for an already cloned repo (e.g. arena/r1234). If,
-- for some reason, the directory does not exist (e.g. the database
-- was recovered on a new computer), it will re-clone the repository.
getRepoDirOrClone :: RepoId -> Channel -> Handler FilePath
getRepoDirOrClone repoId chan = do
  repoDirExists <- doesRepoExistsOnTheDisk repoId
  repoDir <- getRepoDir repoId
  if repoDirExists
    then
      return ()
    else
     do
      repo <- runDB $ get404 repoId
      let repoSpec = RepoSpec {
        repoSpecUrl = repoUrl repo,
        repoSpecBranch = repoBranch repo,
        repoSpecGitAnnexRemote = repoGitAnnexRemote repo }
      let repoCloningSpec = RepoCloningSpec {
        cloningSpecRepo = repoSpec,
        cloningSpecReferenceRepo = repoSpec }
      (exitCode, tmpRepoDir) <- cloneRepoToTempDir repoCloningSpec chan
      case exitCode of
        ExitSuccess -> do
          let commitHash = fromSHA1ToText $ repoCurrentCommit repo
          (exitCode', _) <- runProgram (Just tmpRepoDir) gitPath ["reset",
                                                                 "--hard",
                                                                 T.unpack commitHash] chan
          case exitCode' of
            ExitSuccess -> do
              liftIO $ renameDirectory tmpRepoDir repoDir
              return ()
            ExitFailure _ -> do
              err chan $ "cannot reset to commit" ++ commitHash
              return ()
        ExitFailure _ -> do
          err chan "git failed"
          return ()
  return repoDir

getRepoDir :: Key Repo -> Handler FilePath
getRepoDir repoId = do
  arenaDir <- arena
  return $ arenaDir </> ("r" ++ repoIdAsString)
    where repoIdAsString = show $ fromSqlKey repoId

getOpenViewProgressR :: Int -> Handler TypedContent
getOpenViewProgressR = getViewProgressR

getViewProgressR :: Int -> Handler TypedContent
getViewProgressR jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atom $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> respondSource typePlain $ do
            let loop = do
                    mtext <- liftIO $ atom $ readTChan chan
                    case mtext of
                        Nothing -> return ()
                        Just text -> do
                            sendChunkText text
                            sendFlush
                            loop
            loop


randomInt :: Handler Int
randomInt = liftIO $ randomIO

gatherSHA1ForCollectionOfFiles :: [FilePath] -> IO ByteString
gatherSHA1ForCollectionOfFiles files = do
  contentss <- mapM readFile $ sort files
  return $ CHS.finalize $ foldl' CHS.update CHS.init contentss

anonymizedLabel :: Text
anonymizedLabel = "[anonymized]"

nameNotGivenLabel :: Text
nameNotGivenLabel = "[name not given]"

formatSubmitter :: User -> Text
formatSubmitter user = if userIsAnonymous user
                          then
                            anonymizedLabel
                          else
                            case userName user of
                              Just name -> name
                              Nothing -> nameNotGivenLabel

fieldWithTooltip :: forall master msg msg1. (RenderMessage master msg, RenderMessage master msg1) => msg -> msg1 -> FieldSettings master
fieldWithTooltip name tooltip = (bfs name) { fsTooltip = Just $ SomeMessage tooltip }

nonceGen :: Nonce.Generator
nonceGen = unsafePerformIO Nonce.new
{-# NOINLINE nonceGen #-}

-- | Randomly create a new verification key.
newToken :: MonadIO m => m Text
newToken = Nonce.nonce128urlT nonceGen

enableTriggerToken :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, YesodPersist site, PersistStoreWrite (YesodPersistBackend site)) => Key User -> Maybe a -> HandlerFor site ()
enableTriggerToken _ (Just _) = return ()
enableTriggerToken userId Nothing = do
  token <- newToken
  runDB $ update userId [UserTriggerToken =. Just token]

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1

fetchMainTest :: (MonadIO m, PersistQueryRead backend, BaseBackend backend ~ SqlBackend) => Key Challenge -> ReaderT backend m (Entity Test)
fetchMainTest challengeId = do
  challenge <- get404 challengeId

  activeTests <- selectList [TestChallenge ==. challengeId,
                            TestActive ==. True,
                            TestCommit ==. challengeVersion challenge] []

  return $ getMainTest activeTests


fetchTestByName :: (MonadIO m, PersistQueryRead backend, BaseBackend backend ~ SqlBackend) => Maybe Text -> Key Challenge -> ReaderT backend m (Maybe (Entity Test))
fetchTestByName Nothing challengeId = do
  mainTest <- fetchMainTest challengeId
  return $ Just mainTest
fetchTestByName (Just tName) challengeId = do
  challenge <- get404 challengeId

  tests' <- selectList [TestChallenge ==. challengeId,
                       TestCommit ==. challengeVersion challenge] []

  let tests = sortBy (flip testComparator) tests'

  return $ find (\t -> formatTestEvaluationScheme (entityVal t) == tName) tests


-- get the test with the highest priority
getMainTest :: [Entity Test] -> Entity Test
getMainTest tests = DL.maximumBy testComparator tests

-- get all the non-dev tests starting with the one with the highest priorty
-- (or all the tests if there are no non-dev tests)
getMainTests :: [Entity Test] -> [Entity Test]
getMainTests tests = sortBy testComparator tests'
   where tests' = if null tests''
                    then tests
                    else tests''
         tests'' = filter (not . ("dev-" `isPrefixOf`) . testName . entityVal) tests

testComparator :: Entity Test -> Entity Test -> Ordering
testComparator (Entity _ a) (Entity _ b) =
  ((testName a) `compare` (testName b))
                `thenCmp`
  ((fromMaybe unknownPriority $ testPriority b) `compare` (fromMaybe unknownPriority $ testPriority a))
  where unknownPriority = 9999

formatNonScientifically :: Double -> Text
formatNonScientifically = T.pack . (printf "%f")

formatFullScore :: Maybe Evaluation -> Text
formatFullScore (Just evaluation) = fromMaybe "???" (formatNonScientifically <$> evaluationScore evaluation)
formatFullScore Nothing = "N/A"

formatTruncatedScore :: FormattingOptions -> Maybe Evaluation -> Text
formatTruncatedScore _ Nothing  = formatFullScore Nothing
formatTruncatedScore formattingOpts (Just evaluation) = case evaluationScore evaluation of
  Just score -> T.pack $ formatTheResultWithErrorBounds formattingOpts score (evaluationErrorBound evaluation)
  Nothing -> formatFullScore Nothing


getTestFormattingOpts :: Test -> FormattingOptions
getTestFormattingOpts test =
  FormattingOptions {
     decimalPlaces = testPrecision test,
     asPercentage = fromMaybe False $ testAsPercentage test
  }

formatScore :: Maybe Int -> Double -> Text
formatScore Nothing = T.pack . show
formatScore (Just precision) = T.pack . (printf "%0.*f" precision)

formatParameter :: Parameter -> Text
formatParameter param = parameterName param ++ "=" ++ parameterValue param

formatTestEvaluationScheme :: Test -> Text
formatTestEvaluationScheme = T.pack . evaluationSchemeName . testMetric

formatTest :: Test -> Text
formatTest test = (testName test) ++ "/" ++ (formatTestEvaluationScheme test)

formatTestForHtml :: Test -> Text
formatTestForHtml test = (testName test) ++ " " ++ (formatTestEvaluationScheme test)

findFilePossiblyCompressed :: FilePath -> IO (Maybe FilePath)
findFilePossiblyCompressed baseFilePath = do
  let possibleFiles = [baseFilePath] ++ (map (baseFilePath <.>)  ["gz", "bz2", "xz"])
  foundFiles <- filterM doesFileExist possibleFiles
  return $ case foundFiles of
    [] -> Nothing
    (h:_) -> Just h

localIdRegexp :: Regex
localIdRegexp = makeRegexOpts defaultCompOpt{newSyntax=True} defaultExecOpt ("\\`[a-z0-9][-a-z0-9]{0,63}\\'" ::String)

unwantedLocalIds :: [Text]
unwantedLocalIds = ["git",
                    "gitolite",
                    "admin",
                    "root",
                    "filipg"]

isLocalIdAcceptable :: Text -> Bool
isLocalIdAcceptable localId =
  match localIdRegexp (unpack localId) && not (localId `elem` unwantedLocalIds)

-- need to transfer the information into a JS script
getIsHigherTheBetterArray :: [Test] -> Value
getIsHigherTheBetterArray = Array
                            . DV.fromList
                            . map (convertIsHigherTheBetter
                                   . getMetricOrdering
                                   . evaluationSchemeMetric
                                   . testMetric)
   where convertIsHigherTheBetter TheHigherTheBetter = Bool True
         convertIsHigherTheBetter _ = Bool False

compareFun :: MetricOrdering -> Double -> Double -> Ordering
compareFun TheLowerTheBetter = flip compare
compareFun TheHigherTheBetter = compare

runSlackHook :: Text -> Text -> IO ()
runSlackHook hook message = do
  let (Just (hookUrl, _)) = parseUrlHttps $ DTE.encodeUtf8 hook

  R.runReq def $ do
    let payload = object [ "text" .= message ]
    (_ :: IgnoreResponse) <- R.req R.POST
                                 hookUrl
                                 (R.ReqBodyJson payload)
                                 R.ignoreResponse
                                 mempty
    return ()

slackLink :: App -> Text -> Text -> Text
slackLink app title addr = "<" ++ slink ++ "|" ++ title ++ ">"
  where slink = (appRoot $ appSettings app) ++ "/" ++ addr

formatVersion :: (Int, Int, Int) -> Text
formatVersion (major, minor, patch) = (T.pack $ show major)
                                      <> "." <> (T.pack $ show minor)
                                      <> "." <> (T.pack $ show patch)


checkWhetherGivenUserRepo :: (PersistStoreRead backend, MonadIO m, BaseBackend backend ~ SqlBackend)
                            => Key User -> Key Submission -> ReaderT backend m Bool
checkWhetherGivenUserRepo userId submissionId = do
  submission <- get404 submissionId
  return $ userId == submissionSubmitter submission

fetchTheEvaluation :: (MonadIO m, PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
                     => Out -> SHA1 -> ReaderT backend m (Maybe (Entity Evaluation))
fetchTheEvaluation out version =
  getBy $ UniqueEvaluationTestChecksumVersion (outTest out) (outChecksum out) version
