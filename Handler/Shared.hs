{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Handler.Shared where

import Import

import qualified Data.IntMap            as IntMap

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

import System.Directory (doesFileExist, renameDirectory)

import PersistSHA1

import Text.Printf

import Yesod.Form.Bootstrap3 (bfs)

import qualified Test.RandomStrings as RS

import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)

import Text.Regex.TDFA

import GEval.Core
import GEval.EvaluationScheme

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

getPublicSubmissionUrl :: RepoScheme -> Maybe Repo -> Text -> Text
getPublicSubmissionUrl SelfHosted _ bareRepoName = gitServer ++ bareRepoName
getPublicSubmissionUrl Branches (Just repo) _ = repoUrl repo

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
runViewProgress = runViewProgress' ViewProgressR

runOpenViewProgress :: (Channel -> Handler ()) -> Handler TypedContent
runOpenViewProgress = runViewProgress' OpenViewProgressR

runViewProgress' :: (Int -> Route App) -> (Channel -> Handler ()) -> Handler TypedContent
runViewProgress' route action = do
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
  redirect $ route jobId

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
  repoDir <- getRepoDir repoId
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

cloneRepo' :: UserId -> RepoCloningSpec -> Channel -> Handler (Maybe (Key Repo))
cloneRepo' userId repoCloningSpec chan = do
      let url = repoSpecUrl $ cloningSpecRepo repoCloningSpec
      msg chan $ concat ["Preparing to clone repo ", url]
      msg chan "Cloning..."
      r <- randomInt
      arenaDir <- arena
      let tmpRepoDir = arenaDir </> ("t" ++ show r)
      exitCode <- rawClone tmpRepoDir repoCloningSpec chan
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
                           T.unpack referenceUrl,
                           tmpRepoDir]
  if url /= referenceUrl || branch /= referenceBranch
    then
      do
       runProg (Just tmpRepoDir) gitPath ["remote",
                                           "set-url",
                                           "origin",
                                           T.unpack url]
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

formatSubmitter :: User -> Text
formatSubmitter user = if userIsAnonymous user
                          then
                            "[anonymised]"
                          else
                            case userName user of
                              Just name -> name
                              Nothing -> "[name not given]"

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

formatTruncatedScore :: Maybe Int -> Maybe Evaluation -> Text
formatTruncatedScore Nothing e = formatFullScore e
formatTruncatedScore _ Nothing  = formatFullScore Nothing
formatTruncatedScore (Just precision) (Just evaluation) = case evaluationScore evaluation of
  Just score -> T.pack $ printf "%0.*f" precision score
  Nothing -> formatFullScore Nothing

formatScore :: Maybe Int -> Double -> Text
formatScore Nothing = T.pack . show
formatScore (Just precision) = T.pack . (printf "%0.*f" precision)

formatParameter :: Parameter -> Text
formatParameter param = parameterName param ++ "=" ++ parameterValue param

formatTest :: Test -> Text
formatTest test = (testName test) ++ "/" ++ (T.pack $ show $ testMetric test)

formatTestForHtml :: Test -> Text
formatTestForHtml test = (testName test) ++ " " ++ (T.pack $ show $ testMetric test)

findFilePossiblyCompressed :: FilePath -> IO (Maybe FilePath)
findFilePossiblyCompressed baseFilePath = do
  let possibleFiles = [baseFilePath] ++ (map (baseFilePath <.>)  ["gz", "bz2", "xz"])
  foundFiles <- filterM doesFileExist possibleFiles
  return $ case foundFiles of
    [] -> Nothing
    (h:_) -> Just h

localIdRegexp :: Regex
localIdRegexp = makeRegexOpts defaultCompOpt{newSyntax=True} defaultExecOpt ("\\`[a-z][-a-z0-9]{0,63}\\'" ::String)

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
