{-# LANGUAGE ScopedTypeVariables  #-}

module Handler.Evaluate where

import Import

import Handler.Common
import Handler.Runner
import Handler.Shared

import Gonito.ExtractMetadata (ExtractionOptions(..),
                               extractMetadataFromRepoDir,
                               GonitoMetadata(..))

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import GEval.Core
import GEval.Common
import GEval.EvaluationScheme
import GEval.OptionsParser
import GEval.ParseParams (parseParamsFromFilePath, OutputFileParsed(..))
import GEval.Common (GEvalException, MetricResult(..), MetricValue)
import GEval.Formatting (formatTheResult)

import Options.Applicative
import Data.Conduit.SmartSource
import Data.Conduit.Bootstrap (defaultConfidenceLevel, getConfidenceBounds)

import System.FilePath (takeFileName, dropExtensions, (-<.>))

import PersistSHA1

canBeReevaluated :: (YesodAuthPersist (HandlerSite m),
                    MonadHandler m,
                    PersistUniqueRead backend,
                    AuthEntity (HandlerSite m) ~ User,
                    AuthId (HandlerSite m) ~ Key User,
                    BaseBackend backend ~ SqlBackend)
                   => Key Submission -> ReaderT backend m Bool
canBeReevaluated submissionId = do
  maybeUser <- maybeAuth
  case maybeUser of
    Just (Entity userId _) -> do
      isOwner <- checkWhetherGivenUserRepo userId submissionId
      let isSuperuser = checkIfAdmin maybeUser

      submission <- get404 submissionId
      let submissionVersionHash = submissionVersion submission

      challenge <- get404 $ submissionChallenge submission
      let challengeVersionHash = challengeVersion challenge

      if (submissionVersionHash == challengeVersionHash)
       then return False
       else
        do
         (Entity _ submissionVer) <- getBy404 $ UniqueVersionByCommit submissionVersionHash
         (Entity _ chalengeVer) <- getBy404 $ UniqueVersionByCommit challengeVersionHash

         return  ((isOwner || isSuperuser)
                  &&
                  ((versionMajor submissionVer) == (versionMajor chalengeVer)
                   && (versionMinor submissionVer) == (versionMinor chalengeVer)
                   && (versionPatch submissionVer) < (versionPatch chalengeVer)))


    Nothing -> return False

getReevaluateSubmissionR :: SubmissionId -> Handler TypedContent
getReevaluateSubmissionR submissionId =
  runViewProgress $ doReevaluateSubmission submissionId

doReevaluateSubmission :: SubmissionId -> Channel -> Handler ()
doReevaluateSubmission submissionId chan = do
  status <- runDB $ canBeReevaluated submissionId
  if status
    then
     do
      mRepoDir <- getSubmissionRepoDir submissionId chan
      case mRepoDir of
        Just repoDir -> do
          -- not exactly right, as the parameters might have been changed manuall
          gonitoMetadata <- liftIO
                           $ extractMetadataFromRepoDir repoDir (ExtractionOptions {
                                                                    extractionOptionsDescription = Nothing,
                                                                    extractionOptionsTags = Nothing,
                                                                    extractionOptionsGeneralParams = Nothing,
                                                                    extractionOptionsUnwantedParams = Nothing,
                                                                    extractionOptionsParamFiles = Nothing,
                                                                    extractionOptionsMLRunPath = Nothing,
                                                                    extractionOptionsExternalLinks = Nothing,
                                                                    extractionOptionsDependencies = Nothing })

          submission <- runDB $ get404 submissionId
          let previousVersion = submissionVersion submission
          challenge <- runDB $ get404 $ submissionChallenge submission
          let currentChallengeVersion = challengeVersion challenge

          runDB $ update submissionId [SubmissionVersion =. currentChallengeVersion]

          catch (getOuts True chan submissionId (gonitoMetadataGeneralParams gonitoMetadata) >> return ()) $ \(_::SomeException) -> do
            err chan "SOMETHING WENT WRONG!!! REVERTING TO THE PREVIOUS VERSION"
            runDB $ update submissionId [SubmissionVersion =. previousVersion]

          return ()

        Nothing -> do
          err chan "Something went wrong, won't evaluate"
    else
      msg chan "Won't re-evaluate!"

-- | Does the evaluation for a submission. Inserts Out, Variant and Evaluation records.
getOuts :: Bool -> Channel -> Key Submission -> M.Map Text Text -> Handler ([Out])
getOuts forceEvaluation chan submissionId generalParams = do
  submission <- runDB $ get404 submissionId
  let challengeId = submissionChallenge submission
  let version = submissionVersion submission
  repoDir <- getRepoDirOrClone (submissionRepo submission) chan
  activeTests <- runDB $ selectList [TestChallenge ==. challengeId,
                                    TestActive ==. True,
                                    TestCommit ==. submissionVersion submission] []

  outs' <- mapM (outsForTest repoDir submissionId generalParams) activeTests
  let outs = concat outs'

  mapM_ checkOrInsertOut outs
  mapM_ (checkOrInsertEvaluation repoDir forceEvaluation chan version) outs
  return outs

outFileName :: FilePath
outFileName = "out.tsv"

getOutFilePath :: FilePath -> Test -> FilePath
getOutFilePath repoDir test = repoDir </> (T.unpack $ testName test) </> outFileName

findOutFile :: FilePath -> Test -> IO (Maybe FilePath)
findOutFile repoDir test = do
  let baseOut = getOutFilePath repoDir test
  ofs <- mapM (\ext -> findFilePossiblyCompressed (baseOut -<.> ext)) extensionsHandled
  return $ listToMaybe $ catMaybes ofs

doesOutExist :: FilePath -> Entity Test -> IO Bool
doesOutExist repoDir (Entity _ test) = do
  result <- findOutFile repoDir test
  return $ isJust result

-- | Returns an Out object (won't insert into a database!)
outForTest :: MonadIO m => FilePath -> FilePath -> Key Variant -> Entity Test -> m Out
outForTest repoDir outF variantId (Entity testId test) = do
  let outPath = repoDir </> (T.unpack $ testName test) </> outF
  checksum <- liftIO $ gatherSHA1ForCollectionOfFiles [outPath]
  return Out {
    outVariant=variantId,
    outTest=testId,
    outChecksum=SHA1 checksum }

-- | Returns all possible outs for a given test.
-- Won't insert Out objects to the database, though it might add new variant objects.
outsForTest :: FilePath -> SubmissionId -> M.Map Text Text -> Entity Test -> HandlerFor App [Out]
outsForTest repoDir submissionId generalParams testEnt@(Entity _ test) = do
  outFiles <- liftIO $ outFilesForTest repoDir test

  forM outFiles $ \outFile -> do
    theVariant <- getVariant submissionId generalParams outFile
    outForTest repoDir outFile theVariant testEnt

-- | Returns the filenames (not file paths) of all output files for a given test.
outFilesForTest :: FilePath -> Test -> IO [FilePath]
outFilesForTest repoDir test = do
    mMultipleOuts <- checkMultipleOutsCore repoDir (T.unpack $ testName test) "out.tsv"
    case mMultipleOuts of
      Just outFiles -> return $ map takeFileName outFiles
      Nothing -> do
        mOutFile <- findOutFile repoDir test
        case mOutFile of
          Just outF -> return [takeFileName outF]
          Nothing -> return []

getVariant :: SubmissionId -> M.Map Text Text -> FilePath -> Handler VariantId
getVariant submissionId generalParams outFilePath = runDB $ do
  let outFile = takeFileName outFilePath
  let name = T.pack $ dropExtensions outFile
  maybeVariant <- getBy $ UniqueVariantSubmissionName submissionId name
  case maybeVariant of
    Just (Entity vid _) -> return vid
    Nothing -> do
      vid <- insert $ Variant submissionId name
      let (OutputFileParsed _ paramMap) = parseParamsFromFilePath outFile

      forM_ (M.toList (paramMap `M.union` generalParams)) $ \(param, val) -> do
        _ <- insert $ Parameter vid param val
        return ()

      return vid

checkOrInsertOut :: Out -> Handler ()
checkOrInsertOut out = do
  maybeOut <- runDB $ getBy $ UniqueOutVariantTestChecksum (outVariant out) (outTest out) (outChecksum out)
  case maybeOut of
    Just _ -> return ()
    Nothing -> (runDB $ insert out) >> return ()

checkOrInsertEvaluation :: FilePath -> Bool -> Channel -> SHA1 -> Out -> Handler ()
checkOrInsertEvaluation repoDir forceEvaluation chan version out = do
  test <- runDB $ get404 $ outTest out
  challenge <- runDB $ get404 $ testChallenge test
  maybeEvaluation <- runDB $ fetchTheEvaluation out version

  disclosedInfo <- fetchDisclosedInfo challenge

  if not forceEvaluation && isJust maybeEvaluation
   then
    do
      let Just (Entity _ evaluation) = maybeEvaluation
      case disclosedInfo of
        DisclosedInfo Nothing -> msg chan $ concat ["Already evaluated with score ", (fromMaybe "???" $ formatNonScientifically <$> evaluationScore evaluation)]
        _ -> return ()
   else
    do
      msg chan $ "Start evaluation..."
      challengeDir <- getRepoDirOrClone (challengePrivateRepo challenge) chan
      variant <- runDB $ get404 $ outVariant out
      resultOrException <- liftIO $ rawEval challengeDir (testMetric test) repoDir (testName test) ((T.unpack $ variantName variant) <.> "tsv")
      case resultOrException of
        Right (Left _) -> do
          err chan "Cannot parse options, check the challenge repo"
        Right (Right (_, Just [(_, [result])])) -> do
          let defaultFormattingOpts = FormattingOptions {
                decimalPlaces = Nothing,
                asPercentage = False }
          case disclosedInfo of
            DisclosedInfo Nothing -> msg chan $ concat [ "Evaluated! Score ", (T.pack $ formatTheResult defaultFormattingOpts result) ]
            _ -> msg chan "Evaluated!"
          time <- liftIO getCurrentTime
          let (pointResult, errorBound) = extractResult result
          if (isJust maybeEvaluation)
           then
            runDB $ updateWhere [
              EvaluationTest ==. outTest out,
              EvaluationChecksum ==. outChecksum out,
              EvaluationVersion ==. version ]
                 [ EvaluationScore =. Just pointResult,
                   EvaluationErrorBound =. errorBound,
                   EvaluationErrorMessage =. Nothing,
                   EvaluationStamp =. time ]
           else
             do
              _ <- runDB $ insert $ Evaluation {
                                   evaluationTest=outTest out,
                                   evaluationChecksum=outChecksum out,
                                   evaluationScore=Just pointResult,
                                   evaluationErrorBound=errorBound,
                                   evaluationErrorMessage=Nothing,
                                   evaluationStamp=time,
                                   evaluationVersion=version }
              return ()
          msg chan "Evaluation done"
        Right (Right (_, Just _)) -> do
          err chan "Unexpected multiple results (???)"
        Right (Right (_, Nothing)) -> do
          err chan "Error during the evaluation"
        Left exception -> do
          err chan $ "Evaluation failed: " ++ (T.pack $ show exception)

extractResult :: MetricResult -> (MetricValue, Maybe MetricValue)
extractResult (SimpleRun r) = (r, Nothing)
extractResult (BootstrapResampling vals) = ((upperBound + lowerBound) / 2.0, Just ((upperBound - lowerBound) / 2.0))
  where (lowerBound, upperBound) = getConfidenceBounds defaultConfidenceLevel vals

rawEval :: FilePath
          -> EvaluationScheme
          -> FilePath
          -> Text
          -> FilePath
          -> IO (Either GEvalException (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe [(SourceSpec, [MetricResult])])))
rawEval challengeDir metric repoDir name outF = Import.try (runGEvalGetOptions [
                                                          "--alt-metric", (show metric),
                                                          "--expected-directory", challengeDir,
                                                          "--out-directory", repoDir,
                                                          "--out-file", outF,
                                                          "--test-name", (T.unpack name)])
