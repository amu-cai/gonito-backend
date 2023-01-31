{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import
import Handler.Shared
import Handler.Evaluate
import Handler.SubmissionView
import Handler.TagUtils
import Handler.JWT

import Prelude (read)

import Data.Diff

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import qualified Data.Map as Map

import Data.Text (pack, unpack, unwords, take)

import PersistSHA1

import qualified Data.List as DL

import GEval.Core
import GEval.EvaluationScheme

import GEval.ParseParams (parseParamsFromFilePath, OutputFileParsed(..))

import Data.Swagger hiding (get)
import Data.Proxy as DPR
import Control.Lens hiding ((.=), (^.))
import Data.HashMap.Strict.InsOrd (fromList)

import qualified Data.Set as S

data TestReference = TestReference Text Text
                     deriving (Show, Eq, Ord)

instance ToJSON TestReference where
    toJSON (TestReference metric n) = object
        [ "name" .= n,
          "metric" .= (Data.Text.pack $ evaluationSchemeName $ read $ Data.Text.unpack metric)
        ]

instance ToSchema TestReference where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    return $ NamedSchema (Just "TestReference") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           Data.HashMap.Strict.InsOrd.fromList [  ("name", stringSchema)
                                               , ("metric", stringSchema)
                                               ]
        & required .~ [ "name", "metric" ]

getTestReference :: Entity Test -> TestReference
getTestReference (Entity _ test) = TestReference (Data.Text.pack $ show $ testMetric test) (testName test)

data LeaderboardEntry = LeaderboardEntry {
  leaderboardUser :: User,
  leaderboardUserId :: UserId,
  leaderboardBestSubmission :: Submission,
  leaderboardBestSubmissionId :: SubmissionId,
  leaderboardBestVariant :: Variant,
  leaderboardBestVariantId :: VariantId,
  leaderboardEvaluationMap :: Map TestReference Evaluation,
  leaderboardNumberOfSubmissions :: Int,
  leaderboardTags :: [(Entity Import.Tag, Entity SubmissionTag)],
  leaderboardParams :: [Parameter],
  leaderboardVersion :: ((Int, Int, Int), (Maybe Import.Tag)),
  leaderboardIsOwner :: Bool,
  leaderboardIsVisible :: Bool,
  leaderboardIsReevaluable :: Bool,
  leaderboardTeam :: Maybe (Entity Team)
}

-- | Finds parameters shared by all entries (including values) and removes
-- them from the entries
extractCommonParams :: [TableEntry] -> ([Entity Parameter], [TableEntry])
extractCommonParams [] = ([], [])
extractCommonParams entries@(firstEntry:_) = (commonParams, map removeParams entries)
  where commonParams = filter (\p -> paramToNameVal p `S.member` commonNameVals) $ tableEntryParams firstEntry
        commonNameVals =
          DL.foldl intersection hS tS
        (hS:tS) = map (S.fromList
                         . map paramToNameVal
                         . tableEntryParams) entries
        paramToNameVal (Entity _ p) = (parameterName p, parameterValue p)
        removeParams e
          = e { tableEntryParams = filter (\p -> paramToNameVal p `S.notMember` commonNameVals) $ tableEntryParams e }

data TableEntry = TableEntry {
  tableEntrySubmission :: Entity Submission,
  tableEntryVariant :: Entity Variant,
  tableEntrySubmitter :: Entity User,
  tableEntryMapping :: Map TestReference Evaluation,
  tableEntryTagsInfo :: [(Entity Import.Tag, Entity SubmissionTag)],
  tableEntryParams :: [Entity Parameter],
  tableEntryRank :: Int,
  tableEntryVersion :: ((Int, Int, Int), Maybe Import.Tag),
  tableEntryTeam :: Maybe (Entity Team) }

tableEntryStamp :: TableEntry -> UTCTime
tableEntryStamp = submissionStamp . entityVal . tableEntrySubmission

formatSubmittingEntity :: TableEntry -> Text
formatSubmittingEntity tableEntry =
  case tableEntryTeam tableEntry of
    Just teamEnt -> teamIdent $ entityVal teamEnt
    Nothing -> formatSubmitter $ entityVal $ tableEntrySubmitter tableEntry

submissionsTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> DisclosedInfo -> [Entity Test] -> Table App TableEntry
submissionsTable mauthId challengeName repoScheme challengeRepo disclosedInfo tests = mempty
  ++ Table.int "#" tableEntryRank
  ++ Table.text "submitter" formatSubmittingEntity
  ++ timestampCell "when" tableEntryStamp
  ++ versionCell tableEntryVersion
  ++ descriptionCell mauthId
  ++ mconcat (applyDisclosedInfoOnLast disclosedInfo (\d e@(Entity _ t) -> resultCell (extractScore $ getTestReference e) d t) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\tableEntry -> (entityKey $ tableEntrySubmission tableEntry,
                                                                       entityVal $ tableEntrySubmission tableEntry,
                                                                       entityKey $ tableEntryVariant tableEntry,
                                                                       entityVal $ tableEntryVariant tableEntry,
                                                                       mauthId))

variantTable :: [Text] -> DisclosedInfo -> [Entity Test] -> Table App TableEntry
variantTable paramNames disclosedInfo tests = mempty
  ++ Table.int "#" tableEntryRank
  ++ mconcat (map paramExtractor paramNames)
  ++ mconcat (applyDisclosedInfoOnLast disclosedInfo (\d e@(Entity _ t) -> resultCell (extractScore $ getTestReference e) d t) tests)
  ++ Table.widget "" variantStatusCellWidget

variantStatusCellWidget :: TableEntry -> WidgetFor App ()
variantStatusCellWidget entry = $(widgetFile "variant-status")
  where theVariantId = entityKey $ tableEntryVariant entry

paramExtractor :: Text -> Table App TableEntry
paramExtractor paramName = Table.text paramName (\entry ->
                                                   fromMaybe ""
                                                   $ listToMaybe
                                                   $ map parameterValue
                                                   $ filter (\p -> parameterName p == paramName)
                                                   $ map entityVal
                                                   $ tableEntryParams entry)

descriptionCell :: Maybe UserId -> Table App TableEntry
descriptionCell mauthId = Table.widget "description" (
  \(TableEntry (Entity _ s) (Entity _ v) (Entity _ _) _ tagEnts paramEnts _ _ _) -> fragmentWithSubmissionTags
                                                                                   (descriptionToBeShown s v (map entityVal paramEnts))
                                                                                   (getInfoLink s mauthId)
                                                                                   tagEnts)

formatListWithLimit :: Int -> (a -> Text) -> [a] -> Text
formatListWithLimit limit fun l = (Data.Text.unwords $ map fun $ Import.take limit l) <>
                                     (if length l <= limit
                                      then ""
                                      else " [...]")


descriptionToBeShown :: Submission -> Variant -> [Parameter] -> Text
descriptionToBeShown s v params = (submissionDescription s) ++ (Data.Text.pack vdescription) ++ " " ++ paramsShown
  where (OutputFileParsed r _) = parseParamsFromFilePath (Data.Text.unpack $ variantName v)
        vdescription = if r == "out"
                         then
                           ""
                         else
                           " " ++ r
        maximumNumberOfParamsShown = 8
        paramsShown = formatListWithLimit maximumNumberOfParamsShown formatParameter params

extractScore :: TestReference -> TableEntry -> Maybe Evaluation
extractScore k tableEntry = lookup k $ tableEntryMapping tableEntry

formatSubmittingEntityInLeaderboard :: LeaderboardEntry -> Text
formatSubmittingEntityInLeaderboard entry =
  case leaderboardTeam entry of
    Just teamEnt -> teamIdent $ entityVal teamEnt
    Nothing -> formatSubmitter $ leaderboardUser entry

versionCell :: (a -> ((Int, Int, Int), (Maybe Import.Tag))) -> Table site a
versionCell fun = Table.widget "ver." (
  \e -> fragmentWithTag (formatVersion $ fst $ fun e) (snd $ fun e))

leaderboardTable :: DisclosedInfo -> Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App (Int, LeaderboardEntry)
leaderboardTable disclosedInfo mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmittingEntityInLeaderboard . snd)
  ++ timestampCell "when" (submissionStamp . leaderboardBestSubmission . snd)
  ++ versionCell (leaderboardVersion . snd)
  ++ leaderboardDescriptionCell mauthId
  ++ mconcat (applyDisclosedInfoOnLast disclosedInfo (\d e@(Entity _ t) -> resultCell (extractScoreFromLeaderboardEntry (getTestReference e) . snd) d t) tests)
  ++ Table.int "×" (leaderboardNumberOfSubmissions . snd)
  ++ statusCell challengeName repoScheme challengeRepo (\(_, e) -> (leaderboardBestSubmissionId e,
                                                                   leaderboardBestSubmission e,
                                                                   leaderboardBestVariantId e,
                                                                   leaderboardBestVariant e,
                                                                   mauthId))

altLeaderboardTable :: DisclosedInfo -> Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App (Int, LeaderboardEntry)
altLeaderboardTable disclosedInfo mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" fst
  ++ leaderboardOnlyTagsCell mauthId
  ++ mconcat (applyDisclosedInfoOnLast disclosedInfo (\d e@(Entity _ t) -> resultCell (extractScoreFromLeaderboardEntry (getTestReference e) . snd) d t) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\(_, e) -> (leaderboardBestSubmissionId e,
                                                                   leaderboardBestSubmission e,
                                                                   leaderboardBestVariantId e,
                                                                   leaderboardBestVariant e,
                                                                   mauthId))


extractScoreFromLeaderboardEntry :: TestReference -> LeaderboardEntry -> Maybe Evaluation
extractScoreFromLeaderboardEntry k entry = lookup k (leaderboardEvaluationMap entry)

leaderboardDescriptionCell :: Maybe UserId -> Table App (a, LeaderboardEntry)
leaderboardDescriptionCell mauthId = Table.widget "description" (
  \(_,entry) -> fragmentWithSubmissionTags (descriptionToBeShown (leaderboardBestSubmission entry)
                                                                (leaderboardBestVariant entry)
                                                                (leaderboardParams entry))
                                          (getInfoLink (leaderboardBestSubmission entry)
                                                       mauthId)
                                          (leaderboardTags entry)
  )

leaderboardOnlyTagsCell :: Maybe UserId -> Table App (a, LeaderboardEntry)
leaderboardOnlyTagsCell mauthId = Table.widget "tags" (
  \(_,entry) -> fragmentWithSubmissionTags ("" :: Text)
                                          (getInfoLink (leaderboardBestSubmission entry)
                                                       mauthId)
                                          (leaderboardTags entry)
  )


hoverTextCell :: Text -> (a -> Text) -> (a -> Text) -> Table site a
hoverTextCell h mainTextFun hoverTextFun = Table.widget h (
  \v -> [whamlet|<span title="#{hoverTextFun v}">#{mainTextFun v}|])

timestampCell :: Text -> (a -> UTCTime) -> Table site a
timestampCell h timestampFun = hoverTextCell h (Data.Text.pack . shorterFormat . timestampFun) (Data.Text.pack . show . timestampFun)
   where shorterFormat = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

statusCell :: Text -> RepoScheme -> Repo -> (a -> (SubmissionId, Submission, VariantId, Variant, Maybe UserId)) -> Table App a
statusCell challengeName repoScheme challengeRepo fun = Table.widget "" (statusCellWidget challengeName repoScheme challengeRepo . fun)

resultCell :: (a -> Maybe Evaluation) -> DisclosedInfo -> Test -> Table App a
resultCell fun disclosedInfo test = hoverTextCell (formatTestForHtml test) (formatTruncatedScore disclosedInfo formattingOpts . fun) (formatFullScore disclosedInfo . fun)
  where formattingOpts = getTestFormattingOpts test

textLimited :: Int -> Text -> Text
textLimited limit t
  | l < limit = t
  | otherwise = (Data.Text.take limit t) <> "…"
  where l = length t

textCellSoftLimit :: Int
textCellSoftLimit = 140

textCellHardLimit :: Int
textCellHardLimit = 5 * textCellSoftLimit

limitedWidget :: Int -> Int -> Text -> WidgetFor site ()
limitedWidget softLimit hardLimit v =
  [whamlet|<span title="#{textLimited hardLimit v}"><tt>#{textLimited softLimit v}</tt>|]

limitedTextCell :: Text -> Int -> Int -> (a -> Text) -> Table site a
limitedTextCell h softLimit hardLimit textFun = Table.widget h (
  \v -> limitedWidget softLimit hardLimit (textFun v))

theLimitedTextCell :: Text -> (a -> Text) -> Table site a
theLimitedTextCell h textFun = limitedTextCell h textCellSoftLimit textCellHardLimit textFun

theLimitedDiffTextCell :: Text -> (a -> Diff Text) -> Table site a
theLimitedDiffTextCell h textFun = Table.widget h (
  \v -> case textFun v of
         OneThing u -> limitedWidget textCellSoftLimit textCellHardLimit u
         d@(TwoThings _ _) -> [whamlet|#{d}|])

extractInt :: [PersistValue] -> Int64
extractInt ((PersistInt64 x):_) = x

statusCellWidget :: Text -> RepoScheme -> Repo -> (SubmissionId, Submission, VariantId, Variant, Maybe UserId) -> WidgetFor App ()
statusCellWidget challengeName repoScheme challengeRepo (submissionId, submission, variantId, _, mauthId) = do
  isReevaluable <- handlerToWidget $ runDB $ canBeReevaluated submissionId
  let isVisible = True

  app <- getYesod
  let repoHost = appRepoHost $ appSettings app

  let maybeBrowsableUrl = if isPublic
                                then
                                  Just $ browsableGitRepoBranch repoScheme repoHost challengeRepo challengeName publicSubmissionBranch
                                else
                                  Nothing

  $(widgetFile "submission-status")
    where commitHash = fromSHA1ToText $ submissionCommit submission
          isPublic = submissionIsPublic submission
          isOwner = (mauthId == Just (submissionSubmitter submission))
          publicSubmissionBranch = getPublicSubmissionBranch submissionId

getInfoLink :: Submission -> Maybe UserId -> Maybe (Route App)
getInfoLink submission _ = Just $ QueryResultsR commitHash
   where commitHash = fromSHA1ToText $ submissionCommit submission

checkWhetherVisible :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend)
                      => Submission -> Maybe (Key User) -> ReaderT backend m Bool
checkWhetherVisible submission Nothing = return $ submissionIsPublic submission
checkWhetherVisible submission (Just seerId) = do
  let challengeId = submissionChallenge submission
  achvs <- E.select $ E.from $ \(achievement, course, participant, teacher) -> do
     E.where_ (achievement ^. AchievementChallenge E.==. E.val challengeId
               E.&&. achievement ^. AchievementCourse E.==. course ^. CourseId
               E.&&. participant ^. ParticipantUser E.==. E.val userId
               E.&&. participant ^. ParticipantCourse E.==. course ^. CourseId
               E.&&. teacher ^. TeacherUser E.==. E.val seerId
               E.&&. teacher ^. TeacherCourse E.==. course ^. CourseId)
     E.limit 2
     return ()
  let isTeacher = case achvs of
        [] -> False
        _  -> True
  return (isPublic || isOwner || isTeacher)
  where isPublic = submissionIsPublic submission
        isOwner = (seerId == userId)
        userId = submissionSubmitter submission

getAuxSubmissionEnts :: TestReference -> [TableEntry] -> [(Key User, (User, [(Entity Submission, Entity Variant, Evaluation)]))]
getAuxSubmissionEnts testId evaluationMaps = map processEvaluationMap evaluationMaps
   where processEvaluationMap (TableEntry s v (Entity ui u) m _ _ _ _ _) = (ui, (u, case Map.lookup testId m of
                                                                                     Just e -> [(s, v, e)]
                                                                                     Nothing -> []))


compareMajorVersions ::  ((Int, Int, Int), Maybe Import.Tag) -> ((Int, Int, Int), Maybe Import.Tag) -> Ordering
compareMajorVersions ((aM, _, _),_) ((bM, _, _), _) = aM `compare` bM

compareVersions :: ((Int, Int, Int), Maybe Import.Tag) -> ((Int, Int, Int), Maybe Import.Tag) -> Ordering
compareVersions ((aM, aN, aP), _) ((bM, bN, bP), _) = (aM `compare` bM)
                                            <> (aN `compare` bN)
                                            <> (aP `compare` bP)

getLeaderboardEntriesByCriterion :: (Ord a) => Int
                                 -> Key Challenge
                                 -> ((Entity Submission) -> Bool)
                                 -> ([(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))])
                                 -> (TableEntry -> [a])
                                 -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntriesByCriterion maxPriority challengeId condition preselector selector = do
  (evaluationMaps, tests) <- runDB $ getChallengeSubmissionInfos maxPriority condition (const True) preselector challengeId
  let mainTests = getMainTests tests
  let mMainTestEnt = getMainTest tests
  case mMainTestEnt of
    Nothing -> return ([], ([], []))
    Just mainTestEnt -> do
      let mainTestRef = getTestReference mainTestEnt
      let (Entity _ mainTest) = mainTestEnt
      let auxItems = concat
                     $ map (\i -> map (\s -> (s, [i])) (selector i))
                     $ filter (\entry -> member mainTestRef $ tableEntryMapping entry)
                     $ evaluationMaps
      let auxItemsMap = Map.fromListWith (++) auxItems
      let entryComparator a b =
            (compareMajorVersions (leaderboardVersion a) (leaderboardVersion b))
            <>
            ((compareResult $ Just mainTest) (evaluationScore $ leaderboardEvaluationMap a Map.! mainTestRef)
             (evaluationScore $ leaderboardEvaluationMap b Map.! mainTestRef))
            <>
            (compareVersions (leaderboardVersion a) (leaderboardVersion b))
      entries' <- mapM (toLeaderboardEntry challengeId mainTests)
                 $ filter (\ll -> not (null ll))
                 $ map snd
                 $ Map.toList auxItemsMap
      let entries = DL.nubBy (\a b -> leaderboardBestVariantId a == leaderboardBestVariantId b)
                    $ sortBy (flip entryComparator) entries'
      return (entries, (evaluationMaps, mainTests))


toLeaderboardEntry :: Foldable t => Key Challenge -> [Entity Test] -> t TableEntry -> Handler LeaderboardEntry
toLeaderboardEntry challengeId tests ss = do
  let bestOne = DL.maximumBy submissionComparator ss
  let (TableEntry bestSubmission bestVariant user evals _ _ _ _ _) = bestOne
  let submissionId = entityKey bestSubmission
  tagEnts <- runDB $ getTags submissionId

  theParameters <- runDB $ selectList [ParameterVariant ==. (entityKey bestVariant)] [Asc ParameterName]

  submission <- runDB $ get404 submissionId
  (Just (Entity _ itsVersion)) <- runDB $ getBy $ UniqueVersionByCommit $ submissionVersion submission

  mPhaseTag <- case versionPhase itsVersion of
                   Just phaseId -> runDB $ get phaseId
                   Nothing -> return Nothing

  let theVersion = (versionMajor itsVersion,
                    versionMinor itsVersion,
                    versionPatch itsVersion)

  -- get all user submissions, including hidden ones
  allUserSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId,
                                           SubmissionSubmitter ==. entityKey user]
                                          [Desc SubmissionStamp]

  mUserEnt <- maybeAuthPossiblyByToken
  let isOwner = (entityKey <$> mUserEnt) == Just (submissionSubmitter submission)

  isReevaluable <- runDB $ canBeReevaluated $ entityKey $ tableEntrySubmission bestOne
  let isVisible = True

  mTeam <- case submissionTeam $ entityVal bestSubmission of
            Just teamId -> do
              team <- runDB $ get404 teamId
              return $ Just (Entity teamId team)
            Nothing -> return Nothing

  return $ LeaderboardEntry {
              leaderboardUser = entityVal user,
              leaderboardUserId = entityKey user,
              leaderboardBestSubmission = entityVal bestSubmission,
              leaderboardBestSubmissionId = entityKey bestSubmission,
              leaderboardBestVariant = entityVal bestVariant,
              leaderboardBestVariantId = entityKey bestVariant,
              leaderboardEvaluationMap = evals,
              leaderboardNumberOfSubmissions = length allUserSubmissions,
              leaderboardTags = tagEnts,
              leaderboardParams = map entityVal theParameters,
              leaderboardVersion = (theVersion, mPhaseTag),
              leaderboardIsOwner = isOwner,
              leaderboardIsReevaluable = isReevaluable,
              leaderboardIsVisible = isVisible,
              leaderboardTeam = mTeam
              }
     where submissionComparator (TableEntry _  _  _ em1 _ _ _ v1 _) (TableEntry _  _ _ em2 _ _ _ v2 _) =
             case getMainTest tests of
               Just mainTestEnt@(Entity _ mainTest) ->
                 let mainTestRef = getTestReference mainTestEnt
                 in (compareMajorVersions v1 v2)
                    <>
                    (compareResult (Just $ mainTest) (evaluationScore (em1 Map.! mainTestRef))
                                                     (evaluationScore (em2 Map.! mainTestRef)))
                    <>
                    (compareVersions v1 v2)
               Nothing -> EQ

getLeaderboardEntries :: Int -> LeaderboardStyle -> Key Challenge -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntries maxPriority BySubmitter challengeId =
  getLeaderboardEntriesByCriterion maxPriority
                                   challengeId
                                   (const True)
                                   onlyTheBestVariant
                                   (\entry -> [(entityKey $ tableEntrySubmitter entry,
                                               tagName <$> (snd $ tableEntryVersion entry))])
getLeaderboardEntries maxPriority ByTag challengeId =
  getLeaderboardEntriesByCriterion maxPriority
                                   challengeId
                                   (const True)
                                   onlyTheBestVariant
                                   (noEmptyList . (map (entityKey . fst)) . tableEntryTagsInfo)
  where noEmptyList [] = [Nothing]
        noEmptyList l = map Just l

compareResult :: Maybe Test -> Maybe Double -> Maybe Double -> Ordering
compareResult Nothing _ _ = EQ
compareResult (Just test) (Just x) (Just y) = (compareFun $ getMetricOrdering $ evaluationSchemeMetric $ testMetric test) x y
compareResult _ (Just _) Nothing = GT
compareResult _ Nothing (Just _) = LT
compareResult _ Nothing Nothing = EQ

onlyTheBestVariant :: [(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))]
onlyTheBestVariant = DL.nubBy (\(_, (Entity aid _, _)) (_, (Entity bid _, _)) -> aid == bid)
                     . (sortBy (\(r1, (_, Entity _ va)) (r2, (_, Entity _ vb)) -> (r1 `compare` r2)
                                                                                     `thenCmp`
                                                                    ((variantName va) `compare` (variantName vb))))


getChallengeSubmissionInfos :: (MonadIO m,
                               PersistQueryRead backend,
                               BackendCompatible SqlBackend backend,
                               PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
                              => Int
                                -> (Entity Submission -> Bool)
                                -> (Entity Variant -> Bool)
                                -> ([(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))])
                                -> Key Challenge
                                -> ReaderT backend m ([TableEntry], [Entity Test])
getChallengeSubmissionInfos maxMetricPriority condition variantCondition preselector challengeId = do
  challenge <- get404 challengeId
  let versionCommit = challengeVersion challenge
  getChallengeSubmissionInfosForVersion maxMetricPriority
                                        condition
                                        variantCondition
                                        preselector
                                        challengeId
                                        versionCommit

getChallengeSubmissionInfosForVersion :: (MonadIO m,
                                         PersistQueryRead backend,
                                         BackendCompatible SqlBackend backend,
                                         PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
                                      => Int
                                      -> (Entity Submission -> Bool)
                                      -> (Entity Variant -> Bool)
                                      -> ([(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))])
                                      -> ChallengeId
                                      -> SHA1
                                      -> ReaderT backend m ([TableEntry], [Entity Test])
getChallengeSubmissionInfosForVersion maxMetricPriority condition variantCondition preselector challengeId commit = do

  tests' <- selectList [TestChallenge ==. challengeId, TestActive ==. True, TestCommit ==. commit] []
  let tests = filter (\t -> (evaluationSchemePriority $ testMetric $ entityVal t) <= maxMetricPriority) tests'
  let mainTest = getMainTest tests

  allSubmissionsVariants <- E.select $ E.from $ \(submission, variant) -> do
     E.where_ (submission ^. SubmissionChallenge E.==. E.val challengeId
               E.&&. submission ^. SubmissionIsHidden E.==. E.val False
               E.&&. variant ^. VariantSubmission E.==. submission ^. SubmissionId)
     return (submission, variant)

  scores <- mapM (getScore (entityKey <$> mainTest)) $ map (entityKey . snd) allSubmissionsVariants

  let allSubmissionsVariantsWithRanks =
        sortBy (\(r1, (s1, _)) (r2, (s2, _)) -> (submissionStamp (entityVal s2) `compare` submissionStamp (entityVal s1))
                                                                               `thenCmp`
                                                                            (r2 `compare` r1))
        $ preselector
        $ filter (\(_, (s, _)) -> condition s)
        $ map (\(rank, (_, sv)) -> (rank, sv))
        $ zip [1..]
        $ sortBy (\(s1, _) (s2, _) -> compareResult (entityVal <$> mainTest) s2 s1)
        $ zip scores allSubmissionsVariants

  allTests <- selectList [] [Asc TestName]
  let testsMap = Map.fromList $ map (\(ent@(Entity testId _)) -> (testId, getTestReference ent)) allTests

  let allSubmissions = DL.nubBy (\(Entity a _) (Entity b _) -> a == b) $ map (\(_, (s, _)) -> s) allSubmissionsVariantsWithRanks
  subs <- mapM getBasicSubmissionInfo allSubmissions
  let submissionMap = Map.fromList subs
  -- testsMap and submissionMap are created to speed up getEvaluationMap

  evaluationMaps' <- mapM (getEvaluationMap testsMap submissionMap) allSubmissionsVariantsWithRanks
  let evaluationMaps = filter (variantCondition . tableEntryVariant) evaluationMaps'
  return (evaluationMaps, tests)

getScore :: (MonadIO m, BackendCompatible SqlBackend backend,
            PersistQueryRead backend, PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
           => Maybe (Key Test) -> Key Variant -> ReaderT backend m (Maybe Double)
getScore Nothing _ = return Nothing
getScore (Just testId) variantId = do
  evaluations <- E.select $ E.from $ \(out, evaluation, variant, submission) -> do
                  E.where_ (out ^. OutVariant E.==. E.val variantId
                            E.&&. variant ^. VariantId E.==. E.val variantId
                            E.&&. submission ^. SubmissionId E.==. variant ^. VariantSubmission
                            E.&&. out ^. OutTest E.==. E.val testId
                            E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum
                            -- all this complication here and with orderBy due
                            -- to the legacy issue with evaluation version sometimes missing
                            E.&&. (evaluation ^. EvaluationVersion E.==. submission ^. SubmissionVersion)
                            E.&&. evaluation ^. EvaluationTest E.==. E.val testId)
                  E.orderBy [E.asc (evaluation ^. EvaluationScore)]
                  return evaluation
  return $ case evaluations of
             (e:_) -> evaluationScore $ entityVal e
             [] -> Nothing


data BasicSubmissionInfo = BasicSubmissionInfo {
  basicSubmissionInfoUser :: User,
  basicSubmissionInfoTagEnts :: [(Entity Import.Tag, Entity SubmissionTag)],
  basicSubmissionInfoVersion :: (Version, Maybe Import.Tag),
  basicSubmissionInfoTeam :: Maybe (Entity Team) }

getBasicSubmissionInfo :: (MonadIO m, PersistQueryRead backend,
                          PersistUniqueRead backend,
                          BaseBackend backend ~ SqlBackend)
                         => Entity Submission -> ReaderT backend m (SubmissionId, BasicSubmissionInfo)
getBasicSubmissionInfo (Entity submissionId submission) = do
  user <- get404 $ submissionSubmitter submission
  mTeam <- case submissionTeam submission of
            Just teamId -> do
              team <- get404 teamId
              return $ Just (Entity teamId team)
            Nothing -> return Nothing
  tagEnts <- getTags submissionId
  let versionHash = submissionVersion submission
  (Entity _ ver) <- getBy404 $ UniqueVersionByCommit versionHash

  mPhaseTag <- case versionPhase ver of
                   Just phaseId -> get phaseId
                   Nothing -> return Nothing

  return $ (submissionId, BasicSubmissionInfo {
               basicSubmissionInfoUser = user,
               basicSubmissionInfoTagEnts = tagEnts,
               basicSubmissionInfoVersion = (ver, mPhaseTag),
               basicSubmissionInfoTeam = mTeam })

getEvaluationMap :: (PersistUniqueRead backend,
                    PersistQueryRead backend,
                    BackendCompatible SqlBackend backend,
                    MonadIO m,
                    BaseBackend backend ~ SqlBackend)
                   => Map (Key Test) TestReference
                     -> Map (Key Submission) BasicSubmissionInfo
                     -> (Int, (Entity Submission, Entity Variant))
                     -> ReaderT backend m TableEntry
getEvaluationMap testsMap submissionsMap (rank, (s@(Entity submissionId submission), v@(Entity variantId _))) = do
  let submissionInfo = submissionsMap Map.! submissionId
  let user = basicSubmissionInfoUser submissionInfo
  let tagEnts = basicSubmissionInfoTagEnts submissionInfo
  let theVersion = fst $ basicSubmissionInfoVersion submissionInfo
  let mPhase = snd $ basicSubmissionInfoVersion submissionInfo
  let versionHash = submissionVersion submission
  let team = basicSubmissionInfoTeam submissionInfo

  evaluations <- E.select $ E.from $ \(evaluation, out) ->
    do
     E.where_ (out ^. OutVariant E.==. E.val variantId
               E.&&. evaluation ^. EvaluationTest E.==. out ^. OutTest
               E.&&. evaluation ^. EvaluationChecksum E.==. out ^. OutChecksum
               E.&&. evaluation ^. EvaluationVersion E.==. E.val versionHash)
     E.orderBy [E.asc (out ^. OutId)]
     return evaluation

  let pairs = map (\(Entity _ e) -> (evaluationTest e, e)) evaluations
  let pairs' = map (\(testId, e) -> (testsMap Map.! testId, e)) pairs
  let m = Map.fromList pairs'

  params <- selectList [ParameterVariant ==. variantId] [Asc ParameterName]

  let major = versionMajor theVersion
  let minor = versionMinor theVersion
  let pat = versionPatch theVersion

  return $ TableEntry s v (Entity (submissionSubmitter submission) user) m tagEnts params rank ((major, minor, pat), mPhase) team
