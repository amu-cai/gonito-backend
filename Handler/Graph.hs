module Handler.Graph where

import Import

import Handler.Tables
import Handler.Dashboard (indicatorToEntry, prettyIndicatorEntry, formatTarget, IndicatorEntry(..), TargetStatus(..), filterEntries, getTargetStatus)
import Handler.Shared (formatParameter, formatScore, fetchMainTest, compareFun, fetchDisclosedInfoForTest, DisclosedInfo(..), fetchDisclosedInfo)
import Data.Maybe
import Data.List ((!!))
import Database.Persist.Sql
import GEval.Core (getMetricOrdering)
import GEval.EvaluationScheme
import GEval.Common (MetricValue)
import qualified Data.Map as M
import qualified Data.Text as T

import Data.Aeson (KeyValue)

import Data.SubmissionConditions (parseCondition, checkCondition, VariantEntry(..))

-- graphs for parameters

getChallengeGraphDataR :: Text -> Handler Value
getChallengeGraphDataR challengeName = submissionsToJSON (\_ -> True) challengeName

data ParamGraphItem = ParamGraphItem TableEntry Text Text MetricValue

data ParamGraphSeries = ParamGraphSeries Text [(TableEntry, Text, MetricValue)]

getChallengeParamGraphDataR :: Text -> (Key Test) -> Text -> Handler Value
getChallengeParamGraphDataR challengeName testId paramName = do
  (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName
  test <- runDB $ get404 testId

  disclosedInfo <- fetchDisclosedInfoForTest challengeId (Entity testId test)

  case disclosedInfo of
    DisclosedInfo Nothing ->
      do
          let testRef = getTestReference (Entity testId test)

          (entries, _) <- runDB $ getChallengeSubmissionInfos 1 (const True) (const True) id challengeId

          let values = map (findParamValue paramName) entries

          let items = Data.Maybe.catMaybes $ map (toParamGraphItem testRef paramName) $ zip entries values

          let series = map (\(label, rs) -> ParamGraphSeries label rs)
                       $ organizeBy
                       $ map (\(ParamGraphItem entry label x y) -> (label, (entry, x, y))) items

          return $ object [
            "xs" .= object (map (\(ParamGraphSeries seriesName _) -> ((fromText seriesName) .= (xSeriesName seriesName))) series),
            "columns" .= ((map (toYColumn $ testPrecision test) series) ++ (map toXColumn series))
                          ]

toYColumn :: Maybe Int -> ParamGraphSeries -> [Text]
toYColumn mPrecision (ParamGraphSeries seriesName items) =
  seriesName : (map (\(_,_,v) -> formatScore mPrecision v) items)

toXColumn :: ParamGraphSeries -> [Text]
toXColumn (ParamGraphSeries seriesName items) =
  (xSeriesName seriesName) : (map (\(_,x,_) -> x) items)

xSeriesName :: Text -> Text
xSeriesName = (++ "_x")

organizeBy :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
organizeBy pList = M.toList $ M.fromListWith (++) $ map (\(x, y) -> (x, [y])) pList

toParamGraphItem :: TestReference -> Text -> (TableEntry, Maybe Text) -> Maybe ParamGraphItem
toParamGraphItem _ _ (_, Nothing) = Nothing
toParamGraphItem testRef paramName (entry, Just val) = (ParamGraphItem entry label val) <$> join y
  where label = unwords (tagsFormatted ++ paramsFormatted)
        tagsFormatted =
                map (tagName . entityVal . fst)
                $ tableEntryTagsInfo entry
        paramsFormatted =
                map formatParameter
                $ filter (\pe -> parameterName pe /= paramName)
                $ map entityVal $ tableEntryParams entry
        y = evaluationScore <$> lookup testRef (tableEntryMapping entry)

findParamValue :: Text -> TableEntry -> Maybe Text
findParamValue paramName entry =
  (parameterValue . entityVal) <$> (find (\e -> parameterName (entityVal e) == paramName) $ tableEntryParams entry)

submissionsToJSON :: ((Entity Submission) -> Bool) -> Text -> Handler Value
submissionsToJSON condition challengeName = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName challengeName

  disclosedInfo <- fetchDisclosedInfo challenge

  case disclosedInfo of
    DisclosedInfo Nothing ->
      do
         (entries, _) <- getLeaderboardEntriesByCriterion 1 challengeId
                                                         condition
                                                         onlyTheBestVariant
                                                         (\entry -> [entityKey $ tableEntrySubmission entry])


         mEntMainTest <- runDB $ fetchMainTest challengeId
         case mEntMainTest of
           Just entMainTest -> do
             let mainTestRef = getTestReference entMainTest

             let naturalRange = getNaturalRange mainTestRef entries
             let submissionIds = map leaderboardBestSubmissionId entries

             forks <- runDB $ selectList [ForkSource <-. submissionIds, ForkTarget <-. submissionIds] []

             return $ object [ "nodes" .= (Data.Maybe.catMaybes
                                           $ map (auxSubmissionToNode mainTestRef naturalRange)
                                           $ entries),
                               "edges" .= map forkToEdge forks ]
           Nothing -> do
             return $ object []

getNaturalRange :: TestReference -> [LeaderboardEntry] -> Double
getNaturalRange testRef entries = 2.0 * (interQuantile
                                        $ Data.Maybe.catMaybes
                                        $ map (\entry -> evaluationScore $ ((leaderboardEvaluationMap entry) M.! testRef)) entries)

auxSubmissionToNode :: TestReference -> Double -> LeaderboardEntry -> Maybe Value
auxSubmissionToNode testRef naturalRange entry = case evaluationScore $ ((leaderboardEvaluationMap entry) M.! testRef) of
  Just score ->  Just $ object [
    "id" .= (nodeId $ leaderboardBestSubmissionId entry),
    "x"  .= (stampToX $ submissionStamp $ leaderboardBestSubmission entry),
    "y"  .= (- ((score / naturalRange) * 100.0)),
    "size" .= (2 :: Int),
    "label" .= descriptionToBeShown (leaderboardBestSubmission entry) (leaderboardBestVariant entry) (leaderboardParams entry) ]
  Nothing -> Nothing

forkToEdge :: Entity Fork -> Value
forkToEdge (Entity forkId fork) = object [
  "source" .= nodeId (forkSource fork),
  "target" .= nodeId (forkTarget fork),
  "id"     .= edgeId forkId,
  "type"   .= ["arrow" :: String]
  ]

nodeId :: Key Submission -> String
nodeId = ("n" ++) . show . fromSqlKey

edgeId :: Key Fork -> String
edgeId = ("e" ++) . show . fromSqlKey

stampToX :: UTCTime -> Integer
stampToX = toModifiedJulianDay . utctDay

-- Indicator graph

-- Reduce a list to item which are larger than the largest item encountered so far.
-- (Needed to plot a step graph.)
monotonicBy :: (a -> b) -> (b -> b -> Ordering) -> [a] -> [a]
monotonicBy _ _ [] = []
monotonicBy extractor comparator (theFirst:theRest) = (theFirst : (monotonicBy' theFirst theRest))
  where monotonicBy' _ [] = []
        monotonicBy' theBest (h:t) = if extractor h `comparator` extractor theBest == GT
                                     then
                                       (h:(monotonicBy' h t))
                                     else
                                       monotonicBy' theBest t

targetStatusToClass :: TargetStatus -> String
targetStatusToClass TargetFailed = "target-failed-line"
targetStatusToClass TargetPassed = "target-passed-line"
targetStatusToClass TargetOngoing = "target-ongoing-line"

getIndicatorGraphDataR :: IndicatorId -> Handler Value
getIndicatorGraphDataR indicatorId = do
  indicator <- runDB $ get404 indicatorId
  indicatorEntry <- indicatorToEntry (Entity indicatorId indicator)
  let label = prettyIndicatorEntry indicatorEntry

  let testId = indicatorTest indicator
  test <- runDB $ get404 testId
  let mPrecision = testPrecision test

  (entries, _) <- runDB $ getChallengeSubmissionInfos 1 (const True) (const True) id (testChallenge test)

  theNow <- liftIO $ getCurrentTime -- needed to draw the "now" vertical line

  let targetStatuses = map (getTargetStatus theNow entries indicatorEntry) (indicatorEntryTargets indicatorEntry)

  -- first we apply the "filter condition"
  let filteredEntries =
        filterEntries (indicatorFilterCondition indicator)
        $ sortBy (\a b  -> (tableEntryStamp a) `compare` (tableEntryStamp b)) entries

  -- ... all the entires that passed the "filter condition" are split according
  -- to whether the main "target condition" is fulfilled or not,
  -- "other..." will mean items for which "target condition" is not fulfilled
  -- (but the "filter condition" was), they are also going to be used to draw
  -- an auxilliary graph
  let (targetEntries, otherEntries) =
        partitionEntries (indicatorTargetCondition indicator) filteredEntries
  let (scores', timePoints') = addNow theNow $ entriesToPoints (Entity testId test) targetEntries
  let (otherScores, otherTimePoints) = addNow theNow $ entriesToPoints (Entity testId test) otherEntries
  let otherLabel = label <> " (other filtered)"

  -- grid lines for targets would not be taken into account when determining y range,
  -- that's why we need to enforce y range manually if needed
  -- (x range are not modified this way)
  let targetValues = map (targetValue . entityVal) $ indicatorEntryTargets indicatorEntry
  let maxRange = getBound compare scores' targetValues
  let minRange = getBound (flip compare) scores' targetValues

  -- we need to make sure the deadline line is visible
  let targetsInTheFuture =
        filter (\target -> targetDeadline target > theNow)
        $ map entityVal $ indicatorEntryTargets indicatorEntry
  let scores = scores' ++ (map (const (last $ impureNonNull scores')) targetsInTheFuture)
  let timePoints = timePoints' ++ (map (formatTimestamp . targetDeadline) targetsInTheFuture)

  -- we return a JSON object required by the C3 library
  return $ object [
    "bindto" .=  ("#indicator-chart-" ++ (show $ unSqlBackendKey $ unIndicatorKey indicatorId)),
    "data" .= object [
        "xs" .= object ([
           label .= ("xt" :: String)
           ] ++ (listIf (not $ null otherScores) [otherLabel .= ("xo" :: String)])),
        "columns" .= ([
            ("xt" : timePoints),
            (label : (map (formatScore mPrecision) scores))]
            ++ (listIf (not $ null otherScores) [
                   ("xo" : otherTimePoints),
                   (otherLabel : (map (formatScore mPrecision) otherScores))])),
        "types" .= object [
            label .= ("area-step" :: String),
            otherLabel .= ("step" :: String)
            ]
        ],
    "axis" .= object [
        "x" .= object [
            "type" .= ("timeseries" :: String),
              "tick" .= object [
              "format" .= ("%Y-%m-%d" :: String)
              ]
            ],
        "y" .= object ((getBoundAttr "max" maxRange) ++ (getBoundAttr "min" minRange))
        ],
    "line" .= object [
        "step" .= object [
            "type" .= ("step-after" :: String)
            ]
        ],
    "grid" .= targetsToLines theNow indicatorEntry targetStatuses
    ]

formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- add fake entry for the current time
addNow :: UTCTime -> ([Double], [Text]) -> ([Double], [Text])
addNow _ ([], []) = ([], [])
addNow theNow (scores, timepoints) = (scores ++ [last $ impureNonNull scores], timepoints ++ [formatTimestamp theNow])

entriesToPoints :: Entity Test -> [TableEntry] -> ([Double], [Text])
entriesToPoints testEnt@(Entity _ test) entries = (scores, timePoints)
  where timePoints = map (formatTimestamp . tableEntryStamp) relevantEntries
        scores = map (\entry -> fromJust $ evaluationScore $ (tableEntryMapping entry) M.! testRef) relevantEntries
        relevantEntries =
          monotonicBy (\entry -> fromJust $ evaluationScore $ (tableEntryMapping entry) M.! testRef) comparator
          $ filter (\entry -> testRef `M.member` (tableEntryMapping entry)
                             && isJust (evaluationScore ((tableEntryMapping entry) M.! testRef))) entries
        comparator = compareFun $ getMetricOrdering $ evaluationSchemeMetric $ testMetric test
        testRef = getTestReference testEnt

targetsToLines :: UTCTime -> IndicatorEntry -> [TargetStatus] -> Value
targetsToLines theNow indicator statuses = object [
  "y" .= object [
      "lines" .= map (\(target, status) -> object [
                         "value" .= (targetValue $ entityVal target),
                          "text" .= formatTarget mPrecision target,
                          "class" .= targetStatusToClass status
                         ]) (zip targets statuses)
      ],
  "x" .= object [
      "lines" .= ((map (\(target, status) -> object [
                         "value" .= (formatTimestamp $ targetDeadline $ entityVal target),
                         "text" .= formatTarget mPrecision target,
                         "class" .= targetStatusToClass status
                         ]) $ zip targets statuses)
        ++ [object [
               "value" .= formatTimestamp theNow,
               "text" .= ("now" :: String)
               ]])
      ]
  ]
  where targets = indicatorEntryTargets indicator
        mPrecision = testPrecision $ entityVal $ indicatorEntryTest indicator

getBound :: (a -> a -> Ordering) -> [a] -> [a] -> Maybe a
getBound _ [] _ = Nothing
getBound _ _ [] = Nothing
getBound comparator mainList extraList =
  let mainMax = maximumBy comparator (impureNonNull mainList)
      extraMax = maximumBy comparator (impureNonNull extraList)
  in case extraMax `comparator` mainMax of
       GT -> Just extraMax
       _ -> Nothing

getBoundAttr :: KeyValue p => Text -> Maybe Double -> [p]
getBoundAttr _ Nothing = []
getBoundAttr label (Just s) = [
    label .= s
  ]

listIf :: Bool -> [a] -> [a]
listIf True l = l
listIf False _ = []

partitionEntries :: Maybe Text -> [TableEntry] -> ([TableEntry], [TableEntry])
partitionEntries Nothing entries = (entries, [])
partitionEntries (Just condition) entries = partition (\entry -> checkCondition conditionParsed (toVariantEntry entry)) entries
  where conditionParsed = parseCondition condition
        toVariantEntry :: TableEntry -> VariantEntry
        toVariantEntry entry = VariantEntry {
          variantEntryTags = map (entityVal . fst) $ tableEntryTagsInfo entry,
          variantEntryParams = map entityVal $ tableEntryParams entry
          }

-- auxiliary functions taken from Math.Statistics

interQuantile :: (Fractional b, Ord b) => [b] -> b
interQuantile [] = 10.0
interQuantile xs = (q' - q)
    where q = quantile 0.25 xs
          q' = quantile 0.75 xs

quantile :: (Fractional b, Ord b) => Double -> [b] -> b
quantile q = quantileAsc q . sort

quantileAsc :: (Fractional b, Ord b) => Double -> [b] -> b
quantileAsc _ [] = error "x"
quantileAsc q xs
    | q < 0 || q > 1 = error "quantile out of range"
    | otherwise = xs !! (quantIndex (length xs) q)
    where quantIndex :: Int -> Double -> Int
          quantIndex len q' = case round $ q' * (fromIntegral len - 1) of
                               idx | idx < 0    -> error "Quantile index too small"
                                   | idx >= len -> error "Quantile index too large"
                                   | otherwise  -> idx
