{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Presentation where

import Import

import GEval.MetricsMeta

import Handler.ShowChallenge
import Handler.Tables
import Handler.Shared

import qualified Yesod.Table as Table

import Text.Hamlet (hamletFile)

sampleChallengeName :: Text
sampleChallengeName = "petite-difference-challenge2"

sampleChallengeName' :: Text
sampleChallengeName' = "retroc2"

retrocChallengeName :: Text
retrocChallengeName = "retroc2"

retroc2ChallengeName :: Text
retroc2ChallengeName = "retroc2"

sampleUserIdent :: Text
sampleUserIdent = "ptlen@ceti.pl"

disclosed = DisclosedInfo Nothing

getPresentation4RealR :: Handler Html
getPresentation4RealR = do
  readme <- challengeReadme sampleChallengeName

  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName sampleChallengeName

  (Just (Entity sampleUserId _)) <- runDB $ getBy $ UniqueUser sampleUserIdent
  let condition = (\(Entity _ submission) -> (submissionSubmitter submission == sampleUserId))
  (evaluationMaps', tests) <- runDB $ getChallengeSubmissionInfos 1 condition (const True) onlyTheBestVariant challengeId
  let evaluationMaps = take 10 evaluationMaps'

  sampleLeaderboard <- getSampleLeaderboard sampleChallengeName
  sampleLeaderboard' <- getSampleLeaderboard sampleChallengeName'

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  presentationLayout $(widgetFile "presentation-4real")

getPresentationPSNC2019R :: Handler Html
getPresentationPSNC2019R = do
  readme <- challengeReadme sampleChallengeName

  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName sampleChallengeName

  (Just (Entity sampleUserId _)) <- runDB $ getBy $ UniqueUser sampleUserIdent
  let condition = (\(Entity _ submission) -> (submissionSubmitter submission == sampleUserId))
  (evaluationMaps', tests) <- runDB $ getChallengeSubmissionInfos 1 condition (const True) onlyTheBestVariant challengeId
  let evaluationMaps = take 10 evaluationMaps'

  sampleLeaderboard <- getSampleLeaderboard sampleChallengeName
  sampleLeaderboard' <- getSampleLeaderboard sampleChallengeName'

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  presentationLayout $(widgetFile "presentation-psnc-2019")

getPresentationDATeCH2017R = do
  readme <- challengeReadme retrocChallengeName
  retrocLeaderboard <- getSampleLeaderboard retrocChallengeName
  retroc2Leaderboard <- getSampleLeaderboard retroc2ChallengeName
  presentationLayout $(widgetFile "presentation-datech-2017")

getSampleLeaderboard :: Text -> HandlerFor App (WidgetFor App ())
getSampleLeaderboard name = getSampleLeaderboardGeneralized name 1 BySubmitter (leaderboardTable disclosed)

getSampleAltLeaderboard :: Text -> HandlerFor App (WidgetFor App ())
getSampleAltLeaderboard name = getSampleLeaderboardGeneralized name 2 ByTag (altLeaderboardTable disclosed)

getSampleLeaderboardGeneralized name maxPriority method table = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name

  (leaderboard, (_, tests)) <- getLeaderboardEntries maxPriority method challengeId
  let leaderboardWithRanks = zip [1..] (take 10 leaderboard)

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  return $ Table.buildBootstrap (table Nothing
                                       (challengeName challenge)
                                       scheme challengeRepo tests)
                                leaderboardWithRanks



presentationLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $(hamletFile "templates/presentation-layout.hamlet")

getWritingPapersWithGonitoR :: Handler Html
getWritingPapersWithGonitoR = do
  app <- getYesod
  let tab :: String = "\t"
  let rootAddress = appRoot $ appSettings app
  defaultLayout $ do
    setTitle "Writing papers with Gonito"
    $(widgetFile "writing-papers")
