module Handler.Swagger where

import Import

import Data.Swagger
import Handler.ListChallenges
import Handler.ShowChallenge
import Handler.Query
import Handler.Tags
import Handler.Team
import Handler.YourAccount


import Control.Lens hiding ((.=))

getSwaggerR :: Handler Value
getSwaggerR = return $ toJSON apiDescription

apiDescription :: Swagger
apiDescription = generalApi
                 <> addUserApi
                 <> userInfoApi
                 <> myEvaluationTriggerTokenApi
                 <> listChallengesApi
                 <> leaderboardApi
                 <> allSubmissionsApi
                 <> mySubmissionsApi
                 <> challengeReadmeInMarkdownApi
                 <> queryApi
                 <> challengeSubmissionApi
                 <> makePublicApi
                 <> versionInfoApi
                 <> listTagsApi
                 <> myTeamsApi
                 <> challengeImgApi
                 <> challengeRepoApi
                 <> testProgressApi
                 <> viewProgressWithWebSockets
                 <> viewProgressLog
                 <> currentTimeApi
                 <> formatAsLocalTimeApi

generalApi :: Swagger
generalApi = (mempty :: Swagger)
  & info .~ (mempty &
              title .~ "Gonito API" &
              version .~ "3.18.0")
