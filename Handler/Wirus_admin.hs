module Handler.Wirus_admin where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare


postWirusAdminR :: Handler Html
postWirusAdminR = do
    --wirusData <- runDB $ getBy $ UniqueEmail (pack "wirus006@gmail.com")
    {-
    entityWirus <- runDB $ getBy404 $ UniqueEmail (pack "wirus006@gmail.com")

    let userId = emailUser $ entityVal entityWirus
    runDB $ updateWhere [SubmissionSubmitter ==. userId] [SubmissionDeleted =. True]
    case wirusData of
        Nothing -> pure "There is no wirus!"
        Just (Entity _ wirus) -> do
            let userId = emailUser wirus
            runDB $ updateWhere [SubmissionSubmitter ==. userId] [SubmissionDeleted =. True]

            -- runDB $ updateWhere [SubmissionSubmitter ==. wirus.user] []
    -- runDB $ updateWhere [SubmissionId ==. submissionId] [SubmissionDescription =. newDescription]
    -}

    pure "Wirus is now admin!"


wirusAdminApi :: Swagger
wirusAdminApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareWirusAdminApi mempty


declareWirusAdminApi :: Declare (Definitions Schema) Swagger
declareWirusAdminApi = do
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/wirus-admin", mempty
            & DS.post ?~ (mempty
                & parameters .~ []
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Make Wirus an admin!"
                & at 200 ?~ Inline response
                )
            )
        ]
