module Handler.ViewPublicKey where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare
import           Data.Text                  (pack)

import           Handler.JWT


getViewPublicKeyR :: Handler Text
getViewPublicKeyR = do
    Entity userId _ <- requireAuthPossiblyByToken

    pubKeys <- runDB $ selectList [PublicKeyUser ==. userId] []

    return $ Data.Text.pack $ show $ map keyFromEntity pubKeys

    where
        keyFromEntity (Entity tagId _) = tagId


viewPublicKeyApi :: Swagger
viewPublicKeyApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareViewPublicKeyApi mempty


declareViewPublicKeyApi :: Declare (Definitions Schema) Swagger
declareViewPublicKeyApi = do
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/view-public-key", mempty
            & DS.post ?~ (mempty
                & parameters .~ []
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Show user's public key."
                & at 200 ?~ Inline response
                )
            )
        ]
