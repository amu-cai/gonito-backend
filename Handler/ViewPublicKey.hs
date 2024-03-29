module Handler.ViewPublicKey where

import           Import                     hiding
    ( Proxy
    , encodeUtf8
    , fromList
    , pack
    )

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
    -- Entity userId _ <- requireAuthPossiblyByToken
    maybeWiurs <- runDB $ getBy $ UniqueUser "wirus006@gmail.com"
    case maybeWiurs of
        Nothing -> return $ pack "There is no wirus"
        Just (Entity userId _) -> do
            pubKeys <- runDB $ selectList [PublicKeyUser ==. userId] []
            return $ pack $ show $ map keyFromEntity pubKeys

    -- pubKeys <- runDB $ selectList [PublicKeyUser ==. userId] []

    --return $ pack $ show $ map keyFromEntity pubKeys

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
            & DS.get ?~ (mempty
                & parameters .~ []
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Show logged user's public key."
                & at 200 ?~ Inline response
                )
            )
        ]
