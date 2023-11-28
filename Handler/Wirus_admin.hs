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
    runDB $ updateWhere [UserIdent ==. "wirus006@gmail.com"] [UserIsAdmin =. True]

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
