module Handler.JWT where

import Import hiding (Proxy, fromList)

import           Data.Word8 (isSpace, toLower)
import           Network.Wai (requestHeaders)
import qualified Data.ByteString as BS
import qualified Jose.Jwt as JWT
import qualified Jose.Jwa as JWA

import Data.Aeson


data JwtAuthInfo = JwtAuthInfo
    { jwtAuthInfoUsername   :: Text
    , jwtAuthInfoFamilyName :: Maybe Text
    , jwtAuthInfoGivenName  :: Maybe Text
    }

  deriving (Show, Eq)

instance FromJSON JwtAuthInfo where
    parseJSON (Object v) =
        JwtAuthInfo <$> v .: "preferred_username"
                    <*> v .:? "family_name"
                    <*> v .:? "given_name"
    parseJSON _ = mzero


jwtAuthInfoIdent :: JwtAuthInfo -> Text
jwtAuthInfoIdent = jwtAuthInfoUsername


jwtAuthInfoCustomField :: Text -> JwtAuthInfo -> Maybe Text
jwtAuthInfoCustomField "given_name" jwt = jwtAuthInfoGivenName jwt
jwtAuthInfoCustomField "family_name" jwt = jwtAuthInfoFamilyName jwt
jwtAuthInfoCustomField _ _ = Nothing


authorizationTokenAuth :: Handler (Maybe JwtAuthInfo)
authorizationTokenAuth = do
    app <- getYesod
    let mJwk = appJSONWebKey $ appSettings app

    case mJwk of
        Just jwk -> do
            req <- waiRequest

            case lookup "Authorization" (Network.Wai.requestHeaders req) of
                Nothing -> return Nothing
                Just authHead -> case BS.break isSpace authHead of
                    (strategy, token')
                        | BS.map Data.Word8.toLower strategy == "bearer" -> do
                            let token = BS.filter (/= 32) token'
                            einfo <- liftIO $ JWT.decode [jwk] (Just (JWT.JwsEncoding JWA.RS256)) token

                            return $ case einfo of
                                Right (JWT.Jws (_, infos)) -> decode $ fromStrict infos
                                _ -> Nothing
                        | otherwise -> return Nothing
        Nothing -> return Nothing


maybeAuthPossiblyByToken :: Handler (Maybe (Entity User))
maybeAuthPossiblyByToken = do
    mInfo <- authorizationTokenAuth

    case mInfo of
        Just infos -> do
            x <- runDB $ getBy $ UniqueUser $ jwtAuthInfoIdent infos

            case x of
                Just entUser -> return $ Just entUser
                Nothing -> maybeAuth
        Nothing -> maybeAuth


requireAuthPossiblyByToken :: Handler (Entity User)
requireAuthPossiblyByToken = do
    mInfo <- authorizationTokenAuth

    case mInfo of
        Just infos -> do
            x <- runDB $ getBy $ UniqueUser $ jwtAuthInfoIdent infos

            --maybe requireAuth pure x
            case x of
                Just entUser -> return entUser
                Nothing -> requireAuth
        Nothing -> requireAuth
