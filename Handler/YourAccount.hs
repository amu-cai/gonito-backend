module Handler.YourAccount where

import           Import                     hiding (Proxy, encodeUtf8, fromList)
import           Yesod.Form.Bootstrap3      (BootstrapFormLayout (..), bfs,
                                             renderBootstrap3)

import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import           Data.Conduit.Binary

import           System.Directory
import           System.IO
import           System.Process

import           Handler.Common             (isPasswordAcceptable,
                                             passwordConfirmField,
                                             tooWeakPasswordMessage,
                                             updatePassword)
import           Handler.JWT
import           Handler.Shared

import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS

import           Control.Lens               hiding ((.=), (^.))
import           Data.Proxy                 as DPR
import           Data.Swagger.Declare

import           Data.Aeson                 hiding (Key)
import           Data.Aeson.Key             (fromText)
import           Data.Aeson.KeyMap          hiding (delete, filter, foldr,
                                             fromList, insert, map, null)

import           Data.HashMap.Strict.InsOrd (fromList)

import           Data.Text                  (strip)
import           Data.Text.Encoding         (encodeUtf8)

getYourAccountR :: Handler Html
getYourAccountR = do
    userId <- requireAuthId
    user <- runDB $ get404 userId

    enableTriggerToken userId (userTriggerToken user)

    mIndividualKey <- fetchIndividualKey user

    keyS <- runDB $ selectFirst [PublicKeyUser ==. userId] []
    let key = publicKeyPubkey . entityVal <$> keyS
    (formWidget, formEnctype) <- generateFormPost (yourAccountForm (userName user) (userLocalId user) key (userAltRepoScheme user) (userIsAnonymous user))
    defaultLayout $ do
        setTitle "Your account"
        $(widgetFile "your-account")

postYourAccountR :: Handler Html
postYourAccountR = do
    ((result, formWidget), formEnctype) <- runFormPost (yourAccountForm Nothing Nothing Nothing Nothing False)
    userId <- requireAuthId
    user <- runDB $ get404 userId

    enableTriggerToken userId (userTriggerToken user)

    let accountData = case result of
            FormSuccess res -> Just res
            _               -> Nothing

    mIndividualKey <- case accountData of
        Just (name, localId, mPassword, sshPubKey, mAltRepoScheme, avatarFile, anonimised) -> do
            if checkPassword mPassword
                then do
                    updateUserAccount userId name localId mPassword sshPubKey mAltRepoScheme avatarFile anonimised
                    user' <- runDB $ get404 userId
                    fetchIndividualKey user'
                else do
                    tooWeakPasswordMessage
                    return Nothing
        Nothing -> do
            setMessage $ toHtml ("Something went wrong, probably the password did not match" :: Text)
            return Nothing

    defaultLayout $ do
        setTitle "Your account"
        $(widgetFile "your-account")

checkPassword :: Maybe Text -> Bool
checkPassword Nothing = True
checkPassword (Just "") = True
checkPassword (Just passwd) = isPasswordAcceptable passwd

autocompleteOff :: (RenderMessage master msg2, RenderMessage master msg1) => msg1 -> msg2 -> FieldSettings master
autocompleteOff name tooltip = setts { fsAttrs = fsAttrs setts ++ [("autocomplete", "nope")] }
   where setts = (bfs name) { fsTooltip = Just $ SomeMessage tooltip }

yourAccountForm :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Form (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe FileInfo, Bool)
yourAccountForm maybeName maybeLocalId maybeSshPubKey maybeAltRepoScheme anonimised = renderBootstrap3 BootstrapBasicForm $ (,,,,,,)
    <$> aopt textField (fieldWithTooltip MsgAccountName MsgAccountNameTooltip) (Just maybeName)
    <*> aopt textField (autocompleteOff MsgId MsgIdTooltip) (Just maybeLocalId)
    <*> aopt passwordConfirmField (bfs MsgPassword) Nothing
    <*> aopt textField (fieldWithTooltip MsgSshPubKey MsgSshPubKeyTooltip) (Just maybeSshPubKey)
    <*> aopt textField (fieldWithTooltip MsgAltRepoScheme MsgAltRepoSchemeTooltip) (Just maybeAltRepoScheme)
    <*> fileAFormOpt (bfs MsgAvatar)
    <*> areq checkBoxField (bfs MsgWantToBeAnonimised) (Just anonimised)

updateUserAccount :: Key User -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe FileInfo -> Bool -> Handler ()
updateUserAccount userId name maybeLocalId maybePassword maybeSshPubKey maybeAltRepoScheme maybeAvatarFile anonimised = do
  updateJustName userId name
  updateAvatar userId maybeAvatarFile
  updateLocalIdAndPubKey userId maybeLocalId maybeSshPubKey
  updateAnonimity userId anonimised
  updateAltRepoScheme userId maybeAltRepoScheme
  case maybePassword of
    Nothing -> return ()
    Just "" -> return ()
    Just p -> updatePassword userId (Just p)

updateAvatar :: Key User -> Maybe FileInfo -> Handler ()
updateAvatar _ Nothing = return ()
updateAvatar userId (Just avatarFile) = do
  fileBytes <- runResourceT $ fileSource avatarFile $$ sinkLbs
  runDB $ update userId [UserAvatar =. Just (S.pack . L.unpack $ fileBytes)]

updateLocalIdAndPubKey :: Key User -> Maybe Text -> Maybe Text -> Handler ()
updateLocalIdAndPubKey userId (Just localId) maybeSshPubKey = do
  if isLocalIdAcceptable localId
    then
     do
      otherTheSame <- runDB $ selectFirst [UserLocalId ==. Just localId, UserId !=. userId] []
      case otherTheSame of
        Just _ -> do
          setMessage $ toHtml ("ID already used" :: Text)
        Nothing -> do
          user <- runDB $ get404 userId
          case userLocalId user of
             Just prevLocalId -> do
               unless (prevLocalId == localId) $ setMessage $ toHtml ("only the administrator can change your ID" :: Text)
             Nothing -> do
               runDB $ update userId [UserLocalId =. Just localId]
               setMessage $ toHtml ("ID set" :: Text)
          runDB $ deleteWhere [PublicKeyUser ==. userId]
          case maybeSshPubKey of
            Just key -> do
              _ <- runDB $ insert $ PublicKey {
                publicKeyUser=userId,
                publicKeyPubkey=key }
              setMessage $ toHtml ("SSH public key added; now it may take 10 minutes for the keys to be active, please be patient" :: Text)
              return ()
            Nothing -> return ()
    else
      setMessage $ toHtml ("unexpected ID (use only lower-case letters, digits and hyphens, start with a letter)" :: Text)

updateLocalIdAndPubKey _ Nothing (Just _) = do
  setMessage $ toHtml ("SSH public key cannot be added without an ID" :: Text)

updateLocalIdAndPubKey _ Nothing Nothing = return ()

updateAltRepoScheme :: Key User -> Maybe Text -> Handler ()
updateAltRepoScheme userId mAltRepoScheme = runDB $ update userId [UserAltRepoScheme =. mAltRepoScheme]

updateJustName :: Key User -> Maybe Text -> Handler ()
updateJustName userId name = runDB $ update userId [UserName =. name]

updateAnonimity :: Key User -> Bool -> Handler ()
updateAnonimity userId anonimised = runDB $ update userId [UserIsAnonymous =. anonimised]

getAvatarR :: UserId -> Handler TypedContent
getAvatarR userId = do
    user <- runDB $ get404 userId
    case userAvatar user of
     Just avatarBytes -> do
       addHeader "Content-Disposition" "attachment; filename=\"avatar.png\""
       sendResponse (typePng, toContent avatarBytes)
     Nothing -> do
       sendFile typeSvg "static/images/male-avatar.svg"

getUserInfoR :: Handler Value
getUserInfoR = do
  (Entity _ user) <- requireAuthPossiblyByToken
  return $ String $ userIdent user

data FullUserInfoView = FullUserInfoView {
  fullUserInfoViewIdent :: Text,
  fullUserInfoViewName :: Maybe Text,
  fullUserInfoViewLocalId :: Maybe Text,
  fullUserInfoViewIndividualKey :: Maybe Text
}

instance ToSchema FullUserInfoView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    return $ NamedSchema (Just "FullUserInfo") $ mempty
        & (type_ ?~ SwaggerObject)
        & properties .~
           fromList [  ("ident", stringSchema)
                     , ("name", stringSchema)
                     , ("localId", stringSchema)
                     , ("individualKey", stringSchema)
                    ]
        & required .~ [ "ident" ]


instance ToJSON FullUserInfoView where
    toJSON entry = object
        [ "ident" .= fullUserInfoViewIdent entry
        , "name" .= fullUserInfoViewName entry
        , "localId" .= fullUserInfoViewLocalId entry
        , "individualKey" .= fullUserInfoViewIndividualKey entry
        ]

getFullUserInfoR :: Handler Value
getFullUserInfoR = do
  (Entity _ user) <- requireAuthPossiblyByToken
  mIndividualKey <- fetchIndividualKey user
  return $ toJSON $ FullUserInfoView {
    fullUserInfoViewIdent = userIdent user,
    fullUserInfoViewName = userName user,
    fullUserInfoViewLocalId = userLocalId user,
    fullUserInfoViewIndividualKey = mIndividualKey
    }

userInfoApi :: Swagger
userInfoApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareUserInfoApi mempty

declareUserInfoApi :: Declare (Definitions Schema) Swagger
declareUserInfoApi = do
  -- param schemas
  response <- declareResponse (Proxy :: Proxy String)

  return $ mempty
    & paths .~ fromList [
        ("/api/user-info"
        , mempty & DS.get ?~
            ( mempty
            & parameters .~ [ ]
            & produces ?~ MimeList ["application/json"]
            & description ?~ "Returns the identifier of the user"
            & at 200 ?~ Inline response))
        ]

fullUserInfoApi :: Swagger
fullUserInfoApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareFullUserInfoApi mempty

declareFullUserInfoApi :: Declare (Definitions Schema) Swagger
declareFullUserInfoApi = do
  -- param schemas
  response <- declareResponse (Proxy :: Proxy FullUserInfoView)

  return $ mempty
    & paths .~ fromList [
        ("/api/full-user-info"
        , mempty & DS.get ?~
            ( mempty
            & parameters .~ [ ]
            & produces ?~ MimeList ["application/json"]
            & description ?~ "Returns info about the user"
            & at 200 ?~ Inline response ))
        ]
