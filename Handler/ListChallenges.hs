module Handler.ListChallenges where

import Import

getListChallengesR :: Handler Html
getListChallengesR = generalListChallenges [ChallengeArchived !=. Just True]

getListArchivedChallengesR :: Handler Html
getListArchivedChallengesR = generalListChallenges [ChallengeArchived ==. Just True]

generalListChallenges filterExpr = do
  challenges <- runDB $ selectList filterExpr [Desc ChallengeStarred, Desc ChallengeStamp]
  defaultCustomizableLayout (ourBanner "poland2") $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

listChallengesCore challenges = $(widgetFile "list-challenges-core")

getChallengeImageR :: ChallengeId -> Handler Html
getChallengeImageR challengeId = do
   challenge <- runDB $ get404 challengeId
   case challengeImage challenge of
     Just image -> do
       addHeader "Content-Disposition" "attachment; filename=\"image.png\""
       sendResponse (typePng, toContent image)
     Nothing -> sendResponseStatus status202 ()
