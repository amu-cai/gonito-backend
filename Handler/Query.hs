module Handler.Query where

import Import

import Handler.Tables (formatSubmitter)
import Handler.Shared
import PersistSHA1

import Database.Persist.Sql
import Data.Text as T(pack)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

data FullSubmissionInfo = FullSubmissionInfo {
  fsiSubmissionId :: SubmissionId,
  fsiSubmission :: Submission,
  fsiUser :: User,
  fsiRepo :: Repo,
  fsiChallenge :: Challenge }

getFullInfo :: Entity Submission -> Handler FullSubmissionInfo
getFullInfo (Entity submissionId submission) = do
  repo <- runDB $ get404 $ submissionRepo submission
  user <- runDB $ get404 $ submissionSubmitter submission
  challenge <- runDB $ get404 $ submissionChallenge submission
  return $ FullSubmissionInfo {
    fsiSubmissionId = submissionId,
    fsiSubmission = submission,
    fsiUser = user,
    fsiRepo = repo,
    fsiChallenge = challenge }

findSubmissions :: Text -> Handler [FullSubmissionInfo]
findSubmissions sha1Prefix = do
  mauthId <- maybeAuth
  submissions <- runDB $ case mauthId of
    Just (Entity authId _) -> rawSql "SELECT ?? FROM submission WHERE (is_public OR submitter = ?) AND cast(commit as text) like ?" [toPersistValue authId, PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
    Nothing -> rawSql "SELECT ?? FROM submission WHERE is_public AND cast(commit as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
  mapM getFullInfo submissions

getQueryFormR :: Handler Html
getQueryFormR = do
  (formWidget, formEnctype) <- generateFormPost queryForm
  let submission = Nothing :: Maybe Text
      handlerName = "getQueryFormR" :: Text
  defaultLayout $ do
      aDomId <- newIdent
      setTitle "Searching for submissions"
      $(widgetFile "query-form")

postQueryFormR :: Handler Html
postQueryFormR = do
    ((result, formWidget), formEnctype) <- runFormPost queryForm
    let handlerName = "postQueryFormR" :: Text
    case result of
      FormSuccess query -> processQuery query
      _ -> defaultLayout $ do
        aDomId <- newIdent
        setTitle "Searching for submissions"
        $(widgetFile "query-form")

getQueryResultsR :: Text -> Handler Html
getQueryResultsR = processQuery

processQuery :: Text -> Handler Html
processQuery query = do
  submissions <- findSubmissions query
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "query results"
    $(widgetFile "query-results")

queryResult submission = do
  $(widgetFile "query-result")
    where commitSha1AsText = fromSHA1ToText $ submissionCommit $ fsiSubmission submission
          submitter = formatSubmitter $ fsiUser submission
          publicSubmissionBranch = getPublicSubmissionBranch $ fsiSubmissionId submission
          publicSubmissionRepo = getReadOnlySubmissionUrl $ challengeName $ fsiChallenge submission
          browsableUrl = browsableGitRepoBranch (challengeName $ fsiChallenge submission) publicSubmissionBranch
          stamp = T.pack $ show $ submissionStamp $ fsiSubmission submission

queryForm :: Form Text
queryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgGitCommitSha1) Nothing