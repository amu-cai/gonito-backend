module Handler.MakePublic where

import Import

import Handler.Shared

import PersistSHA1

import Data.Text as T

import Handler.Runner
import Handler.JWT

getMakePublicJsonR :: SubmissionId -> Handler Value
getMakePublicJsonR submissionId = do
  Entity userId _ <- requireAuthPossiblyByToken
  runViewProgressAsynchronously $ doMakePublic userId submissionId

getMakePublicR :: SubmissionId -> Handler TypedContent
getMakePublicR submissionId = do
  userId <- requireAuthId
  runViewProgress $ doMakePublic userId submissionId

doMakePublic :: UserId -> SubmissionId -> Channel -> Handler ()
doMakePublic userId submissionId chan = do
  isOwner <- runDB $ checkWhetherGivenUserRepo userId submissionId
  if not isOwner
   then
    err chan "Only the submitter can make a submission public!"
   else do
    msg chan "Making the submission public..."
    runDB $ update submissionId [SubmissionIsPublic =. True]
    submission <- runDB $ get404 submissionId
    challenge <- runDB $ get404 $ submissionChallenge submission
    repo <- runDB $ get404 $ challengePublicRepo challenge
    let submissionRepoId = submissionRepo submission
    submissionRepoDir <- getRepoDirOrClone submissionRepoId chan

    app <- getYesod
    let scheme = appRepoScheme $ appSettings app
    let repoHost = appRepoHost $ appSettings app

    let targetRepoUrl = getPublicSubmissionUrl scheme repoHost (Just repo) $ challengeName challenge
    let targetBranchName = getPublicSubmissionBranch submissionId
    msg chan $ "Start pushing from " ++ (T.pack submissionRepoDir) ++ " to repo " ++ targetRepoUrl ++ ", branch " ++ targetBranchName ++ " ..."
    let commit = submissionCommit submission
    pushRepo submissionRepoDir commit (T.unpack $ targetRepoUrl) (T.unpack $ targetBranchName) chan
  return ()

pushRepo :: String -> SHA1 -> String -> String -> Channel -> Handler ()
pushRepo repoDir commit targetRepoUrl targetBranchName chan = do
  (_, _) <- runProgram (Just repoDir) gitPath [
    "push",
    (T.unpack $ fixGitRepoUrl $ T.pack targetRepoUrl),
    (T.unpack $ fromSHA1ToText commit) ++ ":refs/heads/" ++ targetBranchName] chan
  return ()

checkWhetherUserRepo :: SubmissionId -> Handler Bool
checkWhetherUserRepo submissionId = do
  Entity userId _ <- requireAuthPossiblyByToken
  runDB $ checkWhetherGivenUserRepo userId submissionId
