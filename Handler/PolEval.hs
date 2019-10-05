{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

module Handler.PolEval where

import Import

getPolEvalTasksR :: Handler Html
getPolEvalTasksR = do
  defaultCustomizableLayout (ourBanner "poland2") $ do
    setTitle "PolEval :: Tasks"
    $(widgetFile "poleval-tasks")

getPolEvalDatesR :: Handler Html
getPolEvalDatesR = do
  defaultCustomizableLayout (ourBanner "poland4") $ do
    setTitle "PolEval :: Dates"
    $(widgetFile "poleval-dates")

getPolEvalResultsR :: Handler Html
getPolEvalResultsR = do
  defaultCustomizableLayout (ourBanner "poland8") $ do
    setTitle "PolEval :: Results"
    $(widgetFile "poleval-results")

getPolEvalPrizesR :: Handler Html
getPolEvalPrizesR = do
  defaultCustomizableLayout (ourBanner "poland3") $ do
    setTitle "PolEval :: Prizes"
    $(widgetFile "poleval-prizes")

getPolEvalPublicationR :: Handler Html
getPolEvalPublicationR = do
  defaultCustomizableLayout (ourBanner "poland7") $ do
    setTitle "PolEval :: Publication"
    $(widgetFile "poleval-publication")

getPolEvalOrganizersR :: Handler Html
getPolEvalOrganizersR = do
  defaultCustomizableLayout (ourBanner "poland8") $ do
    setTitle "PolEval :: Organizers"
    $(widgetFile "poleval-organizers")
