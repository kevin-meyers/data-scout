{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CompanyCreateEdit where

import Import

data CompanyData = CompanyData
    { companyDataName :: Text
    , companyDataDescription :: Maybe Text
    }
  deriving Show

nameAttributes :: FieldSettings master
nameAttributes = FieldSettings 
    "Name*" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing
    [("class", "")] -- list of attributes and their values

descriptionAttributes :: FieldSettings master
descriptionAttributes = FieldSettings 
    "Description" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing
    [("class", "")] -- list of attributes and their values

companyForm :: Maybe Company -> Form CompanyData
companyForm company = renderDivs $ CompanyData
    <$> areq textField nameAttributes (companyName <$> company)
    <*> aopt textField descriptionAttributes (companyDescription <$> company)

getCompanyCreateR :: Handler Html
getCompanyCreateR = do
    (widget, enctype) <- generateFormPost $ companyForm Nothing
    defaultLayout $ do
        setTitle . toHtml $ ("Create new company" :: Text)
        $(widgetFile "company-create")

postCompanyCreateR :: Handler ()
postCompanyCreateR = do
    ((result, _), _) <- runFormPost $ companyForm Nothing
    case result of
        FormSuccess companyData -> do
            uid <- requireAuthId
            companyId <- runDB $ insert $ Company
                (companyDataName companyData)
                (companyDataDescription companyData)
                0 -- default number of seats
                uid
            redirect $ CompanyR companyId CompanyDetailR
        _ -> redirect CompanyCreateR

getCompanyEditR :: CompanyId -> Handler Html
getCompanyEditR companyId = do
    company <- runDB $ get404 companyId
    (widget, enctype) <- generateFormPost $ companyForm $ Just company
    defaultLayout $ do
        setTitle . toHtml $ "Update company " <> companyName company
        $(widgetFile "company-edit")

postCompanyEditR :: CompanyId -> Handler ()
postCompanyEditR companyId = do
    ((result, _), _) <- runFormPost $ companyForm Nothing
    case result of
        FormSuccess companyData -> do
            runDB $ update companyId
                [ CompanyName =. companyDataName companyData
                , CompanyDescription =. companyDataDescription companyData
                ]
            redirect $ CompanyR companyId CompanyDetailR
        _ -> redirect $ CompanyR companyId CompanyEditR

