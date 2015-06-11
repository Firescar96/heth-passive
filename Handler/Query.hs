module Handler.Query where

import Import

getQueryR :: Handler Html
getQueryR = do
    addHeader "Access-Control-Allow-Origin" "*"
    defaultLayout $(widgetFile "query")

