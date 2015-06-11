module Handler.Help where

import Import

-- getHelpR :: Handler Html
-- getHelpR = defaultLayout $ do
--     sendFile "text/html" "static/help.html"

getHelpR :: Handler Html
getHelpR = do
    defaultLayout $ do
        setTitle "BlockApps.net"
        $(widgetFile "help")