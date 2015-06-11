module Handler.Demo where

import Import

getDemoR :: Handler Html
getDemoR = defaultLayout $ do
    --addScript $ StaticR js_address_explorer_js
    sendFile "text/html" "static/index.html"

getAddressExplorerR :: Handler ()
getAddressExplorerR = sendFile "text/js" "static/js/address-explorer.js"

getAddressExplorerCSSR :: Handler ()
getAddressExplorerCSSR = sendFile "text/css" "static/css/address-explorer.css"