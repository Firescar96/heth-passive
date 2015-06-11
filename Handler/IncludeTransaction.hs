{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Handler.IncludeTransaction where

import Blockchain.Data.DataDefs
import Data.Aeson
import Handler.JsonJuggler
import Import

postIncludeTransactionR :: Handler ()
postIncludeTransactionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Content-Type"
  
    tx <- parseJsonBody :: Handler (Result RawTransaction')
    case tx of
        (Success (RawTransaction' raw _)) -> do
                _ <- runDB $ insert $ raw
                let res = toJSON (rawTransactionTxHash raw)
                case res of
                    (String h') ->
                        sendResponseStatus status200 (("/query/transaction?hash=" ++ h') :: Text)
                    _ -> sendResponseStatus status404  ("Could not parse" :: Text)
        
        (Error _msg) -> do
            --liftIO $ Import.putStrLn $ T.pack $ msg
            sendResponseStatus status404  ("Could not parse" :: Text)

optionsIncludeTransactionR :: Handler RepPlain
optionsIncludeTransactionR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Headers" "Content-Type"
  addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"

  return $ RepPlain $ toContent ("" :: Text)
