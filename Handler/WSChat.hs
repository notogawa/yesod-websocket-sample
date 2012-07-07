{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WSChat where

import Import

getWSChatR :: Handler RepHtml
getWSChatR = do
    defaultLayout $ do
        setTitle "WebSocket Chat"
        addScriptRemote "http://code.jquery.com/jquery-1.6.3.min.js"
        $(widgetFile "wschat")
