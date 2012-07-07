{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell #-}
import Prelude
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultDevelApp)
import Settings             (parseExtra)
import Application          (makeApplication)

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

type Client = (Text, WS.Sink WS.Hybi10)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message

staticApp :: Network.Wai.Application
staticApp = Static.staticApp Static.defaultFileServerSettings
  { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static")
  }

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    msg <- WS.receiveData
    liftIO $ print msg
    clients <- liftIO $ readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) ->
                WS.sendTextData ("Wrong announcement" :: Text)
            | any ($ fst client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData ("Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)
            | clientExists client clients ->
                WS.sendTextData ("User already exists" :: Text)
            | otherwise -> do
               liftIO $ modifyMVar_ state $ \s -> do
                   let s' = addClient client s
                   WS.sendSink sink $ WS.textData $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map fst s)
                   broadcast (fst client `mappend` " joined") s'
                   return s'
               talk state client
          where
            prefix = "Hi! I am "
            client = (T.drop (T.length prefix) msg, sink)

talk :: WS.Protocol p => MVar ServerState -> Client -> WS.WebSockets p ()
talk state client@(user, _) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
    talk state client
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClient client s
            broadcast (user `mappend` " disconnected") s'
            return s'
        _ -> return ()

main :: IO ()
--main = defaultMain (fromArgs parseExtra) makeApplication
main = do
  (port, app) <- defaultDevelApp (fromArgs parseExtra) makeApplication
  state <- newMVar newServerState
  Warp.runSettings Warp.defaultSettings
                  { Warp.settingsPort = port
                  , Warp.settingsIntercept = WaiWS.intercept (application state)
                  } app
