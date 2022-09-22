{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module WalletConnect.Internal
  where

import           Control.Lens                       hiding ((#))
import           Control.Monad
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import           Data.Text                          (Text)
import           Language.Javascript.JSaddle
                                         hiding ( eval
                                                , jsf
                                                , js
                                                , js0
                                                , jss
                                                , js2
                                                , js3
                                                , jsg2
                                                , jsg
                                                , js1
                                                , (<#)
                                                , (!)
                                                )

import qualified Language.Javascript.JSaddle   as JSaddle

import WalletConnect.Common

clientInit :: Maybe Text -> Text -> JSM JSVal
clientInit mRelayUrl projectId = do
  client <- jsg "window" ! "@walletconnect/sign-client" ! "SignClient"
  args <- do
    o <- create
    (o <# "logger") ("debug" :: Text)
    forM mRelayUrl $ \ r -> (o <# "relayUrl") r
    (o <# "projectId") projectId
    pure o
  client ^. js1 "init" args

getMetadataPublicKey :: (MonadJSM m) => JSVal -> m (PublicKey, Metadata)
getMetadataPublicKey v = liftJSM $ do
  pk <- valToText =<< v ! "publicKey"
  (pk,) <$> getMetadata v

getMetadata :: (MonadJSM m) => JSVal -> m Metadata
getMetadata v = liftJSM $ do
  getMetadata' =<< v ! "metadata"

getMetadata' :: (MonadJSM m) => JSVal -> m Metadata
getMetadata' meta = liftJSM $ do
  mMetadata <- mapM fromJSVal =<< maybeNullOrUndefined meta
  pure $ case A.fromJSON <$> join mMetadata of
    Just (A.Success m) -> m
    _ -> Metadata "unknown" "" "" [] Nothing

fromJSValOrMempty :: (MonadJSM m, A.FromJSON a, Monoid a) => JSVal -> m a
fromJSValOrMempty obj = liftJSM $ do
  mObj <- mapM fromJSVal =<< maybeNullOrUndefined obj
  pure $ case A.fromJSON <$> join mObj of
    Just (A.Success m) -> m
    _ -> mempty

makePairing :: MonadJSM m => JSVal -> JSVal -> m Pairing
makePairing client pairing = liftJSM $ do
  -- logValue "makePairing"
  topic <- valToText =<< pairing ! "topic"
  peerMeta <- getMetadata' =<< pairing ! "peerMetadata"
  isActive <- valToBool =<< pairing ! "active"
  expiry <- fromJSValUnchecked =<< pairing ! "expiry"
  let
    connect = void . doConnect client (Just topic)
    delete = do
      -- logValue $ "doing disconnect of " <> topic
      args <- do
        o <- create
        (o <# "topic") topic
        (o <# "reason") ("User disconnected." :: Text) -- todo
        reason <- do
          o <- create
          (o <# "code") 6000
          (o <# "message") "User disconnected."
          pure o
        (o <# "reason") reason
        pure o
      pairing <- client ! "pairing"
      void $ pairing ^. js1 "delete" args
  pure $ Pairing topic (Relay "" Nothing) peerMeta isActive expiry connect delete

doConnect :: JSVal -> Maybe Topic -> RequiredNamespaces -> JSM JSVal
doConnect client mTopic namespaces = do
  -- logValue "doConnect"

  args <- do
    o <- create
    (o <# "requiredNamespaces") =<< toJSVal (A.toJSON namespaces)
    forM mTopic $ \pairingTopic -> do
      (o <# "pairingTopic") pairingTopic
    pure o
  -- logValue args
  client ^. js1 "connect" args

doPair uri client = do
  -- logValue "doPair"
  -- logValue uri
  args <- do
    o <- create
    (o <# "uri") uri
    pure o
  client ^. js1 "pair" args

doRequest :: JSVal -> Topic -> Request -> JSM JSVal
doRequest client topic (Request chainId method params) = do
  -- logValue "doRequest"
  -- logValue topic
  args <- do
    o <- create
    (o <# "topic") topic
    (o <# "chainId") =<< toJSVal chainId
    request <- do
      o <- create
      (o <# "method") =<< toJSVal method
      (o <# "params") =<< toJSVal params
      pure o
    (o <# "request") request
    pure o
  -- logValue args
  client ^. js1 "request" args

doRespond :: JSVal -> Topic -> JSVal -> Either () JSVal -> JSM ()
doRespond client topic id' result = do
  -- logValue "doRespond"
  -- logValue topic
  args <- do
    o <- create
    (o <# "topic") topic
    response <- do
      o <- create
      case result of
        Left _ -> do
          error <- do
            o <- create
            -- USER_REJECTED
            (o <# "message") ("User rejected." :: Text)
            (o <# "code") (5000 :: Int)
            pure o
          (o <# "error") error
        Right v -> (o <# "result") v
      (o <# "jsonrpc") ("2.0" :: Text)
      (o <# "id") id'
      pure o
    (o <# "response") response
    pure o
  -- logValue args
  void $ client ^. js1 "respond" args

-- JSaddle APIs

-- Specialised for Text
eval t = JSaddle.eval (t :: Text)
jsf t = JSaddle.jsf (t :: Text)
js t = JSaddle.js (t :: Text)
jss t = JSaddle.jss (t :: Text)
js0 t = JSaddle.js0 (t :: Text)
js1 t = JSaddle.js1 (t :: Text)
js2 t = JSaddle.js2 (t :: Text)
js3 t = JSaddle.js3 (t :: Text)
jsg t = JSaddle.jsg (t :: Text)
jsg2 t = JSaddle.jsg2 (t :: Text)
valT t = val (t :: Text)
(<#) o t = (JSaddle.<#) o (t :: Text)
(!) o t = (JSaddle.!) o (t :: Text)

logValueF = fun $ \_ _ [value] -> logValue value

logValue value = do
  w <- jsg "console"
  w ^. js1 "log" value
  pure ()
