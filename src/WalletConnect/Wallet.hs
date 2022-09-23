{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module WalletConnect.Wallet
  ( Session (..)
  , Proposal (..)
  , WalletConnect (..)
  , initWalletConnect
  , doNewPairing
  , module WalletConnect.Common
  )
  where

import Control.Concurrent
import           Control.Monad
import Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Lens                       hiding ((#))
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as M
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
import Reflex hiding (Request)
import Reflex.Network
import System.Timeout

import WalletConnect.Common
import WalletConnect.Internal

data Session = Session
  { _session_topic :: Topic
  , _session_disconnect :: JSM ()
  , _session_self :: (PublicKey, Metadata)
  , _session_peer :: (PublicKey, Metadata)
  , _session_namespaces :: Namespaces
  , _session_requiredNamespaces :: RequiredNamespaces
  }

data Proposal = Proposal
  { _proposal_id :: JSVal
  , _proposal_topic :: Topic
  , _proposal_proposer :: (PublicKey, Metadata)
  , _proposal_requiredNamespaces :: RequiredNamespaces
  , _proposal_approval :: (Either () Namespaces) -> JSM ()
  }

data WalletConnect t = WalletConnect
  { _walletConnect_pairings :: Dynamic t (Map Topic Pairing)
  , _walletConnect_sessions :: Dynamic t (Map Topic Session)
  , _walletConnect_proposals :: Event t Proposal
  , _walletConnect_requests :: Event t (Topic, Request, Either () A.Value -> JSM ())
  , _walletConnect_client :: MVar JSVal
  }

initWalletConnect ::
  ( TriggerEvent t m
  , PerformEvent t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , Adjustable t m
  , MonadHold t m
  , MonadFix m
  )
  => Maybe Text -- Relay URL
  -> Text       -- Project Id
  -> m (WalletConnect t)
initWalletConnect mRelayUrl projectId = do
  (reqEv, reqAction) <- newTriggerEvent
  (pairingsEv, pairingsAction) <- newTriggerEvent
  (sessionsEv, sessionsAction) <- newTriggerEvent
  (proposalEv, proposalAction) <- newTriggerEvent

  clientMVar <- liftIO $ newEmptyMVar

  liftJSM $ do
    clientPromise <- clientInit mRelayUrl projectId
    clientPromise ^. js2 "then"
      (subscribeToEvents clientMVar reqAction proposalAction sessionsAction pairingsAction)
      logValueF -- TODO: handle errors

  rec
    pairings <- networkHold (pure mempty) $ ffor (attach (current pairings) pairingsEv) $ \(old, new) -> do
      client <- liftIO $ readMVar clientMVar
      (M.fromList <$>) $ forM new $ \(t, p) ->
        case M.lookup t old of
          Just o -> pure (t,o)
          Nothing -> (t,) <$> makePairing client p

  rec
    sessions <- networkHold (pure mempty) $ ffor (attach (current sessions) sessionsEv) $ \(old, new) -> do
      client <- liftIO $ readMVar clientMVar
      (M.fromList <$>) $ forM new $ \(t, s) ->
        case M.lookup t old of
          Just o -> pure (t,o)
          Nothing -> (t,) <$> makeSession client s

  return $ WalletConnect pairings sessions proposalEv reqEv clientMVar

doNewPairing :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => WalletConnect t
  -> Event t Text
  -> m (Event t Bool)
doNewPairing walletConnect uriEv = performEventAsync $ ffor uriEv $ \uri -> \res -> liftJSM $ do
  mClient <- liftIO $ timeout fiveSec $ readMVar (_walletConnect_client walletConnect)
  case mClient of
    Nothing -> liftIO $ res False
    Just client -> do
      p <- doPair uri client
      let resp b = fun $ \_ _ v -> do
            -- logValue "Got pairing response"
            -- logValue v
            liftIO $ res b
      void $ p ^. js2 "then" (resp True) (resp False)
  where fiveSec = 5 * 1000 * 1000

makeSession :: (MonadJSM m) => JSVal -> JSVal -> m Session
makeSession client session = do
  -- liftJSM $ do
  --   logValue "makeSession"
  --   logValue session
  topic <- liftJSM $ valToText =<< session ! "topic"
  peer <- liftJSM $ getMetadataPublicKey =<< session ! "peer"
  self <- liftJSM $ getMetadataPublicKey =<< session ! "self"
  namespaces <- liftJSM $ fromJSValOrMempty =<< session ! "namespaces"
  requiredNamespaces <- liftJSM $ fromJSValOrMempty =<< session ! "requiredNamespaces"
  let
    delete = do
      -- logValue $ "doing disconnect of " <> topic
      args <- do
        o <- create
        (o <# "topic") topic
        reason <- do
          o <- create
          (o <# "code") 6000
          (o <# "message") "User disconnected."
          pure o
        (o <# "reason") reason
        pure o
      void $ client ^. js1 "disconnect" args

  return $ Session topic delete self peer namespaces requiredNamespaces

subscribeToEvents clientMVar reqAction proposalAction sessionAction pairingsAction = fun $ \_ _ (client:_) -> do
  -- logValue ("subscribeToEvents" :: Text)
  -- logValue client

  liftIO $ putMVar clientMVar client

  events <- jsg "window" ! "@walletconnect/sign-client" ! "SIGN_CLIENT_EVENTS"

  let
    onProposal = fun $ \_ _ [proposal'] -> do
      proposal <- proposal' ! "params"
      -- logValue "onProposal"
      -- logValue proposal
      i <- proposal ! "id"
      topic <- valToText =<< proposal ! "pairingTopic"
      requiredNamespaces <- fromJSValOrMempty =<< proposal ! "requiredNamespaces"
      proposer <- getMetadataPublicKey =<< proposal ! "proposer"
      liftIO $ proposalAction $ Proposal i topic proposer requiredNamespaces (either (doReject i) (doApprove i))

    doReject i _ = do
      args <- do
        o <- create
        (o <# "id") i
        reason <- do
          o <- create
          (o <# "code") 5000
          (o <# "message") "User rejected."
          pure o
        (o <# "reason") reason
        pure o
      void $ client ^. js1 "reject" args

    doApprove i namespaces = do
      args <- do
        o <- create
        (o <# "id") i
        (o <# "namespaces") (A.toJSON namespaces)
        pure o
      void $ client ^. js1 "approve" args

  proposal <- events ! "session_proposal"
  client ^. js2 "on" proposal onProposal

  let
    onRequest = fun $ \_ _ [requestEvent] -> do
      -- logValue "onRequest"
      -- logValue requestEvent
      id' <- requestEvent ! "id"
      topic <- valToText =<< requestEvent ! "topic"
      params <- requestEvent ! "params"
      chainId <- valToText =<< params ! "chainId"
      req <- params ! "request"
      method <- valToText =<< req ! "method"
      reqParams <- fromJSValUnchecked =<< req ! "params"
      let
        doSend v = do
          result <- mapM toJSVal v
          doRespond client topic id' result

      liftIO $ reqAction
        ( topic
        , Request chainId method reqParams
        , doSend)

  request <- events ! "session_request"
  client ^. js2 "on" request onRequest

  let onPairingSync = fun $ \_ _ _ -> do
        -- logValue "onSync"
        readPairings

      readPairings = do
        s <- client ! "pairing"
        v <- fromJSValUncheckedListOf =<< s ! "values"
        tp <- forM v $ \pairing -> do
          t <- valToText =<< pairing ! "topic"
          pure (t, pairing)
        liftIO $ pairingsAction tp

  readPairings
  ev1 <- events ! "pairing_delete"
  ev2 <- events ! "pairing_expire"
  forM [ev1, ev2] $ \ev -> client ^. js2 "on" ev onPairingSync

  let onSync = fun $ \_ _ _ -> do
        -- logValue "onSync"
        readSessions

      readSessions = do
        s <- client ! "session"
        v <- fromJSValUncheckedListOf =<< s ! "values"
        tp <- forM v $ \session -> do
          t <- valToText =<< session ! "topic"
          pure (t, session)
        liftIO $ sessionAction tp

  readSessions

  ev3 <- events ! "session_delete"
  ev4 <- events ! "session_expire"
  void $ forM [ev3, ev4] $ \ev -> client ^. js2 "on" ev onSync
