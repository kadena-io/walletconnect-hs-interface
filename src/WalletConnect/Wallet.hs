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

data WalletConnect t = WalletConnect
  { _walletConnect_pairings :: Dynamic t (Map Topic ()) --Pairing)
  , _walletConnect_sessions :: Dynamic t (Map Topic Session)
  , _walletConnect_proposals :: Event t Proposal
  , _walletConnect_requests :: Event t (Topic, Request, Either () A.Value -> JSM ())
  , _walletConnect_client :: MVar JSVal
  }

data Session = Session
  { _session_topic :: Topic
  , _session_disconnect :: JSM ()
  , _session_peer :: (PublicKey, Metadata)
  }

data Proposal = Proposal
  { _proposal_id :: Double
  , _proposal_relays :: [Relay]
  , _proposal_proposer :: (PublicKey, Metadata)
  , _proposal_namespaces :: [ProposalNamespace]
  , _proposal_pairingTopic :: Topic
  , _proposal_approval :: (Either () [Account] -> JSM ())
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
  -- (reqEv, reqAction) <- newTriggerEvent
  -- (pairingsEv, pairingsAction) <- newTriggerEvent
  -- (sessionsEv, sessionsAction) <- newTriggerEvent
  (proposalEv, proposalAction) <- newTriggerEvent
  clientMVar <- liftIO $ newEmptyMVar

  liftJSM $ do
    clientPromise <- clientInit mRelayUrl projectId True
    clientPromise ^. js2 "then" (subscribeToEvents clientMVar proposalAction) logValueF -- TODO: handle errors
      -- (subscribeToEvents clientMVar reqAction proposalAction sessionsAction pairingsAction)


  -- rec
  --   pairings <- networkHold (pure mempty) $ ffor (attach (current pairings) pairingsEv) $ \(old, new) -> do
  --     client <- liftIO $ readMVar clientMVar
  --     (M.fromList <$>) $ forM new $ \(t, p) ->
  --       case M.lookup t old of
  --         Just o -> pure (t,o)
  --         Nothing -> (t,) <$> makePairing client p

  -- rec
  --   sessions <- networkHold (pure mempty) $ ffor (attach (current sessions) sessionsEv) $ \(old, new) -> do
  --     client <- liftIO $ readMVar clientMVar
  --     (M.fromList <$>) $ forM new $ \(t, s) ->
  --       case M.lookup t old of
  --         Just o -> pure (t,o)
  --         Nothing -> (t,) <$> makeSession client s

  return $ WalletConnect (constDyn mempty) (constDyn mempty) proposalEv never clientMVar
  -- return $ WalletConnect pairings sessions proposalEv reqEv clientMVar

subscribeToEvents clientMVar proposalAction = fun $ \_ _ (client:_) -> do
-- subscribeToEvents clientMVar reqAction proposalAction sessionAction pairingsAction = fun $ \_ _ (client:_) -> do
  liftIO $ putMVar clientMVar client
  let
    onProposal = fun $ \_ _ [propEvent] -> do
      logValue ("HERE" :: Text)
      logValue propEvent
      id <- valToNumber =<< propEvent ! "id"
      propParams <- propEvent ! "params"
      topic <- valToText =<< propParams ! "pairingTopic"
      -- ttl <- fromJSValUnchecked =<< propParams ! "expiry"
      namespaces <- getNamespaces propParams
      proposer <-
        getMetadataPublicKey =<< propParams ! "proposer"
      liftIO $ proposalAction $ Proposal id mempty proposer namespaces topic (either (doReject propEvent) (doApprove propEvent))

    doReject propEvent _ = do
      id <- valToText =<< propEvent ! "id"
      args <- do
        reasonObj <- create
        (reasonObj <# "code") 1
        (reasonObj <# "message") "rejected"
        o <- create
        (o <# "id") id
        (o <# "reason") reasonObj
        pure o
      void $ client ^. js1 "reject" args

    doApprove proposal accounts = do
      id <- valToText =<< proposal ! "id"
      namespaces <- do
        kNamespaces <- create
        (kNamespaces <# "accounts") accounts
        -- TODO: Don't hardcode methods
        (kNamespaces <# "methods") ["kadena_sign", "kadena_quicksign"]
        (kNamespaces <# "events") ([] :: [Text])
        oNsp <- create
        (oNsp <# "kadena") kNamespaces
      o <- create
      (o <# "id") id
      (o <# "namespaces") namespaces
      -- TODO: Destructure to get ack-arg to call later
      void $ client ^. js1 "approve" o

  logValue "Setting session prop on event"
  client ^. js2 "on" "session_proposal" onProposal

  -- let
  --   onRequest = fun $ \_ _ [requestEvent] -> do
  --     topic <- valToText =<< requestEvent ! "topic"
  --     id <- valToText =<< requestEvent ! "id"
  --     params <- requestEvent ! "params"

  --     chainId <- valToText =<< params ! "chainId"
  --     req <- params ! "request"
  --     method <- valToText =<< req ! "method"
  --     rpcParams <- fromJSValUnchecked =<< req ! "params"

  --     let
  --       doSend v = do
  --         result <- mapM toJSVal v
  --         doRespond client topic id' result

  --     liftIO $ reqAction
  --       ( topic
  --       , Request chainId method params
  --       , doSend
  --       )

  -- -- request <- session ! "request"
  -- client ^. js2 "on" "session_request" onRequest

  -- let
  --   readPairings = do
  --     s <- client ! "pairing"
  --     v <- fromJSValUncheckedListOf =<< s ! "values"
  --     tp <- forM v $ \pairing -> do
  --       t <- valToText =<< pairing ! "topic"
  --       pure (t, pairing)
  --     liftIO $ pairingsAction tp

  --   onPairingSync = fun $ \_ _ _ -> readPairings

  -- readPairings
  -- void $ client ^. js2 "on" "pairing_ping" onPairingSync
  -- void $ client ^. js2 "on" "pairing_delete" onPairingSync

  -- let onSync = fun $ \_ _ _ -> do
  --       readSessions

  --     readSessions = do
  --       s <- client ! "session"
  --       v <- fromJSValUncheckedListOf =<< s ! "values"
  --       tp <- forM v $ \session -> do
  --         t <- valToText =<< session ! "topic"
  --         pure (t, session)
  --       liftIO $ sessionAction tp

  -- readSessions
  -- void $ client ^. js2 "on" "session_ping" onSync
  pure ()


doNewPairing :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => WalletConnect t
  -> Event t Text
  -> m (Event t Bool)
doNewPairing walletConnect uriEv = performEventAsync $ ffor uriEv $ \uri -> \res -> liftJSM $ do
  mClient <- liftIO $ timeout fiveSec $ readMVar (_walletConnect_client walletConnect)
  case mClient of
    Nothing -> liftIO $ res False
    Just client -> do
      logValue client
      p <- doPair uri client
      let resp b = fun $ \_ _ v -> do
            logValue "Got pairing response"
            logValue v
            logValue "Logging b"
            logValue b
            liftIO $ res b
      void $ p ^. js2 "then" (resp True) (resp False)
  where
    fiveSec = 5 * 1000 * 1000
    doPair uri client = do
      -- logValue "doPair"
      -- logValue uri
      args <- do
        o <- create
        (o <# "uri") uri
        pure o
      client ^. js1 "pair" args
