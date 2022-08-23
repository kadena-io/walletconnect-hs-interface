{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WalletConnect.Common where


import qualified Data.Aeson as A
import qualified Data.List.Split                       as L
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Language.Javascript.JSaddle

type Account = Text
type Chain = Text
type Method = Text
type Topic = Text
type PublicKey = Text

-- Should never actually be using this
data Redirect = Redirect
  { _redirect_native :: Maybe Text
  , _redirect_universal :: Maybe Text
  } deriving (Show, Generic)

instance A.ToJSON Redirect where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON Redirect where
  parseJSON = A.genericParseJSON compactEncoding

data Metadata = Metadata
  { _metadata_name :: Text
  , _metadata_description :: Text
  , _metadata_url :: Text
  , _metadata_icons :: [Text]
  , _metadata_redirect :: Maybe Redirect
  }
  deriving (Show, Generic)

instance A.ToJSON Metadata where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON Metadata where
  parseJSON = A.genericParseJSON compactEncoding

emptyWCMeta :: Metadata
emptyWCMeta = Metadata "unknown" "" "" mempty Nothing

-- TODO: Fix according to spec
data NamespaceExtension = NamespaceExtension
  { _pNE_methods :: [Text]
  , _pNE_chains :: [Text]
  , _pNE_events :: [Text]
  }
  deriving (Show, Generic)

instance A.ToJSON NamespaceExtension where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON NamespaceExtension where
  parseJSON = A.genericParseJSON compactEncoding

-- data Permissions

-- (AKA "RequiredNamespaces")
data ProposalNamespace = ProposalNamespace
  { _pNamespace_methods :: [Text]
  , _pNamespace_chains :: [Text]
  , _pNamespace_events :: [Text]
  , _pNamespace_extension :: Maybe [NamespaceExtension]
  }
  deriving (Show, Generic)

instance A.ToJSON ProposalNamespace where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON ProposalNamespace where
  parseJSON = A.genericParseJSON compactEncoding

data Request = Request
  { _request_chainId :: Chain
  , _request_method :: Method
  , _request_params :: A.Value
  }

data Relay = Relay
  { _relay_protocol :: Text
  , _relay_data :: Maybe Text
  } deriving (Show, Generic)

type PairingURI = Text

-- data Pairing = Pairing
--   { _pairing_topic :: Topic
--   , _pairing_relay :: Relay
--   , _pairing_peerMeta :: Metadata -- This is always the responder's metadata (ie wallet)
--   , _pairing_active :: Bool
--   -- , _pairing_expiry :: Int
--   -- , _pairing_permissions :: Permissions
--   , _pairing_connect :: (Permissions, Metadata) -> JSM ()
--   , _pairing_delete :: JSM ()
--   }


-- instance A.ToJSON Permissions where
--   toJSON (Permissions chains methods) =
--     A.object [ "blockchain" A..= (A.object ["chains" A..= chains])
--              , "jsonrpc" A..= A.object ["methods" A..= methods]]

-- instance A.FromJSON Permissions where
--   parseJSON = A.withObject "Permissions" $ \v -> Permissions
--         <$> (v A..: "blockchain" >>= A.withObject "blockchain" (\c -> c A..: "chains"))
--         <*> (v A..: "jsonrpc" >>= A.withObject "jsonrpc" (\c -> c A..: "methods"))

compactEncoding :: A.Options
compactEncoding = A.defaultOptions
    { A.fieldLabelModifier = shortener
    , A.allNullaryToStringTag = True
    , A.constructorTagModifier = shortener
    , A.omitNothingFields = True
    , A.sumEncoding = A.ObjectWithSingleField
    , A.unwrapUnaryRecords = True
    , A.tagSingleConstructors = False
    }
  where
    -- As long as names are not empty or just underscores this head should be fine:
    shortener = head . reverse . filter (/= "") . L.splitOn "_"
