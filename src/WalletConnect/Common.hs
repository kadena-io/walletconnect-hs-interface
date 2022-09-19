{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WalletConnect.Common where


import qualified Data.Aeson as A
import qualified Data.List.Split                       as L
import           Data.Map                           (Map)
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

data Metadata = Metadata
  { _metadata_name :: Text
  , _metadata_description :: Text
  , _metadata_url :: Text
  , _metadata_icons :: [Text]
  , _metadata_redirect :: Maybe Redirect
  }
  deriving (Show, Generic)

data BaseNamespace = BaseNamespace
  { _pBN_accounts :: [Text]
  , _pBN_methods :: [Text]
  , _pBN_events :: [Text]
  }
  deriving (Show, Generic)

data Namespace = Namespace
  { _pN_base :: BaseNamespace
  , _pN_extension :: Maybe [BaseNamespace]
  }
  deriving (Show, Generic)

type Namespaces = Map Text Namespace

data BaseRequiredNamespace = BaseRequiredNamespace
  { _pBRN_chains :: [Text]
  , _pBRN_methods :: [Text]
  , _pBRN_events :: [Text]
  }
  deriving (Show, Generic)

data RequiredNamespace = RequiredNamespace
  { _pRN_base :: BaseRequiredNamespace
  , _pRN_extension :: Maybe [BaseRequiredNamespace]
  }
  deriving (Show, Generic)

type RequiredNamespaces = Map Text RequiredNamespace

data Request = Request
  { _request_chainId :: Chain
  , _request_method :: Method
  , _request_params :: A.Value
  }

data Relay = Relay
  { _relay_protocol :: Text
  , _relay_data :: Maybe Text
  } deriving (Show, Generic)

data Pairing = Pairing
  { _pairing_topic :: Topic
  , _pairing_relay :: Relay
  , _pairing_peerMeta :: Metadata -- This is always the responder's metadata (ie wallet)
  , _pairing_active :: Bool
  , _pairing_expiry :: Int
  , _pairing_connect :: RequiredNamespaces -> JSM ()
  , _pairing_delete :: JSM ()
  }

type PairingURI = Text

instance A.ToJSON Redirect where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON Redirect where
  parseJSON = A.genericParseJSON compactEncoding

instance A.ToJSON Metadata where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON Metadata where
  parseJSON = A.genericParseJSON compactEncoding

instance A.ToJSON BaseNamespace where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON BaseNamespace where
  parseJSON = A.genericParseJSON compactEncoding

instance A.ToJSON Namespace where
  toJSON (Namespace bn mExts) =
    let A.Object obj = A.toJSON bn
        extHM = case mExts of
          Nothing -> mempty
          Just exts -> let A.Object extsHM = A.object ["extention" A..= exts] in extsHM
    in A.Object (obj <> extHM)

instance A.FromJSON Namespace where
  parseJSON = A.withObject "RequiredNamespace" $ \v -> Namespace
        <$> A.parseJSON (A.Object v)
        <*> (v A..:? "extention")

instance A.ToJSON BaseRequiredNamespace where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON BaseRequiredNamespace where
  parseJSON = A.genericParseJSON compactEncoding

instance A.ToJSON RequiredNamespace where
  toJSON (RequiredNamespace bn mExts) =
    let A.Object obj = A.toJSON bn
        extHM = case mExts of
          Nothing -> mempty
          Just exts -> let A.Object extsHM = A.object ["extention" A..= exts] in extsHM
    in A.Object (obj <> extHM)

instance A.FromJSON RequiredNamespace where
  parseJSON = A.withObject "RequiredNamespace" $ \v -> RequiredNamespace
        <$> A.parseJSON (A.Object v)
        <*> (v A..:? "extention")

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
