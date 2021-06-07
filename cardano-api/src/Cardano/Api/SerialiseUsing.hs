{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseUsing
  ( UsingRawBytes(..)
  , UsingRawBytesHex(..)
  ) where

import           Prelude

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.String (IsString (..))
import qualified Data.Text.Encoding as Text
import           Data.Typeable

import           Data.Aeson (ToJSONKey(..), FromJSONKey(..))
import qualified Data.Aeson.Types as Aeson

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw



-- | For use with @deriving via@, to provide 'ToCBOR' and 'FromCBOR' instances,
-- based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (ToCBOR, FromCBOR) via (UsingRawBytes Blah)
--
newtype UsingRawBytes a = UsingRawBytes a

instance (SerialiseAsRawBytes a, Typeable a) => ToCBOR (UsingRawBytes a) where
    toCBOR (UsingRawBytes x) = toCBOR (serialiseToRawBytes x)

instance (SerialiseAsRawBytes a, Typeable a) => FromCBOR (UsingRawBytes a) where
    fromCBOR = do
      bs <- fromCBOR
      case deserialiseFromRawBytes ttoken bs of
        Just x  -> return (UsingRawBytes x)
        Nothing -> fail ("cannot deserialise as a " ++ tname)
      where
        ttoken = proxyToAsType (Proxy :: Proxy a)
        tname  = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)


-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
--
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
    show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
    fromString str =
      case Base16.decode (BSC.pack str) of
        Right raw -> case deserialiseFromRawBytes ttoken raw of
          Just x  -> UsingRawBytesHex x
          Nothing -> error ("fromString: cannot deserialise " ++ show str)
        Left msg -> error ("fromString: invalid hex " ++ show str ++ ", " ++ msg)
      where
        ttoken :: AsType a
        ttoken = proxyToAsType Proxy

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
    toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

instance (SerialiseAsRawBytes a, Typeable a) => FromJSON (UsingRawBytesHex a) where
    parseJSON =
      Aeson.withText tname $ \str ->
        case Base16.decode (Text.encodeUtf8 str) of
          Right raw -> case deserialiseFromRawBytes ttoken raw of
            Just x  -> return (UsingRawBytesHex x)
            Nothing -> fail ("cannot deserialise " ++ show str)
          Left msg  -> fail ("invalid hex " ++ show str ++ ", " ++ msg)
      where
        ttoken = proxyToAsType (Proxy :: Proxy a)
        tname  = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a)
instance (SerialiseAsRawBytes a, Typeable a) => FromJSONKey (UsingRawBytesHex a)

