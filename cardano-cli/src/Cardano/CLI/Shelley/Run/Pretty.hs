{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Shelley.Run.Pretty (friendlyTxBodyBS) where

import           Cardano.Prelude hiding (undefined)
import           Prelude (error)

import           Cardano.Api as Api (AddressInEra (..),
                   AddressTypeInEra (ByronAddressInAnyEra, ShelleyAddressInEra),
                   ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                   ShelleyEra, TxBody, serialiseToBech32)
import           Cardano.Api.Byron (TxBody (ByronTxBody))
import           Cardano.Api.Shelley (TxBody (ShelleyTxBody), fromShelleyAddr)
import           Cardano.Binary (Annotated)
import           Cardano.CLI.Helpers (textShow)
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Ledger.Shelley as Ledger (ShelleyEra)
import           Cardano.Ledger.ShelleyMA (MaryOrAllegra (Allegra, Mary), ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import           Data.Aeson as JSON (Object, Value (..), object, toJSON, (.=))
import qualified Data.HashMap.Strict as HashMap
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Shelley.Spec.Ledger.API (Addr (..), TxOut (TxOut))
import qualified Shelley.Spec.Ledger.API as Shelley

friendlyTxBodyBS :: Api.TxBody era -> ByteString
friendlyTxBodyBS =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody

friendlyTxBody :: Api.TxBody era -> Value
friendlyTxBody = \case
  ByronTxBody tx ->
    _Object (HashMap.insert "era" "Byron") $ friendlyTxBodyByron tx
  ShelleyTxBody ShelleyBasedEraShelley body aux ->
    Object $
    HashMap.insert "era" "Shelley" $
    HashMap.insert "auxiliary data" (toJSON $ textShow aux) $
    friendlyTxBodyShelley body
  ShelleyTxBody ShelleyBasedEraAllegra body aux ->
    Object $
    HashMap.insert "era" "Allegra" $
    HashMap.insert "auxiliary data" (toJSON $ textShow aux) $
    friendlyTxBodyAllegra body
  ShelleyTxBody ShelleyBasedEraMary body aux ->
    Object $
    HashMap.insert "era" "Mary" $
    HashMap.insert "auxiliary data" (toJSON $ textShow aux) $
    friendlyTxBodyMary body

friendlyTxBodyByron :: Annotated Byron.Tx ByteString -> Value
friendlyTxBodyByron = toJSON

friendlyTxBodyShelley
  :: Shelley.TxBody (Ledger.ShelleyEra StandardCrypto) -> Object
friendlyTxBodyShelley
  Shelley.TxBody
    { _inputs
    , _outputs
    , _certs
    , _wdrls = Shelley.Wdrl withdrawals
    , _txfee
    , _ttl
    , _txUpdate
    , _mdHash
    } =
  HashMap.fromList
    [ "inputs" .= _inputs
    , "outputs" .= fmap friendlyTxOutShelley _outputs
    , "certificates" .= fmap textShow _certs
    , "withdrawals" .= withdrawals
    , "fee" .= _txfee
    , "time to live" .= _ttl
    , "update" .= fmap textShow _txUpdate
    , "metadata hash" .= fmap textShow _mdHash
    ]

friendlyTxBodyAllegra
  :: ShelleyMA.TxBody (ShelleyMAEra 'Allegra StandardCrypto) -> Object
friendlyTxBodyAllegra
  (ShelleyMA.TxBody
    inputs
    outputs
    certificates
    (Shelley.Wdrl withdrawals)
    txfee
    validity
    update
    adHash
    mint) =
  HashMap.fromList
    [ "inputs" .= inputs
    , "outputs" .= fmap friendlyTxOutAllegra outputs
    , "certificates" .= fmap textShow certificates
    , "withdrawals" .= withdrawals
    , "fee" .= txfee
    , "validity interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary data hash" .= fmap textShow adHash
    , "mint" .= mint
    ]

friendlyTxBodyMary
  :: ShelleyMA.TxBody (ShelleyMAEra 'Mary StandardCrypto) -> Object
friendlyTxBodyMary
  (ShelleyMA.TxBody
    inputs
    outputs
    certificates
    (Shelley.Wdrl withdrawals)
    txfee
    validity
    update
    adHash
    mint) =
  HashMap.fromList
    [ "inputs" .= inputs
    , "outputs" .= fmap friendlyTxOutMary outputs
    , "certificates" .= fmap textShow certificates
    , "withdrawals" .= withdrawals
    , "fee" .= txfee
    , "validity interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary data hash" .= fmap textShow adHash
    , "mint" .= mint
    ]

friendlyValidityInterval :: ShelleyMA.ValidityInterval -> Value
friendlyValidityInterval
  ShelleyMA.ValidityInterval{invalidBefore, invalidHereafter} =
    object
      [ "invalid before" .= invalidBefore
      , "invalid hereafter" .= invalidHereafter
      ]

friendlyTxOutShelley :: TxOut (Ledger.ShelleyEra StandardCrypto) -> Value
friendlyTxOutShelley (TxOut addr amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress addr

friendlyAddress :: Addr StandardCrypto -> Object
friendlyAddress addr =
  HashMap.fromList $
    case addr of
      Addr net cred ref ->
        [ ( "address"
          , object
              [ "network" .= net
              , "credential" .= cred
              , "stake reference" .= textShow ref
              , "Bech32" .= addressBech32
              ]
          )
        ]
      AddrBootstrap _ ->
        [("bootstrap address", object ["Bech32" .= String addressBech32])]
  where
    addressBech32 =
      case fromShelleyAddr @Api.ShelleyEra addr of
        AddressInEra (ShelleyAddressInEra _) a -> serialiseToBech32 a
        AddressInEra ByronAddressInAnyEra _ -> error "expected Shelley address"

friendlyTxOutAllegra :: TxOut (ShelleyMAEra 'Allegra StandardCrypto) -> Value
friendlyTxOutAllegra = toJSON

friendlyTxOutMary :: TxOut (ShelleyMAEra 'Mary StandardCrypto) -> Value
friendlyTxOutMary = toJSON

-- | Lens-ish modifier for a JSON.Object
_Object :: (JSON.Object -> JSON.Object) -> JSON.Value -> JSON.Value
_Object f = \case
  Object a -> Object $ f a
  v -> v
