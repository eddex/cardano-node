{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Shelley.Run.Pretty (friendlyTxBodyLbs) where

import           Cardano.Api as Api
                   (ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                   TxBody)
import           Cardano.Api.Byron (TxBody (ByronTxBody))
import           Cardano.Api.Shelley (TxBody (ShelleyTxBody))
import           Cardano.Binary (Annotated)
import           Cardano.CLI.Helpers (textShow)
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.ShelleyMA (MaryOrAllegra (Allegra, Mary), ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import           Cardano.Prelude
import           Data.Aeson as JSON (Object, Value (..), object, toJSON, (.=))
import           Data.Aeson.Encode.Pretty (Config (confCompare), defConfig, encodePretty')
import qualified Data.HashMap.Strict as HashMap
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Shelley.Spec.Ledger.API (Addr (..), TxOut (TxOut))
import qualified Shelley.Spec.Ledger.API as Shelley

friendlyTxBodyLbs :: Api.TxBody era -> LByteString
friendlyTxBodyLbs =
  encodePretty' defConfig{confCompare = compare} . friendlyTxBody

friendlyTxBody :: Api.TxBody era -> Value
friendlyTxBody = \case
  ByronTxBody tx ->
    _Object (HashMap.insert "era" "Byron") $ friendlyTxBodyByron tx
  ShelleyTxBody ShelleyBasedEraShelley body aux ->
    Object $
    HashMap.insert "era" "Shelley" $
    HashMap.insert "auxiliary_data" (toJSON $ textShow aux) $
    friendlyTxBodyShelley body
  ShelleyTxBody ShelleyBasedEraAllegra body aux ->
    Object $
    HashMap.insert "era" "Allegra" $
    HashMap.insert "auxiliary_data" (toJSON $ textShow aux) $
    friendlyTxBodyAllegra body
  ShelleyTxBody ShelleyBasedEraMary body aux ->
    Object $
    HashMap.insert "era" "Mary" $
    HashMap.insert "auxiliary_data" (toJSON $ textShow aux) $
    friendlyTxBodyMary body

friendlyTxBodyByron :: Annotated Byron.Tx ByteString -> Value
friendlyTxBodyByron = toJSON

friendlyTxBodyShelley :: Shelley.TxBody (ShelleyEra StandardCrypto) -> Object
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
    , "timetolive" .= _ttl
    , "update" .= fmap textShow _txUpdate
    , "metadata_hash" .= fmap textShow _mdHash
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
    , "validity_interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary_data_hash" .= fmap textShow adHash
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
    , "validity_interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary_data_hash" .= fmap textShow adHash
    , "mint" .= mint
    ]

friendlyValidityInterval :: ShelleyMA.ValidityInterval -> Value
friendlyValidityInterval
  ShelleyMA.ValidityInterval{invalidBefore, invalidHereafter} =
    object
      [ "invalid_before" .= invalidBefore
      , "invalid_hereafter" .= invalidHereafter
      ]

friendlyTxOutShelley :: TxOut (ShelleyEra StandardCrypto) -> Value
friendlyTxOutShelley (TxOut address amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress address

friendlyAddress :: Addr crypto -> Object
friendlyAddress = HashMap.fromList . \case
  Addr net cred ref ->
    [ ( "address"
      , object
          [ "network" .= net
          , "credential" .= cred
          , "stake reference" .= textShow ref
          ]
      )
    ]
  AddrBootstrap x -> [("bootstrap address", String $ textShow x)]

friendlyTxOutAllegra :: TxOut (ShelleyMAEra 'Allegra StandardCrypto) -> Value
friendlyTxOutAllegra = toJSON

friendlyTxOutMary :: TxOut (ShelleyMAEra 'Mary StandardCrypto) -> Value
friendlyTxOutMary = toJSON

-- | Lens-ish modifier for a JSON.Object
_Object :: (JSON.Object -> JSON.Object) -> JSON.Value -> JSON.Value
_Object f = \case
  Object a -> Object $ f a
  v -> v
