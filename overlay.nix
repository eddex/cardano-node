self: final: _: {
  inherit (self.legacyPackages.${final.system})
    cardano-node cardano-cli cardano-ping bech32 db-converter;
}
