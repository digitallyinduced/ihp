{ mkDerivation, base, bytestring, containers, haskell-src-meta
, http-types, lib, template-haskell, text, time, unordered-containers
, uuid, wai
}:
mkDerivation {
  pname = "ihp-router";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers haskell-src-meta http-types
    template-haskell text time unordered-containers uuid wai
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Trie-based routing with a Yesod-style DSL for WAI";
  license = lib.licenses.mit;
}
