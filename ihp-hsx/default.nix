{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, containers, ghc, hspec, lib, lucid2, megaparsec, mtl
, string-conversions, template-haskell, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "ihp-hsx";
  version = "1.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html blaze-markup bytestring containers ghc lucid2
    megaparsec string-conversions template-haskell text transformers
    unordered-containers
  ];
  testHaskellDepends = [
    base blaze-markup bytestring containers hspec lucid2 megaparsec mtl
    string-conversions template-haskell text unordered-containers
  ];
  doHaddock = false;
  description = "JSX-like but for Haskell";
  license = lib.licenses.mit;
}
