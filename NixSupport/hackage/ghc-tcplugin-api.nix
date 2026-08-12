{ mkDerivation, array, base, containers, ghc, lib, template-haskell
, transformers
}:
mkDerivation {
  pname = "ghc-tcplugin-api";
  version = "0.19.0.0";
  sha256 = "6e8e6a7a5b24637c0b4dcdeea1d7f553f8fde2b55baadd54c09cfb4f84792e90";
  libraryHaskellDepends = [
    array base containers ghc template-haskell transformers
  ];
  homepage = "https://github.com/sheaf/ghc-tcplugin-api";
  description = "An API for type-checker plugins";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
