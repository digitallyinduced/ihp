{ mkDerivation, base, ghc-prim, lib, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.12.2";
  sha256 = "1gah466nd6hkj716gwljfh0g270iaqy2rq2a1vw3di2s7a4dqam6";
  libraryHaskellDepends = [ base ghc-prim unix ];
  description = "A compatibility layer for base";
  license = lib.licenses.mit;
}