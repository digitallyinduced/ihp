{ mkDerivation, aeson, base, binary, bytestring, data-default
, deepseq, filepath, hashable, lens, network-uri, scientific
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.22.0.0";
  sha256 = "05475d5rwkmsh50q18lans7zzd34jhfdpg80m7aijg829dkphskm";
  libraryHaskellDepends = [
    aeson base binary bytestring data-default deepseq filepath hashable
    lens network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}