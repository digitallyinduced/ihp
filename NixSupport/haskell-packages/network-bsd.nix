{ mkDerivation, base, deepseq, network, stdenv }:
mkDerivation {
  pname = "network-bsd";
  version = "2.8.1.0";
  sha256 = "0kid0811lv4x761fd5gv6lsc8p5j2bn41rfd366pjb642p562jfr";
  libraryHaskellDepends = [ base deepseq network ];
  homepage = "https://github.com/haskell/network-bsd";
  description = "POSIX network database (<netdb.h>) API";
  license = stdenv.lib.licenses.bsd3;
}