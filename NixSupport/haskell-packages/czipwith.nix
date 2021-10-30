{ mkDerivation, base, lib, template-haskell, transformers, fetchFromGitHub }:
mkDerivation {
  pname = "czipwith";
  version = "1.0.1.3";
  sha256 = "2dc48540e574ebc924fe13ca2b08be103d228fd42ef90db2896e3727eb0f6687";
  libraryHaskellDepends = [ base template-haskell ];
  testHaskellDepends = [ base transformers ];
  homepage = "https://github.com/lspitzner/czipwith/";
  description = "CZipWith class and deriving via TH";
  license = lib.licenses.bsd3;
  
  # https://github.com/lspitzner/czipwith/pull/2
  src = fetchFromGitHub {
    owner = "mithrandi";
    repo = "czipwith";
    rev = "b6245884ae83e00dd2b5261762549b37390179f8";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i6";
  };
}