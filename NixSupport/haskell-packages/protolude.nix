{ mkDerivation, array, async, base, bytestring, containers, deepseq
, ghc-prim, hashable, lib, mtl, mtl-compat, stm, text, transformers
, transformers-compat
, fetchFromGitHub
}:
mkDerivation {
  pname = "protolude";
  version = "0.3.0";
  sha256 = "4083385a9e03fab9201f63ce198b9ced3fbc1c50d6d42574db5e36c757bedcac";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base bytestring containers deepseq ghc-prim hashable
    mtl mtl-compat stm text transformers transformers-compat
  ];
  homepage = "https://github.com/sdiehl/protolude";
  description = "A small prelude";
  license = lib.licenses.mit;
  
  # https://github.com/protolude/protolude/pull/126
  src = fetchFromGitHub {
    owner = "newhoggy";
    repo = "protolude";
    rev = "0572be7242d8ffa69d3c0ce0954ccc0cfd39cef2";
    sha256 = "sha256-QftuKgdtTv/o/jtVyS9e/ot4yh7BGCgff/EBjukAmCE";
  };
}