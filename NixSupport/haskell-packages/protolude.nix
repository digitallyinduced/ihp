{ mkDerivation, array, async, base, bytestring, containers, deepseq
, fetchzip, ghc-prim, hashable, lib, mtl, mtl-compat, stm, text
, transformers, transformers-compat
}:
mkDerivation {
  pname = "protolude";
  version = "0.3.0";
  src = fetchzip {
    url = "https://github.com/zacwood9/protolude/archive/refs/heads/master.zip";
    sha256 = "0sp0k36cps0d2wqjr6ay6yd8ssvmfrwqpp25q40rb3cjdm12dn80";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base bytestring containers deepseq ghc-prim hashable
    mtl mtl-compat stm text transformers transformers-compat
  ];
  homepage = "https://github.com/sdiehl/protolude";
  description = "A small prelude";
  license = lib.licenses.mit;
}
