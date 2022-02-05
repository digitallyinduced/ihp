{ mkDerivation, base, byteslice, bytestring, fetchzip, gauge
, ghc-hs-meta, integer-logarithms, lib, natural-arithmetic
, primitive, primitive-offset, primitive-unlifted, QuickCheck
, quickcheck-classes, quickcheck-instances, run-st, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, text-short
, vector, wide-word, zigzag
}:
mkDerivation {
  pname = "bytebuild";
  version = "0.3.9.0";
  src = fetchzip {
    url = "https://github.com/zacwood9/bytebuild/archive/refs/heads/master.zip";
    sha256 = "04p7a6a3h62lsvj2h0319nqyhzqf4ks17pkzivsq9mr4iccwjv0v";
  };
  libraryHaskellDepends = [
    base byteslice bytestring ghc-hs-meta integer-logarithms
    natural-arithmetic primitive primitive-offset primitive-unlifted
    run-st template-haskell text-short wide-word zigzag
  ];
  testHaskellDepends = [
    base byteslice bytestring natural-arithmetic primitive
    primitive-unlifted QuickCheck quickcheck-classes
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text
    text-short vector wide-word
  ];
  benchmarkHaskellDepends = [
    base byteslice gauge natural-arithmetic primitive text-short
  ];
  homepage = "https://github.com/byteverse/bytebuild";
  description = "Serialize to a small byte arrays";
  license = lib.licenses.bsd3;
}
