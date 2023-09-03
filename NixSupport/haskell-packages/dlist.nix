{ mkDerivation, base, deepseq, lib, QuickCheck }:
mkDerivation {
  pname = "dlist";
  version = "1.0";
  sha256 = "0581a60xw4gw7pmqlmg5w2hr4hm9yjgx4c2z6v63y5xv51rn6g8p";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/spl/dlist";
  description = "Difference lists";
  license = lib.licenses.bsd3;
  jailbreak = true;
}