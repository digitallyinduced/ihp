{ mkDerivation, aeson, base, constraints, lib, template-haskell }:
mkDerivation {
  pname = "constraints-extras";
  version = "0.3.2.0";
  sha256 = "dac7cdd1e7bdaebdfe57550da84298f57cf2f41b3573ac39dd077f6c1a2d73d8";
  revision = "2";
  editedCabalFile = "09mdfzrmnl0mc57n7h0w68i8xgp89y5729p2d2h6qb2qj81qsfq1";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base constraints template-haskell ];
  executableHaskellDepends = [ aeson base constraints ];
  homepage = "https://github.com/obsidiansystems/constraints-extras";
  description = "Utility package for constraints";
  license = lib.licenses.bsd3;
}