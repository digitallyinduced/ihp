  { mkDerivation, base, criterion, deepseq, doctest, HUnit
   , inspection-testing, lens, profunctors, QuickCheck, tagged, stdenv
   }:
   mkDerivation {
     pname = "generic-lens";
     version = "1.0.0.1";
     sha256 = "0j83ynggqfaxp9g36lkjl9af57qixid9j1x1ljglny1zxqkgm888";
     libraryHaskellDepends = [ base profunctors tagged ];
     testHaskellDepends = [
       base doctest HUnit inspection-testing lens profunctors
     ];
     benchmarkHaskellDepends = [
       base criterion deepseq lens QuickCheck
     ];
     description = "Generically derive traversals, lenses and prisms";
     license = stdenv.lib.licenses.bsd3;
     hydraPlatforms = stdenv.lib.platforms.none;
   }
