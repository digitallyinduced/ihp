{ mkDerivation, base, bytestring, Cabal-syntax, containers
     , data-default, Diff, directory, extra, fail, filepath, free, ghc
     , ghc-boot, ghc-paths, HUnit, mtl, ordered-containers, silently
     , syb, lib
     }:
     mkDerivation {
       pname = "ghc-exactprint";
       version = "1.8.0.0";
       sha256 = "10j98rnn69wig6xks1x5xq19730225ksz3il93x8zniddsn40v8v";
       isLibrary = true;
       isExecutable = true;
       libraryHaskellDepends = [
         base bytestring containers data-default directory fail filepath
         free ghc ghc-boot mtl ordered-containers syb
       ];
       testHaskellDepends = [
         base bytestring Cabal-syntax containers data-default Diff directory
         extra fail filepath ghc ghc-boot ghc-paths HUnit mtl
         ordered-containers silently syb
       ];
       description = "ExactPrint for GHC";
       license = lib.licenses.bsd3;
     }