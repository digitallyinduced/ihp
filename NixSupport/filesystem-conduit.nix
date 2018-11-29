{ mkDerivation, base, blaze-builder, bytestring, conduit
     , containers, hspec, QuickCheck, system-fileio, system-filepath
     , text, transformers, unix, conduit-extra, stdenv, resourcet
     }:
     mkDerivation {
       pname = "filesystem-conduit";
       version = "1.0.0.2";
       sha256 = "05dsl3bgyjciq6sgmba0hki7imilrjq3ddp9ip5gxl9884j1f4a1";
       libraryHaskellDepends = [
         base bytestring conduit containers system-fileio system-filepath
         text transformers unix conduit-extra resourcet
       ];
       testHaskellDepends = [
         base blaze-builder bytestring conduit hspec QuickCheck text
         transformers conduit-extra
       ];
       preConfigure = ''
       		sed -i -e 's/        conduit -any/        conduit -any,\n        resourcet -any,\n        conduit-extra -any/g' filesystem-conduit.cabal
       		sed -i -e 's/import           Data.Conduit/import           Data.Conduit\nimport           Control.Monad.Trans.Resource\n/g' Data/Conduit/Filesystem.hs
       		sed -i -e 's/Prelude hiding (FilePath)/Prelude hiding (FilePath, traverse)/g' Data/Conduit/Filesystem.hs
       		cat Data/Conduit/Filesystem.hs
       '';
       description = "Use system-filepath data types with conduits. (deprecated)";
       license = stdenv.lib.licenses.mit;
       hydraPlatforms = stdenv.lib.platforms.none;
     }