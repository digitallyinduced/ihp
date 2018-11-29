{ mkDerivation, aeson, attoparsec, base, bytestring, conduit
     , directory, filesystem-conduit, old-locale, random
     , shakespeare-text, shelly, system-filepath, text, time
     , transformers, unordered-containers, uuid, zip-archive
     , shakespeare, resourcet
     , stdenv
     }:
     mkDerivation {
       pname = "hs-pkpass";
       version = "0.4";
       sha256 = "01jcl2ia8p29gg5yazpxm6cdxyskl6z895lmgh888qkf9jlzf5mf";
       libraryHaskellDepends = [
         aeson attoparsec base bytestring conduit directory
         filesystem-conduit old-locale random shakespeare-text shelly
         system-filepath text time transformers unordered-containers uuid
         zip-archive shakespeare resourcet
       ];
       preConfigure = ''
          sed -i -e 's/        transformers -any/        transformers -any,\n        shakespeare -any,\n        conduit-extra -any,\n        resourcet -any/g' hs-pkpass.cabal
          sed -i -e 's/deriveToJSON id/deriveToJSON defaultOptions/g' Passbook/Types.hs
          sed -i -e 's/deriveFromJSON id/deriveFromJSON defaultOptions/g' Passbook/Types.hs
          sed -i -e 's/{-# LANGUAGE TemplateHaskell           #-}/{-# LANGUAGE TemplateHaskell           #-}\n{-# OPTIONS_GHC -fdefer-type-errors           #-}/g' Passbook/Types.hs
          sed -i -e 's/import           System.Locale/import           System.Locale hiding (iso8601DateFormat, timeFmt, defaultTimeLocale)/g' Passbook/Types.hs
          sed -i -e 's/import           Data.Conduit.Filesystem/import           "filesystem-conduit" Data.Conduit.Filesystem\nimport Control.Monad.Trans.Resource\n/g' Passbook.hs
          sed -i -e 's/{-# LANGUAGE OverloadedStrings    #-}/{-# LANGUAGE OverloadedStrings    #-}\n{-# LANGUAGE PackageImports    #-}/g' Passbook.hs
          sed -i -e 's/) rawhash/) (LT.fromStrict rawhash)/g' Passbook.hs
          sed -i -e 's/toTextIgnore $ filename file/LT.fromStrict (toTextIgnore $ filename file)/g' Passbook.hs
          sed -i -e 's/(LT.unpack $ toTextIgnore path)/(ST.unpack $ toTextIgnore path)/g' Passbook.hs
          sed -i -e 's/-> Sh Text/-> Sh ST.Text/g' Passbook.hs
          sed -i -e 's/"pass.json"/("pass.json" :: LT.Text)/g' Passbook.hs
          sed -i -e 's/"manifest.json"/("manifest.json" :: LT.Text)/g' Passbook.hs

          sed -i -e 's/, "-in", ("manifest.json" :: LT.Text)/, "-in", ("manifest.json")/g' Passbook.hs
          sed -i -e 's/findEntryByPath ("pass.json" :: LT.Text)/findEntryByPath "pass.json"/g' Passbook.hs
          sed -i -e 's#passOut </> LT.append uuid ".pkpass"#passOut </> LT.unpack (LT.append uuid ".pkpass")#g' Passbook.hs
          sed -i -e 's#renderPass (tmp </> ("pass.json" :: LT.Text))#renderPass (tmp </> ("pass.json" :: FilePath))#g' Passbook.hs
          sed -i -e 's#saveJSON manifest (tmp </> ("manifest.json" :: LT.Text))#saveJSON manifest (tmp </> ("manifest.json" :: FilePath))#g' Passbook.hs
          sed -i -e 's#(toTextIgnore $ passOut </> passFile)#(toTextIgnore $ passOut </> (LT.unpack passFile))#g' Passbook.hs
          sed -i -e 's#(passOut </> passFile)#(passOut </> (LT.unpack passFile))#g' Passbook.hs
          sed -i -e 's#(passOut </> LT.append lazyId ".pkpass")#(passOut </> LT.unpack (LT.append lazyId ".pkpass"))#g' Passbook.hs
          sed -i -e 's#, "-signer", toTextIgnore cert#, "-signer", toTextIgnore cert, "-certfile", "../../../wwdr.pem"#g' Passbook.hs
          sed -i -e 's/NSDateFormatterNoStyle/PKDateStyleNone/g' Passbook/Types.hs
          sed -i -e 's/NSDateFormatterShortStyle/PKDateStyleShort/g' Passbook/Types.hs
          sed -i -e 's/NSDateFormatterMediumStyle/PKDateStyleMedium/g' Passbook/Types.hs
          sed -i -e 's/NSDateFormatterLongStyle/PKDateStyleLong/g' Passbook/Types.hs
          sed -i -e 's/NSDateFormatterFullStyle/PKDateStyleFull/g' Passbook/Types.hs
       '';
       description = "A library for Passbook pass creation & signing";
       license = stdenv.lib.licenses.bsd3;
       hydraPlatforms = stdenv.lib.platforms.none;
     }