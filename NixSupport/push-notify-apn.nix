{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
     , containers, data-default, http2, http2-client
     , optparse-applicative, random, text, time, tls, x509, x509-store
     , stdenv
     }:
     mkDerivation {
       pname = "push-notify-apn";
       version = "0.1.0.8";
       sha256 = "1077sgc2fhaf99y7p09638bx3w1xlayg2jsf0sf2g85rc6j971nd";
       isLibrary = true;
       isExecutable = true;
       libraryHaskellDepends = [
         aeson base base16-bytestring binary bytestring containers
         data-default http2 http2-client random text time tls x509
         x509-store
       ];
       executableHaskellDepends = [
         base bytestring optparse-applicative text
       ];
       testHaskellDepends = [ base ];
       description = "Send push notifications to mobile iOS devices";
       license = stdenv.lib.licenses.bsd3;
     }