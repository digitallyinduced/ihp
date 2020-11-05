{ mkDerivation, base, base16-bytestring, base64-bytestring
, byteable, bytestring, case-insensitive, conduit, cryptohash
, http-client, http-client-tls, http-conduit, http-types, mime-mail
, optparse-applicative, stdenv, tasty, tasty-hunit, text, time
, xml-conduit, xml-types
}:
mkDerivation {
  pname = "mime-mail-ses";
  version = "0.4.3";
  sha256 = "0v4b0y28kf7mx80z16j82wmaccpggkc262f7cn9g9j2nfayy2xhj";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring base64-bytestring byteable bytestring
    case-insensitive conduit cryptohash http-client http-client-tls
    http-conduit http-types mime-mail text time xml-conduit xml-types
  ];
  executableHaskellDepends = [
    base http-client http-client-tls mime-mail optparse-applicative
    text
  ];
  testHaskellDepends = [
    base bytestring case-insensitive tasty tasty-hunit time
  ];
  homepage = "http://github.com/snoyberg/mime-mail";
  description = "Send mime-mail messages via Amazon SES";
  license = stdenv.lib.licenses.mit;
}