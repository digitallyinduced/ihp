{ mkDerivation, aeson, async, attoparsec, auto-update, base
, base16-bytestring, base64-bytestring, basic-prelude, blaze-html
, blaze-markup, bytestring, classy-prelude, clientsession
, containers, countable-inflections, cryptohash, data-default
, directory, filepath, fsnotify, hspec, http-types, ihp, ihp-hsx
, ihp-log, ihp-migrate, ihp-modal, ihp-postgres-parser
, ihp-schema-compiler, inflections, interpolate, lib, megaparsec
, mono-traversable, neat-interpolation, network, network-uri
, postgresql-simple, process, safe-exceptions, split
, string-conversions, text, time, transformers, unagi-chan, unix
, unliftio, uri-encode, uuid, vault, wai, wai-app-static
, wai-asset-path, wai-extra, wai-request-params, wai-session
, wai-session-clientsession, wai-util, wai-websockets, warp
, websockets, with-utf8, wreq
}:
mkDerivation {
  pname = "ihp-ide";
  version = "1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson async attoparsec auto-update base base16-bytestring
    basic-prelude blaze-html blaze-markup bytestring classy-prelude
    clientsession containers countable-inflections cryptohash
    data-default directory filepath fsnotify http-types ihp ihp-hsx
    ihp-log ihp-migrate ihp-modal ihp-postgres-parser
    ihp-schema-compiler inflections interpolate megaparsec
    mono-traversable neat-interpolation network network-uri
    postgresql-simple process safe-exceptions split string-conversions
    text time transformers unagi-chan unix unliftio uri-encode uuid
    vault wai wai-app-static wai-extra wai-request-params wai-session
    wai-session-clientsession wai-util wai-websockets warp websockets
    with-utf8 wreq
  ];
  executableHaskellDepends = [
    aeson async attoparsec auto-update base base16-bytestring
    base64-bytestring basic-prelude blaze-html blaze-markup bytestring
    classy-prelude clientsession containers countable-inflections
    cryptohash data-default directory filepath fsnotify http-types ihp
    ihp-hsx ihp-log ihp-migrate ihp-postgres-parser ihp-schema-compiler
    inflections interpolate megaparsec mono-traversable
    neat-interpolation network network-uri postgresql-simple process
    safe-exceptions split string-conversions text time transformers
    unagi-chan unix unliftio uri-encode uuid vault wai wai-app-static
    wai-extra wai-session wai-session-clientsession wai-util
    wai-websockets warp websockets with-utf8 wreq
  ];
  testHaskellDepends = [
    aeson async attoparsec auto-update base base16-bytestring
    basic-prelude blaze-html blaze-markup bytestring classy-prelude
    clientsession containers countable-inflections cryptohash
    data-default directory filepath fsnotify hspec http-types ihp
    ihp-hsx ihp-log ihp-migrate ihp-modal ihp-postgres-parser
    ihp-schema-compiler inflections interpolate megaparsec
    mono-traversable neat-interpolation network network-uri
    postgresql-simple process safe-exceptions split string-conversions
    text time transformers unagi-chan unix unliftio uri-encode uuid
    vault wai wai-app-static wai-asset-path wai-extra
    wai-request-params wai-session wai-session-clientsession wai-util
    wai-websockets warp websockets with-utf8 wreq
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Dev tools for IHP";
  license = lib.licenses.mit;
}
