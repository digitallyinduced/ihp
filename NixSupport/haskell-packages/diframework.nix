{ mkDerivation
, stdenv
, cabal-install
, base
, classy-prelude
, directory
, free
, string-conversions
, twitch
, warp
, wai
, mtl
, blaze-markup
, wai-extra
, http-types
, blaze-html
, inflections
, text
, postgresql-simple
, wai-middleware-static
, wai-util
, http-conduit
, tagsoup
, http-client
, aeson
, uuid
, jose
, wai-session
, wai-session-clientsession
, clientsession
, pwstore-fast
, parsec
, template-haskell
, haskell-src-meta
, random-strings
, interpolate
, uri-encode
, generic-lens
, lens
, websockets
, wai-websockets
, mime-mail
, mime-mail-ses
, temporary
, tz
, attoparsec
, time_1_9_2
, case-insensitive
, haskell-src-exts
, containers
, http-media
, cookie
, process
, newtype-generics
, unix
, fsnotify
, concurrent-extra
}:
mkDerivation {
  pname = "diframework";
  version = "1.0.28";
  src = /Users/marc/digitallyinduced/playground/Membercard/src/Foundation;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    cabal-install
    base
    classy-prelude
    directory
    free
    string-conversions
    twitch
    warp
    wai
    mtl
    blaze-html
    blaze-markup
    wai-extra
    http-types
    inflections
    text
    postgresql-simple
    wai-middleware-static
    wai-util
    http-conduit
    tagsoup
    http-client
    aeson
    uuid
    jose
    wai-session
    wai-session-clientsession
    clientsession
    pwstore-fast
    parsec
    template-haskell
    haskell-src-meta
    random-strings
    concurrent-extra
    interpolate
    uri-encode
    generic-lens
    websockets
    wai-websockets
    mime-mail
    mime-mail-ses
    temporary
    tz
    attoparsec
    lens
    time_1_9_2
    case-insensitive
    haskell-src-exts
    containers
    http-media
    cookie
    process
    newtype-generics
    unix
    fsnotify
    concurrent-extra
  ];
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    mkdir -p $out/shared
    cp Foundation/ControllerGenerator.hs $out/shared/ControllerGenerator.hs
  '';
}