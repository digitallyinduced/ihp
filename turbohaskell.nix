{ mkDerivation
, callPackage
, fetchFromGitHub
, stdenv
, cabal-install
, base
, classy-prelude
, directory
, string-conversions
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
, http-client
, aeson
, uuid
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
, websockets
, wai-websockets
, mime-mail
, mime-mail-ses
, tz
, attoparsec
, time_1_9_3
, case-insensitive
, haskell-src-exts
, containers
, http-media
, cookie
, process
, newtype-generics
, unix
, fsnotify
, countable-inflections
, typerep-map
, basic-prelude
}:
mkDerivation {
  pname = "turbohaskell";
  version = "1.0.28";
  src =
    let
        gitignore = callPackage (fetchFromGitHub {
            owner = "siers";
            repo = "nix-gitignore";
            rev = "bf7c0f0461e047bec108a5c5d5d1b144289a65ba";
            sha256 = "0x55qgirpkzm0yagyqqxi8l7yc3g20bx42iayz124n09cz7sp7mp";
        }) {};
    in
        gitignore.gitignoreSource [] ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    cabal-install
    base
    classy-prelude
    directory
    string-conversions
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
    http-client
    aeson
    uuid
    wai-session
    wai-session-clientsession
    clientsession
    pwstore-fast
    parsec
    template-haskell
    haskell-src-meta
    random-strings
    interpolate
    uri-encode
    websockets
    wai-websockets
    mime-mail
    mime-mail-ses
    tz
    attoparsec
    time_1_9_3
    case-insensitive
    haskell-src-exts
    containers
    http-media
    cookie
    process
    newtype-generics
    unix
    fsnotify
    countable-inflections
    typerep-map
    basic-prelude
  ];
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    mkdir -p $out/shared
    cp TurboHaskell/ControllerGenerator.hs $out/shared/ControllerGenerator.hs
    cp gen/controller $out/bin/new-controller
    cp run-task $out/bin/run-task
  '';
}