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
, http-conduit
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
  pname = "turbohaskell";
  version = "1.0.28";
  src =
    let
        gitignore = callPackage (fetchFromGitHub {
            owner = "siers";
            repo = "nix-gitignore";
            rev = "d69e4f620ec9d35ddb125ee495f6a5dee519c905";
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
    http-conduit
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
    cp TurboHaskell/ControllerGenerator.hs $out/shared/ControllerGenerator.hs
  '';
}