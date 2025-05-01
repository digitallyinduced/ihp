{ mkDerivation
, callPackage
, fetchFromGitHub
, lib
, cabal-install
, base
, classy-prelude
, directory
, string-conversions
, warp
, warp-systemd
, wai
, mtl
, blaze-markup
, wai-extra
, http-types
, blaze-html
, inflections
, text
, postgresql-simple
, wai-app-static
, wai-util
, aeson
, uuid
, wai-session
, wai-session-clientsession
, clientsession
, pwstore-fast
, template-haskell
, haskell-src-meta
, random-strings
, interpolate
, uri-encode
, websockets
, wai-websockets
, mime-mail
, mime-mail-ses
, smtp-mail
, attoparsec
, case-insensitive
, http-media
, cookie
, process
, unix
, countable-inflections
, typerep-map
, basic-prelude
, data-default
, regex-tdfa
, resource-pool
, wreq
, deepseq
, parser-combinators
, fast-logger
, minio-hs
, temporary
, wai-cors
, lens
, random
, hspec
, html-conduit
, cereal-text
, neat-interpolation
, unagi-chan
, with-utf8
, ihp-hsx
, ihp-postgresql-simple-extra
, safe-exceptions
, nix-gitignore
, filter
, resourcet
}:
mkDerivation {
  pname = "ihp";
  version = "v1.3.0";
  src = filter { root = ./.; include = ["IHP" "ihp.cabal" "LICENSE" "lib"]; };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    classy-prelude
    directory
    string-conversions
    warp
    warp-systemd
    wai
    mtl
    blaze-html
    blaze-markup
    wai-extra
    http-types
    inflections
    text
    postgresql-simple
    wai-app-static
    wai-util
    aeson
    uuid
    wai-session
    wai-session-clientsession
    clientsession
    pwstore-fast
    template-haskell
    haskell-src-meta
    random-strings
    interpolate
    websockets
    wai-websockets
    mime-mail
    mime-mail-ses
    smtp-mail
    attoparsec
    case-insensitive
    http-media
    cookie
    process
    unix
    countable-inflections
    typerep-map
    basic-prelude
    data-default
    regex-tdfa
    resource-pool
    wreq
    deepseq
    uri-encode
    parser-combinators
    fast-logger
    minio-hs
    temporary
    wai-cors
    lens
    random
    hspec
    cereal-text
    neat-interpolation
    unagi-chan
    with-utf8
    ihp-hsx
    ihp-postgresql-simple-extra
    safe-exceptions
    resourcet
  ];
  license = lib.licenses.mit;
  enableLibraryForGhci = true;
  homepage = "https://ihp.digitallyinduced.com";

  postInstall = ''
    mkdir -p $out/lib/IHP
    cp -r lib/IHP/* $out/lib/IHP
  '';

  # For faster builds when hacking on IHP:
  # Uncommenting will build without optimizations
  # configureFlags = [ "--flag FastBuild" ];
  # Uncommenting will not generate documentation
  # doHaddock = false;
}
