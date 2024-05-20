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
, fsnotify
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
, cereal-text
, neat-interpolation
, unagi-chan
, with-utf8_1_1_0_0
, ihp-hsx
, ihp-postgresql-simple-extra
, ihp
, makeWrapper
}:
mkDerivation {
  pname = "ihp-ide";
  version = "v1.3.0";
  src = ./../../ihp-ide;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
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
    fsnotify
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
    with-utf8_1_1_0_0
    ihp-hsx
    ihp-postgresql-simple-extra
    ihp
  ];
  buildDepends = [ makeWrapper ];
  license = lib.licenses.mit;
  postInstall = ''
    cp exe/IHP/CLI/run-script $out/bin/run-script

    mkdir -p $out/lib/IHP
    cp -r lib/IHP/* lib/IHP/.hie-bios $out/lib/IHP
    cp -r ${ihp}/lib/IHP/static/* $out/lib/IHP/static # B.c. for Makefile
  '';
  postFixup = ''
    wrapProgram $out/bin/RunDevServer --set TOOLSERVER_STATIC "$out/lib/IHP/static" --set IHP_STATIC "${ihp}/lib/IHP/static"
  '';
  homepage = "https://ihp.digitallyinduced.com";

  # For faster builds when hacking on IHP:
  # Uncommenting will build without optimizations
  configureFlags = [ "--flag FastBuild" ];
  # Uncommenting will not generate documentation
  doHaddock = false;
}
