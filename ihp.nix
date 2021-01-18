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
, newtype-generics
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
, ip
}:
mkDerivation {
  pname = "ihp";
  version = "v0.8.0";
  src = (import <nixpkgs> { }).nix-gitignore.gitignoreSource [ ] ./.;
  isLibrary = true;
  isExecutable = true;
  allowInconsistentDependencies = true;
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
    wai-middleware-static
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
    newtype-generics
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
    ip
  ];
  license = stdenv.lib.licenses.mit;
  postInstall = ''
    cp exe/IHP/CLI/run-script $out/bin/run-script

    mkdir -p $out/lib/IHP
    cp -r lib/IHP/* lib/IHP/.hie-bios $out/lib/IHP
  '';
  enableLibraryForGhci = true;
  homepage = "https://ihp.digitallyinduced.com";

  # For faster builds when hacking on IHP:
  # Uncommenting will build without optimizations
  # configureFlags = [ "--flag FastBuild" ];
  # Uncommenting will not generate documentation
  # doHaddock = false;
}
