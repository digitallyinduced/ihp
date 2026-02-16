{ mkDerivation, aeson, async, attoparsec, base, basic-prelude
, binary, blaze-html, blaze-markup, bytestring, case-insensitive
, cereal, cereal-text, classy-prelude, clientsession, conduit-extra
, containers, contravariant, cookie, countable-inflections
, data-default, deepseq, directory, fast-logger, filepath, ghc-prim
, hashable, haskell-src-exts, haskell-src-meta, hasql
, hasql-dynamic-statements, hasql-implicits, hasql-mapping
, hasql-pool, hasql-postgresql-types, hasql-transaction, hspec, http-client
, http-client-tls, http-media, http-types, ihp-context, ihp-hsx
, ihp-imagemagick, ihp-log, ihp-modal, ihp-pagehead, ihp-pglistener
, inflections, interpolate, lens
, lib, mime-types, minio-hs, mono-traversable, mtl
, neat-interpolation, network, network-uri, parser-combinators
, postgresql-libpq, postgresql-simple, postgresql-types
, postgresql-types-algebra, process, pwstore-fast
, random, random-strings, regex-tdfa, resource-pool, resourcet
, safe-exceptions, scientific, slugger, split, stm
, string-conversions, tagged, template-haskell, temporary, text
, text-builder, time
, transformers, typerep-map, unagi-chan, unix, unliftio
, unordered-containers, uri-encode, uuid, vault, vector, wai
, wai-app-static, wai-asset-path, wai-cors, wai-extra
, wai-flash-messages, wai-request-params, wai-session
, wai-session-clientsession, wai-util, wai-websockets, warp
, warp-systemd, websockets, with-utf8, wreq
}:
mkDerivation {
  pname = "ihp";
  version = "1.4.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson async attoparsec base basic-prelude binary blaze-html
    blaze-markup bytestring case-insensitive cereal cereal-text
    classy-prelude clientsession conduit-extra containers contravariant
    cookie countable-inflections data-default deepseq directory
    fast-logger filepath ghc-prim hashable haskell-src-exts
    haskell-src-meta hasql hasql-dynamic-statements hasql-implicits
    hasql-mapping hasql-pool hasql-postgresql-types hasql-transaction
    hspec http-client http-client-tls http-media http-types ihp-context
    ihp-hsx ihp-imagemagick ihp-log ihp-modal ihp-pagehead
    ihp-pglistener inflections interpolate
    lens mime-types minio-hs mono-traversable mtl neat-interpolation
    network network-uri parser-combinators postgresql-libpq
    postgresql-simple
    postgresql-types postgresql-types-algebra process pwstore-fast
    random random-strings
    regex-tdfa resource-pool resourcet safe-exceptions scientific
    slugger split stm string-conversions tagged template-haskell
    temporary text text-builder time transformers typerep-map
    unagi-chan unix unliftio
    unordered-containers uri-encode uuid vault vector wai
    wai-app-static wai-asset-path wai-cors wai-extra wai-flash-messages
    wai-request-params wai-session wai-session-clientsession wai-util
    wai-websockets warp warp-systemd websockets with-utf8
    wreq
  ];
  testHaskellDepends = [
    aeson async attoparsec base basic-prelude binary blaze-html
    blaze-markup bytestring case-insensitive cereal cereal-text
    classy-prelude clientsession conduit-extra containers contravariant
    cookie countable-inflections data-default deepseq directory
    fast-logger filepath ghc-prim hashable haskell-src-exts
    haskell-src-meta hasql hasql-dynamic-statements hasql-implicits
    hasql-mapping hasql-pool hasql-postgresql-types hasql-transaction
    hspec http-client http-client-tls http-media http-types ihp-context
    ihp-hsx ihp-imagemagick ihp-log ihp-modal ihp-pagehead
    ihp-pglistener inflections interpolate
    lens mime-types minio-hs mono-traversable mtl neat-interpolation
    network network-uri parser-combinators postgresql-libpq
    postgresql-simple
    postgresql-types postgresql-types-algebra process pwstore-fast
    random random-strings
    regex-tdfa resource-pool resourcet safe-exceptions scientific
    slugger split stm string-conversions tagged template-haskell
    temporary text text-builder time transformers typerep-map
    unagi-chan unix unliftio
    unordered-containers uri-encode uuid vault vector wai
    wai-app-static wai-asset-path wai-cors wai-extra wai-flash-messages
    wai-request-params wai-session wai-session-clientsession wai-util
    wai-websockets warp warp-systemd websockets with-utf8
    wreq
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Haskell Web Framework";
  license = lib.licenses.mit;
}
