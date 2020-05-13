{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, attoparsec, base, basic-prelude
      , blaze-html, blaze-markup, bytestring, case-insensitive
      , classy-prelude, clientsession, containers, cookie
      , countable-inflections, data-default, directory, fsnotify
      , ghc-prim, haskell-src-exts, haskell-src-meta, http-client
      , http-media, http-types, inflections, interpolate, megaparsec, mtl
      , network, network-uri, newtype-generics, parsec, postgresql-simple
      , process, pwstore-fast, random-strings, split, stdenv
      , string-conversions, template-haskell, text, time, typerep-map
      , unix, unliftio, uuid, vault, wai, wai-extra
      , wai-middleware-static, wai-session, wai-session-clientsession
      , wai-util, wai-websockets, warp, websockets
      }:
      mkDerivation {
        pname = "turbohaskell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson async attoparsec base basic-prelude blaze-html blaze-markup
          bytestring case-insensitive classy-prelude clientsession containers
          cookie countable-inflections data-default directory ghc-prim
          haskell-src-exts haskell-src-meta http-client http-media http-types
          inflections interpolate megaparsec mtl network-uri newtype-generics
          parsec postgresql-simple process pwstore-fast random-strings split
          string-conversions template-haskell text time typerep-map uuid
          vault wai wai-extra wai-middleware-static wai-session
          wai-session-clientsession wai-util warp
        ];
        executableHaskellDepends = [
          aeson async attoparsec base basic-prelude blaze-html blaze-markup
          bytestring case-insensitive classy-prelude clientsession containers
          cookie countable-inflections data-default directory fsnotify
          ghc-prim haskell-src-exts haskell-src-meta http-client http-media
          http-types inflections interpolate megaparsec mtl network
          network-uri newtype-generics parsec postgresql-simple process
          pwstore-fast random-strings split string-conversions
          template-haskell text time typerep-map unix unliftio uuid vault wai
          wai-extra wai-middleware-static wai-session
          wai-session-clientsession wai-util wai-websockets warp websockets
        ];
        doHaddock = false;
        description = "Haskell Web Framework";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
