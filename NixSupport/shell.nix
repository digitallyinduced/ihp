let
	pkgs = import ./pkgs.nix;
	ghc = pkgs.haskell.packages.ghc865;
	haskellDeps = ghc.ghcWithPackages (p: with p; [
	    cabal-install
	    base
	    classy-prelude
	    directory
	    string-conversions
	    fsnotify
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
	    http-media
	    newtype-generics
	]);
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../../../.;
        buildInputs = [haskellDeps];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }