let
	pkgs = import ./pkgs.nix;
	ghc = pkgs.haskell.packages.ghc883;
	haskellDeps = ghc.ghcWithPackages (p: with p; [
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
	    template-haskell
	    haskell-src-meta
	    random-strings
	    concurrent-extra
	    interpolate
	    uri-encode
	    websockets
	    wai-websockets
	    mime-mail
	    mime-mail-ses
	    temporary
	    tz
	    attoparsec
	    lens
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
	    countable-inflections
	    typerep-map
	    mmark-cli
	]);
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../.;
        buildInputs = [haskellDeps];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }