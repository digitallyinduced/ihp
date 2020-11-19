let
	pkgs = import ./NixSupport/pkgs.nix;
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
	    interpolate
	    uri-encode
	    websockets
	    wai-websockets
	    mime-mail
	    mime-mail-ses
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
	    mmark-cli
	    data-default
	    hspec
	    regex-tdfa
	    resource-pool
	    wreq
	    githash
    ]);
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../.;
        buildInputs = [haskellDeps pkgs.entr];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }
