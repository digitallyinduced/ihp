with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "turbohaskell-new";
  src = ./.;
  installPhase = ''
  	mkdir -p $out/bin $out/lib;
  	cp bin/turbohaskell-new $out/bin;
  	cp -r boilerplate $out/lib;
  '';
}