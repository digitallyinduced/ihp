with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "turbohaskell-new";
  src = ./.;
  installPhase = ''
  	mkdir -p $out/bin;
  	cp bin/turbohaskell-new $out/bin;
  '';
}