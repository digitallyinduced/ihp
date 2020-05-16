with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "ihp-new";
  src = ./.;
  installPhase = ''
  	mkdir -p $out/bin;
  	cp bin/ihp-new $out/bin;
  '';
}