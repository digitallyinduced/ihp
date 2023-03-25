{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation rec {
  name = "ihp-new";
  src = ./.;
  installPhase = ''
    mkdir -p $out/bin;
    cp bin/ihp-new $out;
    makeWrapper $out/ihp-new $out/bin/ihp-new --prefix PATH ":" "${git}/bin";
  '';
  buildInputs = [ git makeWrapper ];
}