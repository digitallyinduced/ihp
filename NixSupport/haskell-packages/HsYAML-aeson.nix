{ mkDerivation, aeson, base, bytestring, containers, HsYAML, mtl
, scientific, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "HsYAML-aeson";
  version = "0.2.0.0";
  sha256 = "12sxww260pc0bbpiyirm7911haxhljdi2f08a9ddpbgw8d5n7ffg";
  libraryHaskellDepends = [
    aeson base bytestring containers HsYAML mtl scientific text
    unordered-containers vector
  ];
  description = "JSON to YAML Adapter";
  license = stdenv.lib.licenses.gpl2Plus;
  jailbreak = true;
}