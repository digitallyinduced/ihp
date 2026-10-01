{ mkDerivation, base, blaze-builder, bytestring, bytestring-builder
, containers, cookie, entropy, http-types, lib, StateVar, time
, transformers, vault, wai
}:
mkDerivation {
  pname = "wai-session-maybe";
  version = "1.0.0";
  sha256 = "cb68f815b7d04e4e41a102344af3f0f6067297e2a711869f3554b79446d5682d";
  libraryHaskellDepends = [
    base blaze-builder bytestring bytestring-builder containers cookie
    entropy http-types StateVar time transformers vault wai
  ];
  homepage = "https://github.com/digitallyinduced/wai-session-maybe";
  description = "Flexible session middleware for WAI";
  license = "unknown";
}
