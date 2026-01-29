{ mkDerivation, base, bytestring, cereal, cereal-text, lib, text
, vault, wai, wai-session
}:
mkDerivation {
  pname = "wai-flash-messages";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal cereal-text text vault wai wai-session
  ];
  description = "Flash messages for wai apps";
  license = lib.licenses.mit;
}
