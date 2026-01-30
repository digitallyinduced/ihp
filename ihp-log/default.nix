{ mkDerivation, base, bytestring, data-default, fast-logger
, filepath, lib, text, wai, wai-extra
}:
mkDerivation {
  pname = "ihp-log";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring data-default fast-logger filepath text wai
    wai-extra
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Lightweight logging for IHP applications";
  license = lib.licenses.mit;
}
