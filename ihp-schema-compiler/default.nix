{ mkDerivation, base, bytestring, classy-prelude
, countable-inflections, directory, filepath, ihp
, ihp-postgres-parser, interpolate, lib, neat-interpolation, split
, string-conversions, text, with-utf8
}:
mkDerivation {
  pname = "ihp-schema-compiler";
  version = "1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring classy-prelude countable-inflections directory
    filepath ihp ihp-postgres-parser interpolate neat-interpolation
    split string-conversions text with-utf8
  ];
  executableHaskellDepends = [
    base bytestring classy-prelude countable-inflections directory
    filepath ihp ihp-postgres-parser interpolate neat-interpolation
    split string-conversions text with-utf8
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Lightweight schema compiler for IHP";
  license = lib.licenses.mit;
  mainProgram = "build-generated-code";
}
