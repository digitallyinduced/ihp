{ mkDerivation, hspec, ihp, ihp-postgres-parser, lib, megaparsec
, neat-interpolation, text, with-utf8
}:
mkDerivation {
  pname = "ihp-datasync-typescript";
  version = "1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ihp ihp-postgres-parser neat-interpolation
  ];
  executableHaskellDepends = [
    ihp ihp-postgres-parser neat-interpolation text with-utf8
  ];
  testHaskellDepends = [
    hspec ihp ihp-postgres-parser megaparsec neat-interpolation
  ];
  description = "TypeScript code generation for IHP DataSync";
  license = lib.licenses.mit;
  mainProgram = "generate-datasync-types";
}
