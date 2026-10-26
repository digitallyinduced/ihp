{ pkgs, ihp }:
let
  compiler = pkgs.ghc.ihp-schema-compiler;
  compare = ../ihp-schema-compiler/Test/CompareCompilers.py;
in pkgs.runCommand "ihp-schema-compiler-regressions" {
  nativeBuildInputs = [
    pkgs.python3
    (pkgs.ghc.ghcWithPackages (p: [ p.ihp-schema-compiler p.hspec ]))
  ];
  passthru.benchmark = pkgs.writeShellScriptBin "schema-compiler-benchmark" ''
    if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
      echo 'Usage: schema-compiler-benchmark BASELINE_BINARY SCHEMA_SQL [RUNS]' >&2
      exit 2
    fi
    exec ${pkgs.python3}/bin/python3 ${compare} \
      "$1" ${compiler}/bin/build-generated-code "$2" --runs "''${3:-5}"
  '';
} ''
  # Compare cached generation with fresh generation, then exercise invalidation
  # for both relation modes. Benchmarking two revisions is a separate CLI.
  python3 ${compare} ${compiler}/bin/build-generated-code ${compiler}/bin/build-generated-code
  ghci -v0 -ignore-dot-ghci -fbyte-code -i -i${ihp}/ihp-ide/Test \
    -XGHC2021 -XOverloadedStrings -XNoImplicitPrelude -XImplicitParams \
    -XDisambiguateRecordFields -XDuplicateRecordFields -XOverloadedLabels \
    -XDataKinds -XQuasiQuotes -XTypeFamilies -XPackageImports \
    -XRecordWildCards -XBlockArguments -XLambdaCase -XOverloadedRecordDot \
    ${ihp}/ihp-ide/Test/SchemaCompilerSpec.hs \
    -e 'Test.Hspec.hspec (SchemaCompilerSpec.tests >> IDE.SchemaDesigner.ParserSpec.tests)'
  touch "$out"
''
