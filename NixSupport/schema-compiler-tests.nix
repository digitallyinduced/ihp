{ pkgs, ihp }:
let
  compiler = pkgs.ghc.ihp-schema-compiler;
in pkgs.runCommand "ihp-schema-compiler-regressions" {
  nativeBuildInputs = [
    (pkgs.ghc.ghcWithPackages (p: [ p.ihp-schema-compiler p.hspec ]))
  ];
  passthru.benchmark = pkgs.writeShellScriptBin "schema-compiler-benchmark" ''
    if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
      echo 'Usage: schema-compiler-benchmark BASELINE_BINARY SCHEMA_SQL [RUNS]' >&2
      exit 2
    fi
    unset GHCRTS
    exec ${compiler}/bin/compare-schema-compilers \
      "$1" ${compiler}/bin/build-generated-code "$2" "''${3:-5}"
  '';
} ''
  # Cache regressions run in the compiler package's native Hspec test suite.
  # Keep the existing parser and compiler snapshots in this focused check too.
  ghci -v0 -ignore-dot-ghci -fbyte-code -i -i${ihp}/ihp-ide/Test \
    -XGHC2021 -XOverloadedStrings -XNoImplicitPrelude -XImplicitParams \
    -XDisambiguateRecordFields -XDuplicateRecordFields -XOverloadedLabels \
    -XDataKinds -XQuasiQuotes -XTypeFamilies -XPackageImports \
    -XRecordWildCards -XBlockArguments -XLambdaCase -XOverloadedRecordDot \
    ${ihp}/ihp-ide/Test/SchemaCompilerSpec.hs \
    -e 'Test.Hspec.hspec (SchemaCompilerSpec.tests >> IDE.SchemaDesigner.ParserSpec.tests)'
  touch "$out"
''
