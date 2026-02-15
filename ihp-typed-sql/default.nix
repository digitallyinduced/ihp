direnv: loading ~/digitallyinduced/ihp4/.envrc
direnv: using flake . --override-input devenv-root file+file:///Users/marc/digitallyinduced/ihp4/.devenv/root
direnv: nix-direnv: Using cached dev shell
Running tasks     devenv:enterShell

Running           devenv:enterShell
Succeeded         devenv:enterShell (16.27ms)
1 Succeeded
{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, haskell-src-meta, hasql, hasql-dynamic-statements
, hasql-pool, hspec, ihp, ihp-postgresql-simple-extra, ip, lib
, postgresql-libpq, process, scientific, string-conversions
, template-haskell, temporary, text
}:
mkDerivation {
  pname = "ihp-typed-sql";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers haskell-src-meta hasql
    hasql-dynamic-statements hasql-pool ihp ihp-postgresql-simple-extra
    ip postgresql-libpq scientific string-conversions template-haskell
    text
  ];
  testHaskellDepends = [
    base directory filepath hspec ihp process string-conversions
    temporary text
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Compile-time typed SQL quasiquoter for IHP";
  license = lib.licenses.mit;
}
