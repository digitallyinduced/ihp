{ mkDerivation, base, blaze-html, ihp-context, ihp-hsx, lib, text
, vault, wai
}:
mkDerivation {
  pname = "ihp-pagehead";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html ihp-context ihp-hsx text vault wai
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Page title and meta tags for IHP";
  license = lib.licenses.mit;
}
