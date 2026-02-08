{ mkDerivation, base, blaze-html, ihp-context, ihp-hsx, lib, text
, vault, wai
}:
mkDerivation {
  pname = "ihp-modal";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html ihp-context ihp-hsx text vault wai
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Modal dialog support for IHP applications";
  license = lib.licenses.mit;
}
