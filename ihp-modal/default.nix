{ mkDerivation, base, blaze-html, ihp-context, ihp-hsx, lib, text
}:
mkDerivation {
  pname = "ihp-modal";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html ihp-context ihp-hsx text
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Modal dialog support for IHP applications";
  license = lib.licenses.mit;
}
