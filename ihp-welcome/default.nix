{ mkDerivation, base, blaze-html, blaze-markup, ihp, ihp-hsx, lib
, text
}:
mkDerivation {
  pname = "ihp-welcome";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html blaze-markup ihp ihp-hsx text
  ];
  description = "IHP Welcome Controller";
  license = lib.licenses.mit;
}
