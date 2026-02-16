{ mkDerivation, base, ihp, ihp-mail, hasql-implicits, lib, text, wai }:
mkDerivation {
  pname = "ihp-auth-confirmation";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [ base ihp ihp-mail hasql-implicits text wai ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Email confirmation support for IHP authentication";
  license = lib.licenses.mit;
}
