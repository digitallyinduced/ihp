{ mkDerivation, base, blaze-html, bytestring, http-client
, http-client-tls, ihp, lib, mime-mail, mime-mail-ses, network
, smtp-mail, string-conversions, text, typerep-map
}:
mkDerivation {
  pname = "ihp-mail";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html bytestring http-client http-client-tls ihp
    mime-mail mime-mail-ses network smtp-mail string-conversions text
    typerep-map
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Email support for IHP";
  license = lib.licenses.mit;
}
