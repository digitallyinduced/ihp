{ mkDerivation, base, bytestring, http-client, http-client-tls, ihp
, ihp-hsx, lib, mime-mail, mime-mail-ses, network, smtp-mail
, string-conversions, text, typerep-map
}:
mkDerivation {
  pname = "ihp-mail";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring http-client http-client-tls ihp ihp-hsx mime-mail
    mime-mail-ses network smtp-mail string-conversions text typerep-map
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Email support for IHP";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
