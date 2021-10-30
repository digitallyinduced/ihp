{ mkDerivation, aeson, base, bytestring, directory
, ghc-syntax-highlighter, gitrev, lib, lucid, megaparsec, mmark
, mmark-ext, optparse-applicative, stache, text
, unordered-containers
, fetchFromGitHub
}:
mkDerivation {
  pname = "mmark-cli";
  version = "0.0.5.0";
  sha256 = "7ec1e69f4ce0ed638f8a979f0da2e3173d2c034ffd23b9b166a95317b0b81997";
  revision = "5";
  editedCabalFile = "1ncyh564gk6fhirx97jnr6v2nw3k69kngxd3gbn9wbi6hm6zz238";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory ghc-syntax-highlighter gitrev lucid
    megaparsec mmark mmark-ext optparse-applicative stache text
    unordered-containers
  ];
  homepage = "https://github.com/mmark-md/mmark-cli";
  description = "Command line interface to the MMark markdown processor";
  license = lib.licenses.bsd3;

  src = fetchFromGitHub {
    owner = "mmark-md";
    repo = "mmark-cli";
    rev = "8892efde05f4cbdf67cdf21cf17b2f2a2412b1a1";
    sha256 = "sha256-TEN0rcLNuSB+0KgmrnIFX8u6tLawFi+XVs+vFI4oruQ";
  };
}