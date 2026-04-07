{ mkDerivation, base, bytestring, directory, exceptions, file-io
, filepath, lib, os-string, tasty, tasty-hunit, unix
}:
mkDerivation {
  pname = "temporary-ospath";
  version = "1.3";
  sha256 = "74d0a7d57d9a0a3e775d8186544898f95122ccba017972c14cd3edc14dfb69f2";
  libraryHaskellDepends = [
    base bytestring directory exceptions file-io filepath os-string
    unix
  ];
  testHaskellDepends = [
    base directory file-io filepath os-string tasty tasty-hunit unix
  ];
  description = "Portable temporary file and directory support";
  license = lib.licenses.bsd3;
}
