{ mkDerivation, base, lib, transformers }:
mkDerivation {
  pname = "unliftio-core";
  version = "0.2.1.0";
  sha256 = "1qz3gxcq1x8fjgq6fqsnws5vgkgbjcx332p3hldxdnaninx4qf4r";
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO";
  license = lib.licenses.mit;
  jailbreak = true;
}