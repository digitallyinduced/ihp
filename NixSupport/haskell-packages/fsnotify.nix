{ mkDerivation, async, base, bytestring, containers, directory
, exceptions, filepath, hfsevents, lib, monad-control, monad-logger
, random, retry, safe-exceptions, sandwich, string-interpolate
, temporary, text, time, unix-compat, unliftio
}:
mkDerivation {
  pname = "fsnotify";
  version = "0.4.3.0";
  sha256 = "0dq0dm4j5f16wpfrgh22001icvknmq4cmjyslakjvkyj7w9yfmnf";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring containers directory filepath hfsevents
    monad-control safe-exceptions text time unix-compat
  ];
  executableHaskellDepends = [
    base directory exceptions filepath monad-logger random retry
    safe-exceptions string-interpolate temporary unix-compat unliftio
  ];
  testHaskellDepends = [
    async base directory exceptions filepath monad-logger random retry
    safe-exceptions sandwich string-interpolate temporary unix-compat
    unliftio
  ];
  homepage = "https://github.com/haskell-fswatch/hfsnotify";
  description = "Cross platform library for file change notification";
  license = lib.licenses.bsd3;
  mainProgram = "example";
}