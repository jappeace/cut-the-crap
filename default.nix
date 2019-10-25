{ mkDerivation, base, exceptions, generic-lens, gogol
, gogol-youtube, hpack, hspec, hspec-core, lens
, optparse-applicative, optparse-generic, stdenv, text, turtle
, unliftio-core
}:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions generic-lens gogol gogol-youtube lens
    optparse-applicative optparse-generic text turtle unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base exceptions generic-lens gogol gogol-youtube lens
    optparse-applicative optparse-generic text turtle unliftio-core
  ];
  testHaskellDepends = [
    base exceptions generic-lens gogol gogol-youtube hspec hspec-core
    lens optparse-applicative optparse-generic text turtle
    unliftio-core
  ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.mit;
}
