{ mkDerivation, base, exceptions, generic-lens, hpack, hspec
, hspec-core, lens, optparse-applicative, optparse-generic, stdenv
, temporary, text, turtle, unliftio-core
}:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic temporary text turtle unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic temporary text turtle unliftio-core
  ];
  testHaskellDepends = [
    base exceptions generic-lens hspec hspec-core lens
    optparse-applicative optparse-generic temporary text turtle
    unliftio-core
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.mit;
}
