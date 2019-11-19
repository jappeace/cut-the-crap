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
<<<<<<< HEAD
  executableHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic temporary text turtle unliftio-core
  ];
  testHaskellDepends = [
    base exceptions generic-lens hspec hspec-core lens
    optparse-applicative optparse-generic temporary text turtle
    unliftio-core
  ];
  preConfigure = "hpack";
=======
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec hspec-core ];
  prePatch = "hpack";
>>>>>>> template/master
  license = stdenv.lib.licenses.mit;
}
