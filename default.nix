{ mkDerivation, base, generic-lens, hpack, hspec, hspec-core, lens
, optparse-applicative, optparse-generic, stdenv, text, turtle
}:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base generic-lens lens optparse-applicative optparse-generic text
    turtle
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base generic-lens lens optparse-applicative optparse-generic text
    turtle
  ];
  testHaskellDepends = [
    base generic-lens hspec hspec-core lens optparse-applicative
    optparse-generic text turtle
  ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.mit;
}
