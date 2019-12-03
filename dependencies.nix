{ mkDerivation, base, exceptions, foldl, generic-lens, hpack, hspec
, hspec-core, lens, optparse-applicative, optparse-generic
, regex-tdfa, stdenv, template, temporary, text, turtle
, unliftio-core
}:
mkDerivation {
  pname = "cut-the-crap";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions foldl generic-lens lens optparse-applicative
    optparse-generic regex-tdfa temporary text turtle unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base exceptions foldl generic-lens lens optparse-applicative
    optparse-generic regex-tdfa template temporary text turtle
    unliftio-core
  ];
  testHaskellDepends = [
    base exceptions foldl generic-lens hspec hspec-core lens
    optparse-applicative optparse-generic regex-tdfa temporary text
    turtle unliftio-core
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.mit;
}
