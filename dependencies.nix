{ mkDerivation, base, exceptions, generic-lens, hpack, hspec
, hspec-core, lens, optparse-applicative, regex-tdfa, shelly
, stdenv, system-filepath, temporary, text, unliftio-core
}:
mkDerivation {
  pname = "cut-the-crap";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative regex-tdfa
    shelly system-filepath temporary text unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative regex-tdfa
    shelly system-filepath temporary text unliftio-core
  ];
  testHaskellDepends = [
    base exceptions generic-lens hspec hspec-core lens
    optparse-applicative regex-tdfa shelly system-filepath temporary
    text unliftio-core
  ];
  prePatch = "hpack";
  description = "Cuts out uninteresting parts of videos by detecting silences";
  license = stdenv.lib.licenses.mit;
}
