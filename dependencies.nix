{ mkDerivation, base, exceptions, generic-lens, hspec, hspec-core
, lens, optparse-applicative, pocketsphinx, QuickCheck
, quickcheck-classes, regex-tdfa, shelly, sphinxbase, stdenv
, system-filepath, temporary, text, time, unliftio-core
}:
mkDerivation {
  pname = "cut-the-crap";
  version = "1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative regex-tdfa
    shelly system-filepath temporary text time unliftio-core
  ];
  libraryPkgconfigDepends = [ pocketsphinx sphinxbase ];
  executableHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative regex-tdfa
    shelly system-filepath temporary text time unliftio-core
  ];
  executablePkgconfigDepends = [ pocketsphinx sphinxbase ];
  testHaskellDepends = [
    base exceptions generic-lens hspec hspec-core lens
    optparse-applicative QuickCheck quickcheck-classes regex-tdfa
    shelly system-filepath temporary text time unliftio-core
  ];
  testPkgconfigDepends = [ pocketsphinx sphinxbase ];
  description = "Cuts out uninteresting parts of videos by detecting silences";
  license = stdenv.lib.licenses.mit;
}
