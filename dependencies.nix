{ mkDerivation, base, exceptions, generic-lens, hpack, hspec
, hspec-core, lens, optparse-applicative, optparse-generic
, pocketsphinx, regex-tdfa, shelly, sphinxbase, stdenv
, system-filepath, temporary, text, unliftio-core
}:
mkDerivation {
  pname = "cut-the-crap";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic regex-tdfa shelly system-filepath temporary text
    unliftio-core
  ];
  libraryPkgconfigDepends = [ pocketsphinx sphinxbase ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic regex-tdfa shelly system-filepath temporary text
    unliftio-core
  ];
  executablePkgconfigDepends = [ pocketsphinx sphinxbase ];
  testHaskellDepends = [
    base exceptions generic-lens hspec hspec-core lens
    optparse-applicative optparse-generic regex-tdfa shelly
    system-filepath temporary text unliftio-core
  ];
  testPkgconfigDepends = [ pocketsphinx sphinxbase ];
  prePatch = "hpack";
  description = "Cuts out uninteresting parts of videos by detecting silences";
  license = stdenv.lib.licenses.mit;
}
