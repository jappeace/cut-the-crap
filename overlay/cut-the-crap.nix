{ mkDerivation, base, exceptions, fetchgit, generic-lens, hpack
, hspec, hspec-core, lens, optparse-applicative, optparse-generic
, regex-tdfa, shelly, stdenv, system-filepath, temporary, text
, unliftio-core
}:
mkDerivation {
  pname = "cut-the-crap";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/jappeace/cut-the-crap";
    sha256 = "0l294rvlmslvp4c120d4p6aravg22r4pwm0zqhqjj7vj2w4js6h7";
    rev = "2a87fa606e68f0a80cac56d25fe447e6c292620d";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic regex-tdfa shelly system-filepath temporary text
    unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base exceptions generic-lens lens optparse-applicative
    optparse-generic regex-tdfa shelly system-filepath temporary text
    unliftio-core
  ];
  testHaskellDepends = [
    base exceptions generic-lens hspec hspec-core lens
    optparse-applicative optparse-generic regex-tdfa shelly
    system-filepath temporary text unliftio-core
  ];
  prePatch = "hpack";
  description = "Cut the crap is an automatic video editing program for streamers. It can cut out uninteresting parts by detecting silences.";
  license = stdenv.lib.licenses.mit;
}
