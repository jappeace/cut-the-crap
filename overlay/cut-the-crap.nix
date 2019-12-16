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
    sha256 = "0hmbpd6rsn09gawam6sahdp79nv8480qg6wir5xhd4g494sq78an";
    rev = "7882e7ec94d616f9b25cc121cfba088ed220f032";
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
  license = stdenv.lib.licenses.mit;
}
