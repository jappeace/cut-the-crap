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
    sha256 = "0jznbk1lwlp97b54kdz7lvblal12lsxx9yxd4q2pdzvqdyfa2ipr";
    rev = "dd083b02ef26f755449f615e29e9d0075ce743b3";
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
