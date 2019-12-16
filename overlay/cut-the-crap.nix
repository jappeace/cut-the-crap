{ mkDerivation, base, exceptions, fetchgit, generic-lens, hpack
, hspec, hspec-core, lens, optparse-applicative, optparse-generic
, regex-tdfa, shelly, stdenv, system-filepath, temporary, text
, unliftio-core, ffmpeg, makeWrapper
}:
(mkDerivation {
  pname = "cut-the-crap";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/jappeace/cut-the-crap";
    sha256 = "1sqfd1fjr8y29i47ak6qnq2axbjqvbqgm8mjgapylfnzwrlwyd5h";
    rev = "53d64d359f5973ce08bfcdf5f421a2d752b995ec";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ makeWrapper ];
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
  postInstall = ''
     wrapProgram "$out/bin/cut-the-crap" \
      --prefix PATH ":" "${ffmpeg}/bin"
  '';

})

