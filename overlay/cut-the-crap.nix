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
    sha256 = "1z4s0zan421q4722k32p0n33bsh7di1m6pbbdzqi7j87lfd9i7nk";
    rev = "0ea8a0a0fd3e8d85f44337f13ef2445d22d1bf97";
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

