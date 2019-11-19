{ mkDerivation, base, hpack, hspec, hspec-core, stdenv }:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec hspec-core ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.mit;
}
