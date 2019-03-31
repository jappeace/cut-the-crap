{ mkDerivation, base, hpack, servant, stdenv }:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base servant ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base servant ];
  preConfigure = "hpack";
  homepage = "https://github.com/jappeace/template#readme";
  license = stdenv.lib.licenses.mit;
}
