{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/jappeace/template#readme";
  license = stdenv.lib.licenses.mit;
}
