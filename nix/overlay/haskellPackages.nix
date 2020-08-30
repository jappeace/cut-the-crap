{ pkgs, haskellLib }:

with haskellLib;
self: super:
builtins.intersectAttrs super {
  cut-the-crap = let path = pkgs.stdenv.lib.makeBinPath [ pkgs.ffmpeg ];
  in overrideCabal (addBuildTool super.cut-the-crap pkgs.makeWrapper) (_drv: {
    libraryToolDepends = [ self.c2hs ];
    postInstall = ''
      wrapProgram $out/bin/cut-the-crap \
      --prefix PATH : "${path}"
    '';
  });
}
