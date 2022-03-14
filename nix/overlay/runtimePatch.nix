/**
This file applies the upstream nixpkgs patch
https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-nix.nix#L733
*/
{pkgs, overrideCabal, addBuildTool, cut-the-crap, self, ...}:

let path = pkgs.lib.makeBinPath [ pkgs.ffmpeg pkgs.youtube-dl];
in overrideCabal (addBuildTool cut-the-crap pkgs.makeWrapper) (_drv: {
postInstall = ''
    wrapProgram $out/bin/cut-the-crap \
    --prefix PATH : "${path}"
'';
})
