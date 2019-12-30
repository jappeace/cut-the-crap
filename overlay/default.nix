self: super:
{
  cut-the-crap = super.haskell.lib.overrideCabal (super.haskellPackages.callPackage ./cut-the-crap.nix {}) (drv: {
  postInstall = ''
     wrapProgram "$out/bin/cut-the-crap" \
      --prefix PATH ":" "${super.ffmpeg}/bin"
  '';
  });
}
