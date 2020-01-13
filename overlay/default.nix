self: super:
{
  cut-the-crap = super.haskell.lib.overrideCabal (super.haskellPackages.callPackage ./cut-the-crap.nix {}) (drv: {

  buildDepends = [ super.makeWrapper ];
  postInstall = ''
     wrapProgram "$out/bin/cut-the-crap" \
      --prefix PATH ":" "${super.ffmpeg}/bin"
  '';
  });
}
