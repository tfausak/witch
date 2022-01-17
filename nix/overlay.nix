{
  vimBackground ? "light",
  vimColorScheme ? "PaperColor"
}:
[(self: super:
    let
      callPackage = self.lib.callPackageWith self.haskellPackages;
      dontCheck = self.haskell.lib.dontCheck;
      doJailbreak = self.haskell.lib.doJailbreak;
    in
      {
        haskell-ide = import (
          fetchTarball "https://github.com/21it/ultimate-haskell-ide/tarball/f5aaaddd0a8efcde271cd497c86e300da608c4a8"
          ) {
            formatter = "none";
            inherit vimBackground vimColorScheme;
          };
        haskellPackages = super.haskell.packages.ghc901.extend(
          self': super': {

          }
        );
      }
)]
