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
          fetchTarball "https://github.com/21it/ultimate-haskell-ide/tarball/78fb42182fb8bdc3b084b9f81d8e0b0df60dc73a"
          ) {
            formatter = "brittany";
            inherit vimBackground vimColorScheme;
          };
        haskellPackages = super.haskell.packages.ghc901.extend(
          self': super': {

          }
        );
      }
)]
