{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { } }:
let
  haskellPackages = pkgs.haskell.packages.ghc94;
in
pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "taskwarrior" ./. {}) {
  postPatch = ''
    sed -i "s/-Wall/-Wall -Werror/" taskwarrior.cabal
  '';
  testToolDepends = with pkgs.haskellPackages; [ cabal-install hlint fourmolu ];
  postCheck = ''
    echo "Checking hlint hints …"
    hlint src
    echo "hlint hints okay."

    echo "Checking formatting with fourmolu …"
    fourmolu -m check src/**/*.hs
    echo "Formatting okay."
  '';
  postHaddock = ''
    echo "Checking that documentation is complete …"
    if [[ ! -z $(./Setup haddock | grep "\(is out of scope\|Missing documentation\)") ]]; then
      echo "Detected missing documentation."
      exit 1;
    fi
    echo "Documentation checked."
  '';
}
