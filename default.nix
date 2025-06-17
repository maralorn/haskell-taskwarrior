{
  sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs { },
}:
let
  haskellPackages = pkgs.haskellPackages;
in
pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "taskwarrior" ./. { }) {
  postPatch = ''
    sed -i "s/-Wall/-Wall -Werror/" taskwarrior.cabal
  '';
  testToolDepends = builtins.attrValues {
    inherit (pkgs) cabal-install hlint fourmolu;
    inherit (pkgs.haskellPackages) cabal-gild;
  };
  postCheck = ''
    echo "Checking hlint hints …"
    hlint src
    echo "hlint hints okay."

    echo "Checking formatting with fourmolu …"
    fourmolu -m check src/**/*.hs
    echo "Formatting okay."

    echo "Checking formatting with cabal-gild …"
    cabal-gild -m check -i taskwarrior.cabal
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
