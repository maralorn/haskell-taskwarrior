{ pkgs ? import <nixpkgs-master> { } }:

let
  haskellPackages = pkgs.haskellPackages;
  drv = haskellPackages.callCabal2nix "taskwarrior" ./. { };
in {
  taskwarrior = drv;
  shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [ drv ];
    buildInputs = with haskellPackages; [
      hlint
      cabal-install
      brittany
      pkgs.coreutils
      ghcide
    ];
  };
}
