{ compiler ? "default"
, pkgs ? fix.nixpkgs {}

, overrides ? {}
, fix-src ? ./nix/fix
, fix ? import fix-src fix // overrides
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = pkgs.lib.cleanSourceWith
      { filter = path: type: baseNameOf path != ".nix";
        src = pkgs.lib.cleanSource ./.;
      };
    name = "javaq";
    source-overrides = {
      inherit (fix) hspec-hedgehog jvmhs jvm-binary;
    };
    overrides = hsuper: hself: { };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid haskell-language-server fourmolu ])
    ;
  }
