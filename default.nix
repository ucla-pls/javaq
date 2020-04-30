{ pkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
, jvmhs ? import ./nix/jvmhs.nix 
, jvm-binary ? import ./nix/jvm-binary.nix 
}: 
let 
  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages 
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = pkgs.lib.cleanSourceWith 
      { filter = path: type: ! (pkgs.lib.hasSuffix ".nix" path);
        src = pkgs.lib.cleanSource ./.;
      };
    name = "javaq";
    source-overrides = {
      inherit jvmhs jvm-binary; 
    };
    overrides = hsuper: hself: { };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }
