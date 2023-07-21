{
  description = "JavaQ - a jvm decompiler";

  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/23.05";
      flake-utils.url = "github:numtide/flake-utils";
      jvm-binary.url = github:ucla-pls/jvm-binary;
      jvmhs.url = github:ucla-pls/jvmhs;
    };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , jvmhs
    , jvm-binary
    }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" ] (system:
    let
      pkgs = (import nixpkgs { inherit system; });
      haskellPackages = pkgs.haskellPackages;
      project = returnShellEnv:
        haskellPackages.developPackage {
          inherit returnShellEnv;
          root = self;
          name = "javaq";
          source-overrides = {
            inherit jvmhs jvm-binary;
          };
          overrides = hsuper: hself: { };
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (with haskellPackages; [ cabal-install ghcid haskell-language-server hpack fourmolu ]);
        };
    in
    {
      defaultPackage = project false;
      devShell = project true;
    });
}
