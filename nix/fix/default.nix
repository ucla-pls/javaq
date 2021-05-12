self:
{
  hspec-hedgehog = import ./hspec-hedgehog.nix;
  nixpkgs = import ./nixpkgs.nix;
  jvmhs = import ./jvmhs.nix;
  jvm-binary = import ./jvm-binary.nix;
}
