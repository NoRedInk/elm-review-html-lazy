let
  sources = import ./nix/sources.nix;
  niv = import sources.niv { };
  pkgs = import sources.nixpkgs {
    localSystem = if builtins.currentSystem == "aarch64-darwin" then
      "x86_64-darwin"
    else
      builtins.currentSystem;
  };
in pkgs.mkShell {
  buildInputs = [
    niv.niv
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-language-server
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-test
  ];
}
