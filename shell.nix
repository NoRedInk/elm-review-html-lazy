let
  sources = import ./nix/sources.nix;
  niv = import sources.niv { };
  pkgs = import sources.nixpkgs { };
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
