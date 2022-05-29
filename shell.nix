let
  pkgs = import <nixpkgs> { };

  lambo = import ./default.nix;

in
lambo.env.overrideAttrs (prev: {
  buildInputs = with pkgs; [
    cabal-install
    ghcid
  ];
})
