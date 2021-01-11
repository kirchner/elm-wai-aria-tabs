let
  pkgs = import <nixpkgs> {};
in

with pkgs;

mkShell {
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format
    elmPackages.elm-doc-preview
    elmPackages.elm-language-server
    nodejs
  ];
}
