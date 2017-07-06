{ pkgs ? import <nixpkgs> {} }:

with pkgs;
dockerTools.buildImage {
  name = "monitor";
  contents = [ (import ./shell.nix {}) ];
  config = {
    Cmd = [ "monitor" ];
    ExposedPorts = {
      "3000/tcp" = {};
    };
  };
}
