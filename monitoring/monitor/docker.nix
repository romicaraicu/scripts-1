{ pkgs ? import <nixpkgs> {} }:

with pkgs;
dockerTools.buildImage {
  name = "monitor";
  contents = [
    (import ./default.nix {})
    bash
    coreutils
    jq
    curl
  ];
  runAsRoot = ''
    #!${stdenv.shell}
    ${dockerTools.shadowSetup}
    mkdir /checks
  '';
  config = {
    Cmd = [ "monitor" ];
    ExposedPorts = {
      "3000/tcp" = {};
    };
    WorkingDir = "/checks";
    Volumes = {
      "/checks" = {};
    };
  };
}
