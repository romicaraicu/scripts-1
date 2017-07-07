{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  main =
    haskell.lib.dontCheck
      (haskell.lib.justStaticExecutables (import ./default.nix {}));
in
  dockerTools.buildImage {
    name = "monitor";
    tag = "latest";
    contents = [
      main
      bash
      coreutils
      jq
      curl
    ];
    runAsRoot = ''
      #!${stdenv.shell}
      ${dockerTools.shadowSetup}
      mkdir -p /checks
    '';
    config = {
      Env = [
        "DOCKER_HOST="
        "RABBITMQ_ADDRESS="
        "RABBITMQ_CREDS="
      ];
      WorkingDir = "/checks";
      ExposedPorts = {
        "3000/tcp" = {};
      };
      Volumes = {
        "/checks" = {};
      };
      Cmd = [ "${main}/bin/monitor" ];
    };
  }
