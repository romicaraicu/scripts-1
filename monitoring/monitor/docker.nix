{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  main =
    haskell.lib.dontCheck
      (haskell.lib.justStaticExecutables
        (haskellPackages.callPackage (import ./default.nix {})));
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
      mkdir /checks
    '';
    config = {
      Cmd = [ "${main}/bin/monitor" ];
      ExposedPorts = {
        "3000/tcp" = {};
      };
      WorkingDir = "/checks";
      Volumes = {
        "/checks" = {};
      };
    };
  }
