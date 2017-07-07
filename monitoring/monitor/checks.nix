{ stdenv }:

stdenv.mkDerivation {
  name = "checks";
  src = ../checks;
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/checks
    cp -r ../checks/* $out/checks/
  '';
}
