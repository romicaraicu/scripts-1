{ stdenv }:

stdenv.mkDerivation {
  name = "checks";
  src = ../checks;
  phases = [ "patchPhase" "unpackPhase" "installPhase" ];
  patchPhase = ''
    patchShebangs .
  '';
  installPhase = ''
    mkdir -p $out/checks
    cp -r ../checks/* $out/checks/
  '';
}
