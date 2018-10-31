let
  pkgs = import <nixpkgs> {};
  drv =
    { mkDerivation, base, hspec, htoml
    , taglib, unordered-containers, vector, stdenv
    }:
    mkDerivation {
      pname = "populate-drv";
      version = "1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [ base hspec htoml taglib unordered-containers vector ];
      license = stdenv.lib.licenses.bsd3;
    };
  executable = pkgs.haskellPackages.callPackage drv {};
  populate = pkgs.runCommand "populate" { buildInputs = [ pkgs.makeWrapper ]; } ''
    mkdir -p $out/bin
    cp ${executable}/bin/populate $out/bin/populate
    wrapProgram $out/bin/populate \
      --prefix PATH : ${pkgs.youtube-dl}/bin \
      --prefix PATH : ${pkgs.ffmpeg}/bin
  '';
in
  populate
