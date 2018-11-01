let
  pkgs = import <nixpkgs> {};
  drv =
    { mkDerivation, base, directory, hspec, htoml, optparse-applicative
    , parsec, process, stdenv, taglib, text, unordered-containers
    , vector
    }:
    mkDerivation {
      pname = "populate";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        base directory htoml optparse-applicative parsec process taglib
        text unordered-containers vector
      ];
      executableHaskellDepends = [ base ];
      testHaskellDepends = [ base hspec text ];
      homepage = "https://github.com/cronokirby/populate#readme";
      license = stdenv.lib.licenses.mit;
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
