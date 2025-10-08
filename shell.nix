{ pkgs ? import <nixpkgs> {} }:


pkgs.mkShell {
  buildInputs = [
    pkgs.gcc
    pkgs.avrdude
    pkgs.avra
    pkgs.screen
    pkgs.minicom
#   pkgs.avr-sim
  ];
}
