let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.dhall
    pkgs.glibc
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ "884" ]; })
    # pkgs.haskellPackages.ghcide
    pkgs.haskellPackages.ormolu
    pkgs.haskellPackages.floskell
    pkgs.haskellPackages.brittany
  ];
  shellHook = ''
  export TEST="IN HERE!"
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH''${LD_LIBRARY_PATH:+:}${pkgs.glibc}/lib
  '';
}
