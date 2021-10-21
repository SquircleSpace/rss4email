{ stdenv, haskellPackages, ... }:
let
  ghc = haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    feed
    req
    aeson
    optparse-applicative
    filepath
    aeson-pretty
  ]);
in
stdenv.mkDerivation rec {
  name = "rss4email-${version}";
  version = "1.0.0";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    ${ghc}/bin/ghc -o rss4email Main.hs
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp rss4email $out/bin
  '';
}
