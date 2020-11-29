let
  sources = import ./nix/sources.nix;
  defaultPkgs = import sources.nixpkgs { };
in { pkgs ? defaultPkgs, name ? "asheshambasta/flowerpower", tag ? "develop"
, system ? builtins.currentSystem }:

let

  rp = import sources.reflex-platform { inherit system; };
  project = import ./default.nix { inherit system; };

  # However, our docker image is only concerned with the executable; which we then use.
  backend = pkgs.haskell.lib.justStaticExecutables project.ghc.fht-backend;
in pkgs.dockerTools.buildImage {
  inherit name tag;
  fromImageName = "alpine:latest";
  # packages to be included
  contents = [ backend pkgs.cacert ];
  config = {
    Entrypoint = [ "fht-backend" ];
    Cmd = [ "-h" ];
    WorkingDir = "/srv/";
    ExposedPorts = { "3000/tcp" = { }; };
  };
}
