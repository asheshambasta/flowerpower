let
  sources = import ./nix/sources.nix;
  defaultPkgs = import sources.nixpkgs { };
in { pkgs ? defaultPkgs, name ? "asheshambasta/flowerpower", tag ? "develop"
, system ? builtins.currentSystem, optimiseFrontend ? true }:

let

  rp = import sources.reflex-platform { inherit system; };
  project = import ./default.nix { inherit system; };

  # However, our docker image is only concerned with the executable; which we then use.
  backend = pkgs.haskell.lib.justStaticExecutables project.ghc.fht-backend;

  frontend = if optimiseFrontend then rec {
    result = import ./site.nix {};
    staticFiles = "${result}";
  } else rec {
    result = import project.ghcjs.fht-frontend;
    staticFiles = "${result}/bin/fht-frontend.jsexe";
  };
  # frontend = if optimiseFrontend then import ./site.nix else project.ghcjs.fht-frontend;
  # frontendStaticFiles = if optimiseFrontend then "${frontend}" else "${frontend}/bin/fht-frontend.jsexe";

  varWww = "/var/www";
in pkgs.dockerTools.buildImage {
  inherit name tag;
  fromImageName = "alpine:latest";
  # packages to be included
  contents = [ backend pkgs.cacert ];
  runAsRoot = ''
    mkdir /var
    echo "LS fht-frontend: ${frontend.staticFiles}"
    ls ${frontend.staticFiles}
    echo "CP fht-frontend: ${frontend.staticFiles} to ${varWww}"
    cp -r ${frontend.staticFiles} ${varWww}
  '';
  config = {
    Entrypoint = [ "fht-backend" ];
    Cmd = [ "-h" ];
    WorkingDir = "/srv/";
    ExposedPorts = { "3000/tcp" = { }; };
  };
}
