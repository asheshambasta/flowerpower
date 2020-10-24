{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix;
  rp = import sources.reflex-platform { inherit system; };
in rp.project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = { fht-frontend = ./fht-frontend; 
               fht-data = ./fht-data;
               fht-api = ./fht-api;
               fht-backend = ./fht-backend;
             };
  shells = {
    ghc = [ "fht-frontend" "fht-backend" "fht-data" "fht-api" ] # ++ ( with pkgs; [ inotify-tools ])
    ;
    ghcjs = [ "fht-frontend" ];
  };

  overrides = self: super: { 
    inherit (sources) bulmex reflex-dom-helpers;
  };

})
