{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix;
  rp = import sources.reflex-platform { inherit system; };
in rp.project ({ pkgs, ... }: {

  name = "flowerpower";

  # uses jsaddle-warp to fire up a warp server instead of webgtk.
  useWarp = true;
  withHoogle = false;

  packages = {
    fht-frontend = ./fht-frontend;
    fht-data = ./fht-data;
    fht-api = ./fht-api;
    fht-backend = ./fht-backend;
  };

  shells = {
    ghc = [ "fht-frontend" "fht-backend" "fht-data" "fht-api" ];
    ghcjs = [ "fht-frontend" "fht-data" "fht-api" ];
  };

  # overrides = self: super: { 
  # inherit (sources) bulmex # reflex-dom-helpers # servant-reflex
  # ;
  # reflex-dom-helpers = self.callCabal2nix "reflex-dom-helpers" ../../reflex-dom-helpers {};
  # servant-reflex = self.callCabal2nix "servant-reflex" ../../servant-reflex {};
  # # bulmex = self.callCabal2nix "bulmex" ../../bulmex/bulmex {};
  # bulmex = self.callCabal2nix "bulmex" "${sources.bulmex}/bulmex" {} ;
  # servant-reflex = self.callPackage ../../servant-reflex {};
  # } 
  overrides = self: super:
  with sources; {
    bulmex = self.callCabal2nix "bulmex" "${bulmex}/bulmex" { };
    servant-reflex = self.callCabal2nix "servant-reflex" servant-reflex { };
    reflex-dom-helpers =
    self.callCabal2nix "reflex-dom-helpers" reflex-dom-helpers { };
  };

})
