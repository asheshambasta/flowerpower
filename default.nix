{ system ? builtins.currentSystem }:
let

  sources = import ./nix/sources.nix;
  # pkgs = import sources.nixpkgs {};

  # prelude-polysemy = pkgs.haskellPackages.callCabal2nix "prelude-polysemy" sources.prelude-polysemy {};
  # dbstorage-polysemy = pkgs.haskellPackages.callCabal2nix "dbstorage-polysemy" sources.dbstorage-polysemy { inherit prelude-polysemy; };

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

    # inherit dbstorage-polysemy prelude-polysemy;

    bulmex = self.callCabal2nix "bulmex" "${bulmex}/bulmex" { };
    servant-reflex = self.callCabal2nix "servant-reflex" servant-reflex { };
    # prelude-polysemy = import sources.prelude-polysemy;
    prelude-polysemy = self.callCabal2nix "prelude-polysemy" prelude-polysemy {};
    dbstorage-polysemy = self.callCabal2nix "dbstorage-polysemy" dbstorage-polysemy {};

    # composite-opaleye needs composite-base, which needs vinyl.
    # These packages are included as overrides since they're either marked broken
    # or aren't present.
    composite-opaleye = self.callCabal2nix "composite-opaleye" "${composite}/composite-opaleye" {};
    composite-base = self.callCabal2nix "composite-base" "${composite}/composite-base" {};
    vinyl = self.callCabal2nix "vinyl" vinyl {};

    reflex-dom-helpers =
      self.callCabal2nix "reflex-dom-helpers" reflex-dom-helpers { };
  };

})
