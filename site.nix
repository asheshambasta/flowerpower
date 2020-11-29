{ system ? builtins.currentSystem
  , name ? "fht-frontend"
}:
let 
  sources = import ./nix/sources.nix;
  rp = import sources.reflex-platform { 
    inherit system;
  };
  project = import ./default.nix { inherit system; };
  pkgs = rp.nixpkgs;
  # The frontend app
  app = pkgs.lib.getAttr name project.ghcjs; # pkgs.haskell.lib.dontHaddock (pkgs.lib.getAttr name project.ghcjs);
  appStaticFiles = "${app}/bin/${name}.jsexe";
in 
  pkgs.runCommand "${name}-site" {} ''

    # The original all.js is pretty huge; so let's run it by the closure
    # compiler.    
    # cp ${app}/bin/${name}.jsexe/all.js $out/

    mkdir $out

    ${pkgs.closurecompiler}/bin/closure-compiler \
        --externs=${appStaticFiles}/all.js.externs \
        --jscomp_off=checkVars \
        --js_output_file="$out/all.js" \
        -O ADVANCED \
        -W QUIET \
        ${appStaticFiles}/all.js

    echo "LS ${appStaticFiles}"
    ls ${appStaticFiles}
    echo "CP ${appStaticFiles}/* $out"
    cp -n ${appStaticFiles}/* $out/
  ''
