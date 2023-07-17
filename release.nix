{
  system ? builtins.currentSystem,
  reflex-platform ? import ./nix/reflex-platform {
    inherit system;
  },
}:
let
  pkgs = reflex-platform.pkgs;
  project = (reflex-platform.project ({ pkgs, thunkSource, ... }: {
    name = "jsaddle-project";
    src = ./.;
    compiler-nix-name = "ghc8107Splices";
    ghcjs-compiler-nix-name = "ghcjs8107JSString";
    shells = ps: with ps; ([
      jsaddle
    ] ++ pkgs.lib.optionals (builtins.hasAttr "jsaddle-warp" ps)
      [
        jsaddle-warp
        jsaddle-wkwebview
        jsaddle-webkit2gtk
        jsaddle-clib
      ]);
    shellTools = {
      cabal = "3.8.1.0";
    };
  })).extend (self: super: {
    shells = super.shells // {
      ghc = self.shell-driver {
        crossBuilds = [ ];
        buildInputs = with self.pkgs; [ nodejs ];
      };
    };
  });

  node-client =
    pkgs.buildNpmPackage rec {
      pname = "jsaddle-warp-node-client";
      version = "0.1.0";
      src = ./jsaddle-warp/node-client;
      dontNpmBuild = true;
      npmDepsHash = "sha256-zbJs22Bak93e9QB/GCSBAOfcoBSmXWBOMGy64TOkQDY=";
    };
in {
  inherit project;
  ghc-jsaddle = project.hsPkgs.jsaddle.components.library;
  ghc-jsaddle-warp = project.hsPkgs.jsaddle-warp.components.library;
  # TODO: Fix webkit2gtk and wkwebview builds
  # ghc-jsaddle-webkit2gtk = project.hsPkgs.jsaddle-webkit2gtk.components.library;

  ghcjs-jsaddle = project.crossSystems.ghcjs.hsPkgs.jsaddle.components.library;

  runTests = pkgs.runCommandNoCC "run-tests-jsaddle-warp-spec" {
    nativeBuildInputs = [
      pkgs.nodejs
    ];
    strictDeps = true;
  } ''
    mkdir $out
    cd $out
    mkdir $out/node-client
    ln -s ${node-client}/lib/node_modules/jsaddle-warp-node-client/* $out/node-client/
    ${project.hsPkgs.jsaddle-warp.components.tests.spec}/bin/spec
  '';
}
