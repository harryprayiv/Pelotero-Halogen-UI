{
  inputs = {
    #Generic Stuff
    nixpkgs.follows = "purs-nix/nixpkgs";
    # systems.url = "github:nix-systems/default";
    utils.url = "github:ursi/flake-utils";

    # Purescript stuff
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    ps-tools.follows = "purs-nix/ps-tools";

    npmlock2nix = {
      flake = false;
      url = "github:nix-community/npmlock2nix";
    };
  };

  outputs = {
    self,
    utils,
    nixpkgs,
    systems,
    ...
  } @ inputs: let
    name = "pelotero-halogen";
    systems = [
      "x86_64-linux"
      "x86_64-darwin"
    ];
  in
    utils.apply-systems
    {
      inherit inputs systems;
    }
    ({system, ...}: let
      pkgs = nixpkgs.legacyPackages.${system};
      npmlock2nix = (import inputs.npmlock2nix {inherit pkgs;}).v1;
      purs-nix = inputs.purs-nix {inherit system;};
      ps-tools = inputs.ps-tools.legacyPackages.${system};
      ps-command = ps.command {};
      ps =
        purs-nix.purs
        {
          # Project dir (src, test)
          srcs = [
            "src/**/*.purs"
          ];
          # test = "src/purescript/test/**/*.purs";

          # test-module = "Test.Main";
          # Dependencies
          dependencies = with purs-nix.ps-pkgs; [
            prelude
            console
            effect
            halogen
            datetime
            arrays
            either
            node-fs
            node-fs-aff
            node-buffer
            exceptions
            partial
            psci-support
            quickcheck
            aff
            argonaut
          ];

          foreign.Main.node_modules = npmlock2nix.node_modules {src = ./.;} + /node_modules;
        };

      purs-watch = pkgs.writeShellApplication {
        name = "purs-watch";
        runtimeInputs = with pkgs; [entr ps-command];
        text = "find src | entr -s 'echo building && purs-nix compile'";
      };

      vite = pkgs.writeShellApplication {
        name = "vite";
        runtimeInputs = with pkgs; [nodejs];
        text = "npx vite --open";
      };

      purs-dev = pkgs.writeShellApplication {
        name = "purs-dev";
        runtimeInputs = with pkgs; [concurrently];
        text = "concurrently purs-watch vite";
      };
    in rec {
      defaultApp = utils.lib.mkApp {
        type = "app";
        drv = live-server;
      };

      live-server = pkgs.nodePackages.live-server;

      packages = with ps; {
        default = ps.modules.Main.bundle {};
        bundle = bundle {};
        output = output {};
      };
      # bundle.esbuild = {format = "iife";};
      devShells.default =
        pkgs.mkShell
        {
          inherit name;
          packages = with pkgs; [
            ps-command
            ps-tools.for-0_15.purescript-language-server
            ps-tools.for-0_15.purs-tidy
            purs-nix.esbuild
            purs-nix.purescript
            nodejs

            vite
            purs-watch
            purs-dev
          ];
          buildInputs = with pkgs; [
            nodejs

            purs-nix.esbuild
            purs-nix.purescript

            # You can choose pnpm, yarn, or none (npm).
            nodePackages.pnpm
            nodePackages.live-server
          ];
          shellHook = ''
            export NIX_SHELL_NAME="pelotero_Front_End"
            echo "Welcome to the development shell!"
            echo
            echo opening VSCodium for this project....
            # codium .
            echo .
            echo ..
            echo ...
            # purs-nix compile
          '';
        };
      apps = {
        live-server = {
          type = "app";
          program = "${live-server}/bin/live-server";
        };
      };
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-experimental-features = ["nix-command flakes" "ca-derivations"];
    allow-import-from-derivation = "true";
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}
