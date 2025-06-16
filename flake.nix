{
  inputs = {
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachSystem
    [ inputs.flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.rust-overlay.overlays.default ];
        };

        lib = pkgs.lib;

        rust-stable = pkgs.rust-bin.stable.latest.default;

        cargoNix = import ./Cargo.nix {
          inherit pkgs;
          buildRustCrateForPkgs = crate:
            pkgs.buildRustCrate.override {
              rustc = rust-stable;
              cargo = rust-stable;
            };
        };

        pythonWithLibraries = (pkgs.python3.withPackages
          (py-pkgs: with py-pkgs; [ matplotlib numpy ]));
      in {
        formatter = pkgs.writeShellApplication {
          name = "format";
          runtimeInputs = [ rust-stable pkgs.nixfmt-classic ];
          text = ''
            set -v
            cargo fmt --all
            find . -name '*.nix' | grep -v Cargo.nix | xargs nixfmt'';
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = let p = pkgs;
          in [
            p.bashInteractive
            p.cargo-flamegraph
            p.cargo-show-asm
            p.tinymist
            p.typstyle
            (p.crate2nix.override { cargo = rust-stable; })
            (rust-stable.override {
              extensions = [ "rust-src" "rust-analyzer" ];
            })
          ];

          shellHook = ''
            git rev-parse --is-inside-work-tree > /dev/null && [ -n "$CARGO_TARGET_DIR_PREFIX" ] && \
            export CARGO_TARGET_DIR="$CARGO_TARGET_DIR_PREFIX$(git rev-parse --show-toplevel)"
            exec zsh
          '';
        };

        packages = rec {
          default = oatlog;
          oatlog = cargoNix.workspaceMembers.oatlog.build;
          extract-demo = cargoNix.workspaceMembers.extract-demo.build;
          quadratic-formula = cargoNix.workspaceMembers.quadratic-formula.build;
        };
        apps = builtins.mapAttrs (k: v: {
          type = "app";
          program = "${pkgs.writeShellScript k v}";
        }) {
          mk-report = ''
            set -v -e
            PATH="${pkgs.ripgrep}/bin:$PATH"
            PATH="${pkgs.graphviz}/bin:$PATH"
            PATH="${pythonWithLibraries}/bin:$PATH"
            make -C ./doc/report lint dependencies
            ${
              lib.getExe pkgs.typst
            } compile ./doc/report/main.typ --root . report.pdf \
              --font-path ${pkgs.newcomputermodern}
          '';
        };
      });
}
