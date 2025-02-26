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
          overlays =
            [ inputs.rust-overlay.overlays.default ];
        };

        lib = pkgs.lib;

        cargoNix = import ./Cargo.nix { inherit pkgs; };
      in {
        formatter = pkgs.writeShellApplication {
          name = "format";
          runtimeInputs =
            [ pkgs.rust-bin.stable.latest.default pkgs.nixfmt-classic ];
          text = ''
            set -v
            cargo fmt --all
            find . -name '*.nix' | grep -v Cargo.nix | xargs nixfmt'';
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = let p = pkgs;
          in [
            p.bashInteractive
            p.crate2nix
            (p.rust-bin.stable.latest.default.override {
              extensions = [ "rust-src" "rust-analyzer" ];
            })
          ];

          shellHook = ''
            git rev-parse --is-inside-work-tree > /dev/null && [ -n "$CARGO_TARGET_DIR_PREFIX" ] && \
            export CARGO_TARGET_DIR="$CARGO_TARGET_DIR_PREFIX$(git rev-parse --show-toplevel)"
            exec zsh
          '';
        };

        checks.default = cargoNix.workspaceMembers.egraph.build.override { runTests = true; };

        packages = rec {
          default = egraph;
          egraph = cargoNix.workspaceMembers.egraph.build;
          math = cargoNix.workspaceMembers.math.build;
          mk-report = pkgs.writeShellScriptBin "mk-report" ''
            set -v
            ${lib.getExe pkgs.typst} compile ./doc/report/main.typ report.pdf \
              --font-path ${pkgs.newcomputermodern}
          '';
        };
      });
}
