# "borrowed" from https://github.com/brendanzab/ocaml-flake-example/blob/main/flake.nix
{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = legacyPackages.lib;

        ezcurl = ocamlPackages.buildDunePackage {
          pname = "ezcurl";
          version = "0.2.4";
          duneVersion = "3";

          src = builtins.fetchGit {
            url = "https://github.com/c-cube/ezcurl.git";
            ref = "refs/tags/v0.2.4";
            rev = "d259d85eb773794267b5314e517c0309bdbb6131";
          };

          buildInputs = [ ocamlPackages.ocurl ];
          propagatedBuildInputs = [ ocamlPackages.ocurl ];
        };

        ezcurl-lwt = ocamlPackages.buildDunePackage {
          pname = "ezcurl-lwt";
          duneVersion = "3";
          inherit (ezcurl) version src;

          buildInputs = [
            ocamlPackages.ocurl
            ocamlPackages.lwt
          ];
          propagatedBuildInputs = [
            ocamlPackages.ocurl
            ocamlPackages.lwt
            ezcurl
          ];
        };

        sources = {
          ocaml = nix-filter.lib {
            root = ./.;
            include = [
              ".ocamlformat"
              "dune-project"
              (nix-filter.lib.inDirectory "lib")
              (nix-filter.lib.inDirectory "test")
            ];
          };

          nix = nix-filter.lib {
            root = ./.;
            include = [
              (nix-filter.lib.matchExt "nix")
            ];
          };
        };
      in
      {
        formatter = legacyPackages.nixpkgs-fmt;
        packages = {
          default = ocamlPackages.buildDunePackage {
            pname = "botocaml";
            version = "0.1.0";
            duneVersion = "3";
            src = sources.ocaml;

            buildInputs = [
              ocamlPackages.angstrom
              ocamlPackages.cohttp
              ocamlPackages.lwt
              ocamlPackages.mirage-crypto
              ocamlPackages.ocurl
              ocamlPackages.timedesc
              ocamlPackages.uri
              ocamlPackages.yojson
              ocamlPackages.ppx_deriving
              ocamlPackages.ppx_yojson_conv
              ocamlPackages.ppx_fields_conv
              ocamlPackages.ppx_variants_conv
              ocamlPackages.ppx_string

              ezcurl
              ezcurl-lwt
            ];

            strictDeps = true;

            preBuild = "dune build botocaml.opam";
          };
        };

        #TODO: 
        #  - Add tests
        #  - Auto formatting

        devShells = {
          default = legacyPackages.mkShell {
            packages = [
              legacyPackages.ocamlformat
              legacyPackages.fswatch
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat-rpc-lib
              ocamlPackages.utop
              ocamlPackages.alcotest
            ];

            inputsFrom = [ self.packages.${system}.default ];
          };
        };
      }
    );

}
