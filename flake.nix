{
  description = "Simple presentation mode for Emacs Org-mode";

  nixConfig = {
    ## https://github.com/NixOS/rfcs/blob/master/rfcs/0045-deprecate-url-syntax.md
    extra-experimental-features = ["no-url-literals"];
    extra-substituters = ["https://cache.garnix.io"];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    ## Isolate the build.
    sandbox = "relaxed";
    use-registries = false;
  };

  outputs = {
    flake-utils,
    flaky,
    nixpkgs,
    self,
    systems,
  }: let
    pname = "epresent";
    ename = "emacs-${pname}";

    supportedSystems = import systems;
  in
    {
      schemas = {
        inherit
          (flaky.schemas)
          overlays
          homeConfigurations
          packages
          devShells
          projectConfigurations
          checks
          formatter
          ;
      };

      overlays = {
        default = flaky.lib.elisp.overlays.default self.overlays.emacs;

        emacs = final: prev: efinal: eprev: {
          "${pname}" = self.packages.${final.system}.${ename};
        };
      };

      homeConfigurations =
        builtins.listToAttrs
        (builtins.map
          (flaky.lib.homeConfigurations.example self [
            ({pkgs, ...}: {
              programs.emacs = {
                enable = true;
                extraConfig = "(require '${pname})";
                extraPackages = epkgs: [epkgs.${pname}];
              };
            })
          ])
          supportedSystems);
    }
    // flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        flaky.overlays.default
        flaky.overlays.elisp-dependencies
      ];

      src = pkgs.lib.cleanSource ./.;

      emacsDeps = epkgs: [epkgs.evil];
    in {
      packages = {
        default = self.packages.${system}.${ename};
        "${ename}" = pkgs.elisp.package pname src emacsDeps;
      };

      projectConfigurations =
        flaky.lib.projectConfigurations.emacs-lisp {inherit pkgs self;};

      devShells =
        self.projectConfigurations.${system}.devShells
        // {default = flaky.lib.devShells.default system self [] "";};

      checks =
        self.projectConfigurations.${system}.checks
        // {
          elisp-doctor = pkgs.elisp.checks.doctor src;
          elisp-lint = pkgs.elisp.checks.lint src emacsDeps;
        };

      formatter = self.projectConfigurations.${system}.formatter;
    });

  inputs = {
    ## Flaky should generally be the source of truth for its inputs.
    flaky.url = "github:sellout/flaky";

    flake-utils.follows = "flaky/flake-utils";
    nixpkgs.follows = "flaky/nixpkgs";
    systems.follows = "flaky/systems";
  };
}
