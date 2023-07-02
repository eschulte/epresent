{
  description = "Simple presentation mode for Emacs Org-mode";

  nixConfig = {
    ## https://github.com/NixOS/rfcs/blob/master/rfcs/0045-deprecate-url-syntax.md
    extra-experimental-features = ["no-url-literals"];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    extra-trusted-substituters = ["https://cache.garnix.io"];
    ## Isolate the build.
    registries = false;
    sandbox = true;
  };

  outputs = inputs: let
    pname = "epresent";
    ename = "emacs-${pname}";
  in
    {
      overlays = {
        default = final: prev: {
          emacsPackagesFor = emacs:
            (prev.emacsPackagesFor emacs).overrideScope'
            (inputs.self.overlays.emacs final prev);
        };

        emacs = final: prev: efinal: eprev: {
          "${pname}" = inputs.self.packages.${final.system}.${ename};
        };
      };

      homeConfigurations =
        builtins.listToAttrs
        (builtins.map
          (system: {
            name = "${system}-example";
            value = inputs.home-manager.lib.homeManagerConfiguration {
              pkgs = import inputs.nixpkgs {
                inherit system;
                overlays = [inputs.self.overlays.default];
              };

              modules = [
                ./nix/home-manager-example.nix
                {
                  # These attributes are simply required by home-manager.
                  home = {
                    homeDirectory = /tmp/${ename}-example;
                    stateVersion = "23.05";
                    username = "${ename}-example-user";
                  };
                }
              ];
            };
          })
          inputs.flake-utils.lib.defaultSystems);
    }
    // inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [(import ./nix/dependencies.nix)];
      };

      emacsPath = package: "${package}/share/emacs/site-lisp/elpa/${package.pname}-${package.version}";

      ## Read version in format: ;; Version: xx.yy
      readVersion = fp:
        builtins.elemAt
        (builtins.match
          ".*(;; Version: ([[:digit:]]+\.[[:digit:]]+(\.[[:digit:]]+)?)).*"
          (builtins.readFile fp))
        1;

      src = pkgs.lib.cleanSource ./.;

      ## We need to tell Eldev where to find its Emacs package.
      ELDEV_LOCAL = emacsPath pkgs.emacsPackages.eldev;
    in {
      packages = {
        default = inputs.self.packages.${system}.${ename};

        "${ename}" =
          inputs.bash-strict-mode.lib.checkedDrv pkgs
          (pkgs.emacsPackages.trivialBuild {
            inherit ELDEV_LOCAL pname src;

            version = readVersion ./${pname}.el;

            nativeBuildInputs = [
              (pkgs.emacsWithPackages (epkgs: [
                epkgs.buttercup
                epkgs.evil
              ]))
              # Emacs-lisp build tool, https://doublep.github.io/eldev/
              pkgs.emacsPackages.eldev
            ];

            postPatch = ''
              {
                echo
                echo "(mapcar"
                echo " 'eldev-use-local-dependency"
                echo " '(\"${emacsPath pkgs.emacsPackages.buttercup}\""
                echo "   \"${emacsPath pkgs.emacsPackages.evil}\""
                echo "   \"${emacsPath pkgs.emacsPackages.goto-chg}\"))"
              } >> Eldev
            '';

            doCheck = true;

            checkPhase = ''
              runHook preCheck
              ## TODO: Currently needed to make a temp file in
              ##      `eldev--create-internal-pseudoarchive-descriptor`.
              export HOME="$PWD/fake-home"
              mkdir -p "$HOME"
              eldev --external test
              runHook postCheck
            '';

            doInstallCheck = true;

            installCheckPhase = ''
              runHook preInstallCheck
              eldev --external --packaged test
              runHook postInstallCheck
            '';
          });
      };

      devShells.default =
        inputs.bash-strict-mode.lib.checkedDrv pkgs
        (pkgs.mkShell {
          inputsFrom =
            builtins.attrValues inputs.self.checks.${system}
            ++ builtins.attrValues inputs.self.packages.${system};

          nativeBuildInputs = [
            # Nix language server,
            # https://github.com/oxalica/nil#readme
            pkgs.nil
            # Bash language server,
            # https://github.com/bash-lsp/bash-language-server#readme
            pkgs.nodePackages.bash-language-server
          ];
        });

      checks = {
        doctor =
          inputs.bash-strict-mode.lib.checkedDrv pkgs
          (pkgs.stdenv.mkDerivation {
            inherit ELDEV_LOCAL src;

            name = "eldev-doctor";

            nativeBuildInputs = [
              pkgs.emacs
              # Emacs-lisp build tool, https://doublep.github.io/eldev/
              pkgs.emacsPackages.eldev
            ];

            buildPhase = ''
              runHook preBuild
              ## TODO: Currently needed to make a temp file in
              ##      `eldev--create-internal-pseudoarchive-descriptor`.
              export HOME="$PWD/fake-home"
              mkdir -p "$HOME/.cache/eldev"
              eldev doctor
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              runHook postInstall
            '';
          });

        lint =
          ## TODO: Can’t currently use `inputs.bash-strict-mode.lib.checkedDrv`
          ##       because the `emacs` wrapper script checks for existence of a
          ##       variable with `-n` intead of `-v`.
          inputs.bash-strict-mode.lib.shellchecked pkgs
          (pkgs.stdenv.mkDerivation {
            inherit ELDEV_LOCAL src;

            name = "eldev-lint";

            nativeBuildInputs = [
              pkgs.emacs
              pkgs.emacsPackages.eldev
            ];

            postPatch = ''
              {
                echo
                echo "(mapcar"
                echo " 'eldev-use-local-dependency"
                echo " '(\"${emacsPath pkgs.emacsPackages.dash}\""
                echo "   \"${emacsPath pkgs.emacsPackages.elisp-lint}\""
                echo "   \"${emacsPath pkgs.emacsPackages.evil}\""
                echo "   \"${emacsPath pkgs.emacsPackages.goto-chg}\""
                echo "   \"${emacsPath pkgs.emacsPackages.package-lint}\""
                echo "   \"${emacsPath pkgs.emacsPackages.relint}\""
                echo "   \"${emacsPath pkgs.emacsPackages.xr}\"))"
              } >> Eldev
            '';

            buildPhase = ''
              runHook preBuild
              ## TODO: Currently needed to make a temp file in
              ##      `eldev--create-internal-pseudoarchive-descriptor`.
              export HOME="$PWD/fake-home"
              mkdir -p "$HOME"
              ## Need `--external` here so that we don’t try to download any
              ## package archives (which would break the sandbox).
              eldev --external lint
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              runHook preInstall
            '';
          });

        nix-fmt =
          inputs.bash-strict-mode.lib.checkedDrv pkgs
          (pkgs.stdenv.mkDerivation {
            inherit src;

            name = "nix fmt";

            nativeBuildInputs = [inputs.self.formatter.${system}];

            buildPhase = ''
              runHook preBuild
              alejandra --check .
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              runHook preInstall
            '';
          });
      };

      # Nix code formatter, https://github.com/kamadorueda/alejandra#readme
      formatter = pkgs.alejandra;
    });

  inputs = {
    bash-strict-mode = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:sellout/bash-strict-mode";
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager/release-23.05";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
  };
}
