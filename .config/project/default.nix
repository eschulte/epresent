{config, flaky, lib, ...}: {
  project = {
    name = "epresent";
    summary = "Simple presentation mode for Emacs Org-mode";
  };

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git = {
      enable = true;
      ignores = [
        # Compiled
        "*.elc"
        # Packaging
        "/.eldev"
      ];
    };
  };

  ## formatting
  editorconfig.enable = true;
  ## See the file for why this needs to force a different version.
  project.file.".dir-locals.el".source = lib.mkForce ../emacs/.dir-locals.el;
  programs = {
    treefmt = {
      enable = true;
      ## In elisp repos, we prefer Org over Markdown, so we don’t need this
      ## formatter.
      programs.prettier.enable = lib.mkForce false;
    };
    vale = {
      enable = true;
      excludes = [
        "*.el"
        "./.github/settings.yml"
        "./Eldev"
        "./org-mode-unicorn.png"
      ];
      vocab.${config.project.name}.accept = [
        "Eldev"
      ];
    };
  };

  ## CI
  services.garnix = {
    enable = true;
    builds.exclude = [
      # TODO: Remove once garnix-io/garnix#285 is fixed.
      "homeConfigurations.x86_64-darwin-${config.project.name}-example"
    ];
  };
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
      (lib.concatMap flaky.lib.garnixChecks [
        (sys: "check elisp-doctor [${sys}]")
        (sys: "check elisp-lint [${sys}]")
        (sys: "homeConfig ${sys}-${config.project.name}-example")
        (sys: "package default [${sys}]")
        (sys: "package emacs-${config.project.name} [${sys}]")
        ## FIXME: These are duplicated from the base config
        (sys: "check formatter [${sys}]")
        (sys: "devShell default [${sys}]")
      ]);

  ## publishing
  services.flakehub.enable = true;
  services.github.enable = true;
}
