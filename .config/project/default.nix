{
  config,
  flaky,
  lib,
  supportedSystems,
  ...
}: {
  project = {
    name = "epresent";
    summary = "Simple presentation mode for Emacs Org-mode";
  };

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    git.enable = true;
  };

  ## formatting
  editorconfig.enable = true;
  programs = {
    treefmt.enable = true;
    vale = {
      enable = true;
      excludes = ["./org-mode-unicorn.png"];
    };
  };

  ## CI
  services.garnix.enable = true;

  ## publishing
  services.flakehub.enable = true;
  services.github.enable = true;
}
