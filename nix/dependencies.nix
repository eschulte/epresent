final: prev: {
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope' (efinal: eprev: {
      # The `eldev` package doesn’t expose the executable.
      eldev = eprev.eldev.overrideAttrs (old: {
        postInstall = ''
          mkdir -p "$out"
          cp -R "$src/bin" "$out/"
        '';
      });
    });
}
