{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}: let
  myEmacs29 = pkgs.emacs29.overrideAttrs (finalAttrs: previousAttrs: {
    buildInputs = previousAttrs.buildInputs ++ [pkgs.tree-sitter];
  });
in {
  programs.emacs = {
    enable = true;
    package = myEmacs29;
    extraConfig = ''
      (org-babel-load-file "${homeDirectory}/.emacs.d/configuration.org")
    '';
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setq package-enable-at-startup nil)
  '';
  home.file.".emacs.d/configuration.org".source =
    config.lib.file.mkOutOfStoreSymlink ./configuration.org;
  home.file."Library/LaunchAgents/gnu.emacs.daemon.plist".text = ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
        "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
        <dict>
        <key>Label</key>
        <string>gnu.emacs.daemon</string>
        <key>ProgramArguments</key>
        <array>
            <string>${config.programs.emacs.finalPackage}/Applications/Emacs.app/Contents/MacOS/Emacs</string>
            <string>--daemon</string>
        </array>
        <key>RunAtLoad</key>
        <true/>
        <key>ServiceDescription</key>
        <string>Gnu Emacs Daemon</string>
        </dict>
    </plist>
  '';
}
