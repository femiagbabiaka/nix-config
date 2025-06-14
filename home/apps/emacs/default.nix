{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}:
let
  myEmacsAttrs = pkgs.emacs-git-pgtk.overrideAttrs (previousAttrs: {
    buildInputs = previousAttrs.buildInputs ++ [
      pkgs.tree-sitter
      pkgs.jansson
    ];
    patches =
      (previousAttrs.patches or [ ])
      # Only add the patches when condition is true
      ++ (
        if pkgs.stdenv.isDarwin then
          [
            # Fix OS window role (needed for window managers like yabai)
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-31/round-undecorated-frame.patch";
              sha256 = "WWLg7xUqSa656JnzyUJTfxqyYB/4MCAiiiZUjMOqjuY=";
            })
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-31/system-appearance.patch";
              sha256 = "4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
            })
          ]
        else
          [ ]
      );
  });

  myEmacs = (
    myEmacsAttrs.override {
      withNativeCompilation = false;
    }
  );
in
{
  programs.emacs = {
    enable = true;
    package = myEmacs;
    extraConfig = ''
      (org-babel-load-file "${homeDirectory}/.emacs.d/configuration.org")
    '';
  };

  home.file.".emacs.d/early-init.el".text = ''
    ;;; -*- lexical-binding: t -*-
    (setq package-enable-at-startup nil)
  '';
  home.file.".emacs.d/configuration.org".source =
    config.lib.file.mkOutOfStoreSymlink ./configuration.org;
  services.emacs.enable = lib.mkIf pkgs.stdenv.isLinux true;
  home.file."Library/LaunchAgents/gnu.emacs.daemon.plist".text =
    if pkgs.stdenv.isDarwin then
      ''
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
      ''
    else
      "";
}
