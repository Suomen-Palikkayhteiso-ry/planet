{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

let
  stable = import inputs.nixpkgs-stable {
    system = pkgs.system;
    config = {
      allowUnfree = true;
    };
  };
in
{
  profiles = {
    full-vim.module = {
      services.devcontainer.enable-vscode = true;
      services.devcontainer.enable-vscode-vim = true;
      services.devcontainer.enable-podman = true;
    };
  };

  # https://devenv.sh/basics/
  env.GREET = "Welcome to the Planet generator!";

  # https://devenv.sh/packages/
  packages = [
    pkgs.gemini-cli
    pkgs.git
    pkgs.nodejs
    pkgs.openssl
    pkgs.pkg-config
    pkgs.treefmt
    pkgs.zlib
    pkgs.entr
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-language-server
    pkgs.elmPackages.elm-format
    pkgs.elm-land
    stable.cabal-install
    stable.fourmolu
    stable.hlint
    stable.haskell.packages.ghc96.haskell-language-server
  ];

  # https://devenv.sh/languages/
  languages.haskell.enable = true;
  languages.haskell.package = stable.haskell.compiler.ghc96;
  languages.haskell.stack.enable = true;
  languages.haskell.languageServer = stable.haskell.packages.ghc96.haskell-language-server;
  languages.elm.enable = true;

  enterShell = ''
    echo $GREET
    git --version
    stack --version
  '';

  dotenv.disableHint = true;

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;
}
