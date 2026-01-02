{ pkgs, lib, config, inputs, ... }:

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
    pkgs.git
    pkgs.nodejs
    pkgs.zlib
    pkgs.openssl
    pkgs.pkg-config
    pkgs.cabal-install
  ];

  # https://devenv.sh/languages/
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc96;
  languages.haskell.stack.enable = true;
  languages.haskell.languageServer = null;

  enterShell = ''
    echo $GREET
    git --version
    stack --version
  '';

  dotenv.disableHint = true;

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;
}
