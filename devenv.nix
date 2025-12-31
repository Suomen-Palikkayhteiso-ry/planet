{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "Welcome to the Planet generator!";

  # https://devenv.sh/packages/
  packages = [ 
    pkgs.git 
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

  # https://devenv.sh/scripts/
  scripts.build-site.exec = "cabal run";
  
  enterShell = ''
    echo $GREET
    git --version
    stack --version
  '';

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;
}
