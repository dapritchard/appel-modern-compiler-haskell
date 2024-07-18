{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/336eda0d07dc5e2be1f923990ad9fdb6bc8e28e3.tar.gz") {}
}:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [ ghc haskell-language-server ];
}
