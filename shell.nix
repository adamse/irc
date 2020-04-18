{ pkgs ? import <nixpkgs> {}
, ghc ? "ghc883" 
, inspircdSrc ? "/home/adam/src/inspircd"
, inspircdRun ? "/home/adam/src/inspircd/run"
}:

let
  ghcPkgs = pkgs.haskell.packages.${ghc};
  ghcWithPackages = ghcPkgs.ghcWithPackages (p: with p; [ base ]);
  ghcVersion = ghcPkgs.ghc.version;
in
pkgs.mkShell {
  INC = "-I${inspircdSrc}/include";
  INSPIRCD_RUN_PATH = "${inspircdRun}";
  GHC_VERSION = ghcVersion;
  buildInputs = [ 
    ghcWithPackages
    pkgs.gcc
  ];
}