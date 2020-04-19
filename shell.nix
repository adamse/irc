{ pkgs ? import <nixpkgs> {}
, ghc ? "ghc883"
, inspircdSrc ? "/home/adam/src/inspircd"
, inspircdRun ? "/home/adam/src/inspircd/run"
}:

let
  ghcPkgs = pkgs.haskell.packages.${ghc};
  ghcWithPackages = ghcPkgs.ghcWithPackages (p: with p; [ base async text containers ]);
  ghcVersion = ghcPkgs.ghc.version;
  hie = ''
    cradle:
      direct:
        arguments: ["-package-db ${ghcWithPackages.out}/lib/ghc-${ghcVersion}/package.conf.d"]
  '';
in
pkgs.mkShell {
  INC = "-I${inspircdSrc}/include";
  INSPIRCD_RUN_PATH = "${inspircdRun}";
  GHC_VERSION = ghcVersion;
  buildInputs = [
    ghcWithPackages
    pkgs.gcc
  ];
  shellHook = ''
    echo "${hie}" > hie.yaml
  '';
}