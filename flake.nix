{
  description = "Containers that may not be empty, by construction";

  inputs = {
    nixpkgs.url       = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url   = github:sixears/flake-build-utils/r1.0.0.13;

    base1.url         = github:sixears/base1/r0.0.9.32;
    more-unicode.url  = github:sixears/more-unicode/r0.0.17.11;
    tasty-plus.url    = github:sixears/tasty-plus/r1.5.2.22;
  };

  outputs = { self, nixpkgs, build-utils
            , base1, more-unicode, tasty-plus }:
    build-utils.lib.hOutputs self nixpkgs "non-empty-containers" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, containers, deepseq, lens
                    , mono-traversable, QuickCheck, tasty, tasty-hunit
                    , tasty-quickcheck, template-haskell, text, text-printer
                    , unordered-containers
                    }:
        mkDerivation {
          pname = "non-empty-containers";
          version = "1.4.3.34";
          src = ./.;
          libraryHaskellDepends = [
            base base-unicode-symbols containers deepseq lens mono-traversable
            QuickCheck tasty tasty-hunit tasty-quickcheck template-haskell text
            text-printer
            unordered-containers
          ] ++ mapPkg [ base1 more-unicode tasty-plus ];
          testHaskellDepends = [ base tasty ];
          description = "Containers that may not be empty, by construction";
          license = lib.licenses.mit;
        };
    };
}
