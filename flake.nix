{
  description = "Containers that may not be empty, by construction";

  inputs = {
    nixpkgs.url       = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url   = "github:sixears/flake-build-utils/r1.0.0.12";

    base1.url         = "github:sixears/base1/r0.0.9.20";
    more-unicode.url  = "github:sixears/more-unicode/r0.0.17.9";
    tasty-plus.url    = "github:sixears/tasty-plus/r1.5.2.16";
  };

  outputs = { self, nixpkgs, build-utils
            , base1, more-unicode, tasty-plus }:
    build-utils.lib.hOutputs self nixpkgs "non-empty-containers" {
      deps = {
        inherit base1 more-unicode tasty-plus;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
