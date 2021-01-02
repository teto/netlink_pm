{
  description = "A multipath TCP subflow management software.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
        pkgs = nixpkgs.legacyPackages."${system}";
      haskellOverlay = hnew: hold: with pkgs.haskell.lib; {

          ip = unmarkBroken (dontCheck hold.ip);
          bytebuild = unmarkBroken (dontCheck hold.bytebuild);

          # ip = dontCheck overrideSrc hold.ip {
          #   src = prev.fetchFromGitHub {
          #     owner = "andrewthad";
          #     repo = "haskell-ip";
          #     rev = "1.7.2";
          #     # sha256 = 
          #     sha256 = "sha256-OL5KkVsoGAZskfwj8HAq2aBh60F2TW+WnAUfTjiekNc=";
          #     # sha256 = "sha256-QGpMskYOcUR4o/IC30etrTEAkdvevpOOi4chWbEw5Ik=";
          #   };
          # };

          # c2hsc = appendPatch hold.c2hsc (prev.fetchpatch {
          #   url = "https://github.com/jwiegley/c2hsc/commit/490ecab202e0de7fc995eedf744ad3cb408b53cc.patch";
          #   # sha256 = "12g462qmj8c7if797gnyvf8h0cddmm3xy0pjldw48w8f8sr4qsj0";
          #   # sha256 = "sha256-QGpMskYOcUR4o/IC30etrTEAkdvevpOOi4chWbEw5Ik=";
          #   sha256 = "sha256-B0OcZkVrLTBCCBJL+b4vfxSTBDfTT5lKYOie3Pe187A=";
          # });

          # can be released on more recent nixplks
          # wide-word = doJailbreak (hold.wide-word);
          # quickcheck-classes = hold.quickcheck-classes_0_6_4_0;

          # for newer nixpkgs (March 2020)
          # base-compat = doJailbreak (hold.base-compat);
          # time-compat = doJailbreak (hold.time-compat);

          # rebase needs time == 1.9.*
          # time is a core package of ghc
          # time = hold.time_1_10;
          # time = null;
          # rebase = doJailbreak (hold.rebase);

          netlink = (overrideSrc hold.netlink {
            # src = builtins.fetchGit {
            #   # url = https://github.com/ongy/netlink-hs;
            #   url = https://github.com/teto/netlink-hs;
            # };
            src = prev.fetchFromGitHub {
              owner = "teto";
              repo = "netlink-hs";
              rev = "090a48ebdbc35171529c7db1bd420d227c19b76d";
              sha256 = "sha256-qopa1ED4Bqk185b1AXZ32BG2s80SHDSkCODyoZfnft0=";
            };
          });
        };
      # };
    # };
      in rec {

    # haskell.compiler."ghc"
    packages.mptcppm = pkgs.haskellPackages.developPackage {
      root = ./.;
      name = "mptcp-pm";
      returnShellEnv = false;
      withHoogle = true;
      overrides = haskellOverlay;
      modifier = drv:
        pkgs.haskell.lib.addBuildTools drv (with pkgs;
        [
          # ghcid
          haskellPackages.cabal-install
          haskellPackages.c2hs
          haskellPackages.stylish-haskell
          haskellPackages.hlint
          haskellPackages.haskell-language-server
          haskellPackages.hasktags
        ]);
    };

    defaultPackage = packages.mptcppm;
    # devShell = 
  }) // {
    # overlay = ;
  };
}
