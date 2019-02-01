with import <nixpkgs> {};

let
  hie_remote = builtins.fetchTarball {
    url    = https://github.com/domenkozar/hie-nix/tarball/master;
    # "https://github.com/NixOS/nixpkgs/archive/3389f23412877913b9d22a58dfb241684653d7e9.tar.gz";
    # sha256 = "0wgm7sk9fca38a50hrsqwz6q79z35gqgb9nw80xz7pfdr4jy9pf7";
  };
  #  haskellPackages.hie

  # todo make it automatic depending on nixpkgs' ghc
  hie = (import hie_remote {} ).hie86;

in
haskellPackages.shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    # netlink-pm 
    (import ./. )
  ];
  withHoogle = true;
  # haskellPackages.stack 
  nativeBuildInputs = [ 
    hie 
    haskellPackages.cabal-install 
    # haskellPackages.bytestring-conversion
    haskellPackages.gutenhasktags
    haskellPackages.haskdogs # seems to build on hasktags/ recursively import things
    haskellPackages.hasktags

    # for https://hackage.haskell.org/package/bytestring-conversion-0.2/candidate/docs/Data-ByteString-Conversion-From.html
  ];

  # export HIE_HOOGLE_DATABASE=$NIX_GHC_DOCDIR as DOCDIR doesn't exist it won't work
  shellHook = ''
    # check if it's still needed ?
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    # export runghc=" "
    function rundaemon() {
      sudo setcap cap_net_admin+ep hs/dist-newstyle/build/x86_64-linux/ghc-8.6.3/netlink-pm-1.0.0/x/daemon/build/daemon/daemon
    } 
  '';
}
