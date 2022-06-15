{
  description = "A command line tool to generate fractals from an IFS.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "Fractals";

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in {
        packages.${packageName} = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            # Ex with gi-gtk-declarative:
            # If version is broken then:
            # gi-gtk-declarative = jailbreakUnbreak haskeppPackages.gi-gtk-declarative;
            # or if tests failing: 
            # gi-gtk-declarative = pkgs.haskell.lib.dontCheck haskellPackages.gi-gtk-declarative;
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal2nix
            
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.implicit-hie
            # For StateT
            haskellPackages.transformers_0_6_0_4
            # Random library
            haskellPackages.random
            # For nunOrd
            haskellPackages.extra
            # Stuff for plotting
            haskellPackages.Chart
            haskellPackages.Chart-cairo
            haskellPackages.gtk2hs-buildtools
            haskellPackages.gtk
            haskellPackages.colour
            haskellPackages.data-default-class
            haskellPackages.lens
            pkg-config
            cairo
            pango
            gtk2
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
