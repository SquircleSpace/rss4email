{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = fn: nixpkgs.lib.genAttrs supportedSystems fn;
      version = "0.0.${self.lastModifiedDate}.${self.shortRev or "dirty"}";
      mkNixpkgs = system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };
      mkRss4email = system: (mkNixpkgs system).rss4email;
    in
      {
        overlay = final: prev: {
          rss4email = final.callPackage (import ./default.nix) { src = self; };
        };

        devShell = forAllSystems mkRss4email;
        defaultApp = forAllSystems mkRss4email;
        defaultPackage = forAllSystems mkRss4email;
        packages = forAllSystems (system: { rss4email = mkRss4email system; });

        nixosModule = {...}: {
          imports = [ ./nixosModule.nix ];
          nixpkgs.overlays = [ self.overlay ];
        };
      };
}
