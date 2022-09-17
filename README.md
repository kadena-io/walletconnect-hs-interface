# Haskell interface for Wallet Connect JS library

Currently only supports the wallet side operations.

### Build process for JS Bundle used in reflex bindings

``` shell.nix
{ pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    nodePackages.npm
  ];
}
```

```
npm install
npm run bootstrap
npm run build
# Build product now at packages/sign-client/dist/umd/index.min.js
```
