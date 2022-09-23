# Haskell interface for Wallet Connect JS library

Currently only supports the wallet side operations.

### Build process for JS Bundle used in reflex bindings

The generated JS is included in js dir

This was created using this branch https://github.com/dfordivam/walletconnect-monorepo/tree/dn-wc-2.0.0-rc.2-kadena

$ nvm use v16.14.0
$ lerna bootstrap -- --production 
$ npm run build --production

The generated `packages/sign-client/dist/index.umd.js` was then edited manually to change `global` to `globalThis`
