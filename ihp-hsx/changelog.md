# Changelog for `ihp-hsx`

## Version 1.6.0

- Replace the Blaze `MarkupM` tree with a direct `ByteString.Builder` markup
  backend for faster rendering.
- Support GHC 9.12 and 9.14
- Add `isEmpty` helper
- Add `renderMarkupText` and `renderMarkupLazyText`
- Fix HSX action URLs so `href={MyAction}` renders `pathTo MyAction`
- Restore `ConvertibleStrings` `Text`/`String` `Html` instances for
  compatibility

## Version 1.5.0

- Suport for GHC 9.10.x and 9.12.x
- Add backend for lucid2 via dedidcated ihp-hsx-lucid2 library
- Add missing attributes muted, onkeypress

## Versions < 0.18:

[See the IHP main upgrade instructions](https://github.com/digitallyinduced/ihp/blob/master/UPGRADE.md)
