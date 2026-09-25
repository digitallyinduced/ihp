# Changelog for `ihp-sitemap`

## v1.6.0

- **Breaking:** `renderXmlSitemap` now returns `IO ResponseReceived` and uses
  `?request` / `?respond`, matching the framework-wide response handling
  change where response helpers return the WAI response token directly.

## v1.5.0

- No user-facing changes since 1.4.0
