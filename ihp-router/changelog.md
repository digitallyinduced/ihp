# Changelog

## 1.0.0 — 2026-04-25

Initial release. Extracted from `ihp-1.6.0` so that plain WAI applications can use the trie-based router and the `[routes|…|]` DSL without depending on the rest of the IHP framework.

Modules:

- `IHP.Router.Trie` — path-indexed trie data structure + lookup
- `IHP.Router.Middleware` — standard WAI `Middleware` wrapping the trie
- `IHP.Router.Capture` — `UrlCapture` typeclass with base instances (`Text`, `Int`, `Integer`, `UUID`, `Bool`, `Day`, `Segment`)
- `IHP.Router.UrlGenerator` — `HasPath` class for URL generation
- `IHP.Router.DSL.AST` / `Parser` / `Runtime` / `TH` — the `[routes|…|]` quasi-quoter
- `IHP.Router.WAI` — public facade re-exporting the surface plain WAI users need
