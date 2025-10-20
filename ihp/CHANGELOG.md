# unreleased
- [Removed option: 'WORKING_DIRECTORY' no longer required on deployment static assets wrapped automatically](https://github.com/digitallyinduced/ihp/commit/9c54588d983d74bdc28b063d1132952deb57dc16)

# v1.4.0 (unreleased)
## Highlights
- 🚀 Improved Dev Server: faster, more reliable reloads without race conditions.
- 📦 Ecosystem split: IHP now modular (`ihp-ide`, `ihp-ssc`, `ihp-migrate`, `ihp-sitemap`).
- 🔧 New helpers: `createRecordDiscardResult`, `copyRecord`, `queryUnionList`, `orderByAscJoinedTable`.
- 🛠️ Production-ready: warp + systemd integration, healthcheck endpoint, better job runner handling.
- 🐘 Compiler upgrade: GHC 9.8 (with support for 9.10 and 9.12).

---

## Major Changes
- [Added `createRecordDiscardResult`](https://github.com/digitallyinduced/ihp/commit/c7cbbef728d96f85493494afba8da15d6bf96e70) (complements `updateRecordDiscardResult`) for cases where the result is not needed (e.g. scheduling jobs).
- [Added `copyRecord`](https://github.com/digitallyinduced/ihp/commit/06e5e30ebe1d9b95d5cbebb3e04513e33ab607e2) for easier duplication of database records.
- [Improved Dev Server](https://github.com/digitallyinduced/ihp/pull/2050) reliability and performance. Race conditions on rapid file updates are fixed by using proper bracketed resource management.
- Split IHP into multiple packages ([`ihp-ide`](https://github.com/digitallyinduced/ihp/commit/6b21f550d8a6d843ee85c09150b8e2ba587df248), [`ihp-ssc`](https://github.com/digitallyinduced/ihp/commit/50fbe028cb2ef5e3646ce5ab59077be68f5a0df1), [`ihp-migrate`](https://github.com/digitallyinduced/ihp/commit/de021bb3161c8be384a10571963470956aac753c), [`ihp-sitemap`](https://github.com/digitallyinduced/ihp/commit/996e2431ce7a0e74bbb920cfed6ce7d53f668f58)) for cleaner dependency boundaries.
- Production improvements: [integrated `warp-systemd`](https://github.com/digitallyinduced/ihp/commit/d0cf5c6fafa151f3052b39d15f2691f487cefeb4), [added healthcheck endpoint](https://github.com/digitallyinduced/ihp/commit/7b685e696c8f7474ac20cd1737525f4bd50566a6), and [improved socket activation](https://github.com/digitallyinduced/ihp/commit/250da5a3d569afdd048b135d8e0f7db8de55f719).
- [Upgraded to GHC 9.8](https://github.com/digitallyinduced/ihp/commit/a1b0ebb429ae5e95278405be4611b74365e795bb), with support for [9.10.x and 9.12.x](https://github.com/digitallyinduced/ihp/commit/5611f77306c076c2a8c2a89b3efcf3afd54fd7fd).

## Minor Changes

### Nix / Deployment
- [Added options: `rtsFlags`, `optimizationLevel`, `appName`](https://github.com/digitallyinduced/ihp/commit/5718a7cdfb3984958ce78ed9c958ce8bf686a7c2).
- [Support for aarch64 NixOS deployments](https://github.com/digitallyinduced/ihp/commit/fa107a604511d383bddf163211b406aa6f28ff5b).
- [Faster `nix build` via caching of `Generated.Types`](https://github.com/digitallyinduced/ihp/commit/b5e5cdafc4d0980c333db0afcafaf038be4e2eee).
- [Simplified migration service (`IHP_MIGRATION_DIR`)](https://github.com/digitallyinduced/ihp/commit/5684d1a214c2bd1c34fc314f7a4bd3265dd8b4c8).
- [Dropped dependency on `ihp` source](https://github.com/digitallyinduced/ihp/commit/d93f07e0bc6aa38e58f3219ce109ef73d1157460) to reduce closure size.
- [Updated nixpkgs to 25.05 and devenv to 1.8.2](https://github.com/digitallyinduced/ihp/commit/42060df96e10ab31c5b56b07cf9db15646235964).
- [Use flake overlays instead of manual `mkGhcCompiler`](https://github.com/digitallyinduced/ihp/commit/03675cea869226fdb4e46acea1f99391bd94af22).
- [Don’t set `system.stateVersion`](https://github.com/digitallyinduced/ihp/commit/bc9a676bed1775a6ebfda35b499b7630cbcd5c7d) automatically (configured in project’s `flake.nix`).

### Job Runner
- [Jobs with timeouts are now restarted if workers crash](https://github.com/digitallyinduced/ihp/commit/c33034ca77d44f5e2d4ec7ab652f66b3ed028acb).
- [Improved async handling of jobrunner](https://github.com/digitallyinduced/ihp/commit/11377c503be9fa38ad369d3a52c91d1952c5afb1).
- [Use `ResourceT` for job worker scheduling](https://github.com/digitallyinduced/ihp/commit/30d1a156a25ecf350291c634e7909b19c2dc447f) to avoid leaks.
- [Exceptions are now printed to logs](https://github.com/digitallyinduced/ihp/commit/9de03cd652aa69859eda1c0df8951bd7a99e2f4f) for easier debugging.
- [Don’t log job worker SQL queries](https://github.com/digitallyinduced/ihp/commit/54948a62307b829675e37b49754e324d7f292145).

### DataSync
- Fixes: [arrays double-escaped](https://github.com/digitallyinduced/ihp/commit/ad90fc222159bb07235c3f66afd39e8422ee1049), [connections closed prematurely](https://github.com/digitallyinduced/ihp/commit/611b881d29cdb4be32887c356332510d06951273), [crash with spaces in table names](https://github.com/digitallyinduced/ihp/commit/c37be20011ded07120f4ee6efd75175ee52f7438), [non-snake_case column names](https://github.com/digitallyinduced/ihp/commit/96d41a5d9f68aef9514bd8950e7c91ff4787db03).
- Features: [added `useCount`](https://github.com/digitallyinduced/ihp/commit/9bc16f6ff1c85a2dab1040dedc8a9fce2c56b26), [custom `newRecordBehaviour`](https://github.com/digitallyinduced/ihp/commit/d1613e631f6542d5aef4d021ef8f2f253784e090), [support for event triggers](https://github.com/digitallyinduced/ihp/commit/51d17d24eee0bb50e64c53acb2e15a5d19440974), [React 18 `useSyncExternalStore`](https://github.com/digitallyinduced/ihp/commit/1ffdf28bcf956fda43561d1a4670994c2334f570).
- [Avoid creating `large_pg_notifications` table when not needed](https://github.com/digitallyinduced/ihp/commit/6fc9158647619cf50a8bf2c7ade9f7f1b4aa274c).

### HSX / View Layer
- [Added `uncheckedHsx` and `customHsx`](https://github.com/digitallyinduced/ihp/commit/61bc6a7365f4d545a5a7bd120d5186dfbb65ab36).
- [Lucid2 port and core library split](https://github.com/digitallyinduced/ihp/commit/7649fbd06e373c3564ea920f518902ebaf37659c).
- [Added missing HTML attributes](https://github.com/digitallyinduced/ihp/commit/ba748e871dac4e73b26f66dfc6ad5ede2f48e5c9).
- [Fixed generator issues (brackets, variable references, migrations)](https://github.com/digitallyinduced/ihp/commit/e02a4e38e2475329ec40413d80becb464811b8a1).
- [Use Blaze helpers in HSX](https://github.com/digitallyinduced/ihp/commit/50fa3ce43afb87d93710ed6c59b3894bb5c24634).

### SQL / Query Layer
- [Added `filterWhereInCaseInsensitive`](https://github.com/digitallyinduced/ihp/commit/012c7a3c03437109426c64b7c98189b6d639ca44), [`queryUnionList`](https://github.com/digitallyinduced/ihp/commit/d9fe4048f38812b887edf2614052d2116f0a8741), [`orderByAscJoinedTable` / `orderByDescJoinedTable`](https://github.com/digitallyinduced/ihp/commit/dcabc288485457258fdeb508f3fd8e7c4ea63986).
- [Refactored pagination queries](https://github.com/digitallyinduced/ihp/commit/2cd71cfd23156089975a4a27e4c82b4be5b663b4).
- Logging improvements: [show final SQL](https://github.com/digitallyinduced/ihp/commit/0265137b47702f88641ebb7e01d5c8f366f4be01), [rounded runtimes](https://github.com/digitallyinduced/ihp/commit/1fb65084fdf306978da4b42ba3d86f3a17f4efc2), [simplified log format with icons](https://github.com/digitallyinduced/ihp/commit/903e44d521ac673a6414f4c3ae33a1185e8581eb).

### Storage
- [Support custom storage directory via `IHP_STORAGE_DIR`](https://github.com/digitallyinduced/ihp/commit/2c23415e2fb002a9b0f7d0320cdc45dc3d6af838).
- [Support custom filenames in `storeFileFromPath`](https://github.com/digitallyinduced/ihp/commit/c570fb1b5b0d0365a534a85424c32293c4774504).
- [S3 `storagePrefix` now returns base URL](https://github.com/digitallyinduced/ihp/commit/157c0709dc292f0e55142f57e1e222c247f5fc2a).
- [Fix `createTemporaryDownloadUrlFromPath`](https://github.com/digitallyinduced/ihp/commit/145d3c52a45e97d04f22c264212ac6befd6d6fa3).

### Miscellaneous
- [Support custom base URL in `assetPath`](https://github.com/digitallyinduced/ihp/commit/459e72cbb01fdb337b12f0284e9fc368bf2e0845).
- [Add `withPGListener`](https://github.com/digitallyinduced/ihp/commit/c02be01a43b546ffe3439cd3eb82537dbc8c1611).
- [Always evaluate Blaze HTML](https://github.com/digitallyinduced/ihp/commit/2fe1b63319d35da1cf55b7666273ad1751873583).
- [Added `mainInParentDirectory`](https://github.com/digitallyinduced/ihp/commit/27a696af90efcd0f84260d84805dd2dc753e818c).
- [Enabled warp file cache in production](https://github.com/digitallyinduced/ihp/commit/acbb143cc9eae0ac32fa072dbbc7421191d81c00).
- [Set default gracefulShutdownTimeout](https://github.com/digitallyinduced/ihp/commit/58cc32da1264aeb581d6abd6fa87971acc82160a).
- [Improved schema compiler performance](https://github.com/digitallyinduced/ihp/commit/0a779b798050440c373aa50f70314cf3b4916f69).

## IHP OpenAI
- [Removed deprecated `prompt` field](https://github.com/digitallyinduced/ihp/commit/7d6f039c68a69ac50a3c99b1eafc4b5964c1563f).
- [Added support for tools and tool calls](https://github.com/digitallyinduced/ihp/commit/413b95fec6b31106b5fef902571a8bff7098acf9), [`response_format`](https://github.com/digitallyinduced/ihp/commit/c2aec3302f1a2d31cafaa2129ce88faf0eef7e07), [Anthropic `cache_control`](https://github.com/digitallyinduced/ihp/commit/6604104ae177406737c6594cf56760ebec1f4e1f), [Google Gemini](https://github.com/digitallyinduced/ihp/commit/e22491b10b9749266590758db7a3fe5bb97df98a).
- [Added `extraHeaders`](https://github.com/digitallyinduced/ihp/commit/0b50e7076ec08ec5dcb32887fcbacb72909cbb74), [`finish_reason`](https://github.com/digitallyinduced/ihp/commit/4d1fc1a5242994be542ceac87ceeed05a69f7d5b), [`usage` in streaming responses](https://github.com/digitallyinduced/ihp/commit/6931a682f99ea6471b6a6c11b1e3213961b87449).
- [Changed `maxTokens`, `temperature`, `presencePenalty`, `frequencyPenalty` to `Maybe`](https://github.com/digitallyinduced/ihp/commit/3ccc20e51978e6564e290a3e21a7d61bd4a3576c).
- [Improved error messages](https://github.com/digitallyinduced/ihp/commit/92c774224d6b40350e4efcdb9ee29cdd9df51ad9) and [quota exceeded feedback](https://github.com/digitallyinduced/ihp/commit/807047c6d716942ce0bbcbdd5e5db65c634264ac).
- [Added `reasoningEffort`](https://github.com/digitallyinduced/ihp/commit/f0fea79ce9bbeb0ea9154a48ee6d2bc940fdcb60) and [`parallelToolCalls`](https://github.com/digitallyinduced/ihp/commit/9e3c3983bd3942311ea23eb0e4f8b98247abdc47).
- [`system_fingerprint` is now optional](https://github.com/digitallyinduced/ihp/commit/290c462779adc1343d92f258b4a2073ea6a3ba49).

## Documentation
- [Improved deployment docs](https://github.com/digitallyinduced/ihp/commit/dcf28e67cefe96fc9fb46058dbbb1267225b5f63) (workers, systemd integration, AWS Terraform).
- [Updated guides](https://github.com/digitallyinduced/ihp/commit/d0d3cd0ff710c4a6d11b863b382f0e876c205093) for pagination, htmx, realtime SPAs.
- [Clarified environment differences between dev/prod](https://github.com/digitallyinduced/ihp/commit/56ef57a22462ad41e3f513f8725ec8c2878f2ab3).
- [Added docs for `validateFieldIO`](https://github.com/digitallyinduced/ihp/commit/9c7476d68c489fec454543d1a0d993fa22227172) and [scheduled job execution with `runAt`](https://github.com/digitallyinduced/ihp/commit/0ac5d136a565303d00609f1536b1b5e6a656d858).
- [Removed outdated NixOS installation instructions](https://github.com/digitallyinduced/ihp/commit/de206e94807d3c00cb759ecb5ac0552c52ec9855).
- [Cleaned up Emacs docs](https://github.com/digitallyinduced/ihp/commit/3821f80b3a6abe12cc5bb29893fb32538aa05626).
- [Added troubleshooting notes for EC2 deployments](https://github.com/digitallyinduced/ihp/commit/5ae4b04d0c05e593347b2032a2243f7aefff5dca).

