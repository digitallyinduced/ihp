# Changelog for `ihp-datasync-typescript`

## v1.6.0

- Convert the DataSync JavaScript client sources to TypeScript
- Add generated table/new-record registries for type-safe schema augmentation
- Type DataSync events with `DataSyncEventMap`
- Tighten public API generics around table names, record IDs, optimistic deletes,
  and pending record creation

## v1.5.0

- Add PostgreSQL table inheritance (`INHERITS`) support in TypeScript codegen

## v1.4.0

- Initial release as a standalone package
