# ihp-datasync-typescript

1. Add `ihp-datasync-typescript` to `flake.nix`
2. Run `generate-datasync-types Application/Schema.sql Frontend/types/ihp-datasync/index.d.ts`

Requires a `typeRoots` in `tsconfig.json` like this:

```json
{
  "compilerOptions": {
    "target": "es6",
    "lib": [
      "dom",
      "dom.iterable",
      "esnext"
    ],
    "allowJs": true,
    "skipLibCheck": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "strict": true,
    "forceConsistentCasingInFileNames": true,
    "noFallthroughCasesInSwitch": true,
    "module": "esnext",
    "moduleResolution": "node",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",
    "typeRoots": ["./node_modules/@types", "./Frontend/types"],
    "declaration": true,
  },
  "include": [
    "src"
  ]
}
```