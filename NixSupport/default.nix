{ compiler ? null  # Uses haskellPackages (default GHC) when null
, additionalNixpkgsOptions ? {}
, pkgs ? import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; additionalNixpkgsOptions = additionalNixpkgsOptions; }
, ghc ? if compiler == null then pkgs.haskellPackages else pkgs.haskell.packages.${compiler}
, ihp
, haskellDeps ? (p: [])
, otherDeps ? (p: [])
, projectPath ? ./.
, withHoogle ? false
, optimized ? false
, rtsFlags ? ""
, appName ? "app"
, optimizationLevel ? "2"
, relationSupport ? true
, filter
, ihp-env-var-backwards-compat
, ihp-static
, static
}:

let
    splitSections = if !pkgs.stdenv.hostPlatform.isDarwin then "-split-sections" else "";

    # Common IHP environment setup
    ihpEnvSetup = ''
        export IHP_LIB=${ihp-env-var-backwards-compat}
        export IHP=${ihp-env-var-backwards-compat}
    '';

    # Generate the models package source from Schema.sql
    modelsPackageSrc = pkgs.stdenv.mkDerivation {
        name = "${appName}-models-src";
        src = filter {
            root = projectPath;
            include = ["Application/Schema.sql" "Makefile"];
            name = "${appName}-models-source";
        };
        nativeBuildInputs = [
            (ghc.ghcWithPackages (p: [ p.ihp-schema-compiler ])) # Needed for build-generated-code
            pkgs.gnumake # Needed for make print-ghc-options
        ];
        buildPhase = ''
            export IHP_RELATION_SUPPORT=${if relationSupport then "1" else "0"}

            # Generate types from schema
            build-generated-code

            # Find all generated modules and create cabal file
            # Use path relative to build/ and convert slashes to dots for module names
            MODULES=$(cd build && find Generated -name '*.hs' | sed 's/\.hs$//' | sed 's|/|.|g' | sort)

            # Create cabal file
            cat > ${appName}-models.cabal <<'CABAL_EOF'
cabal-version: 2.2
name: ${appName}-models
version: 0.1.0
build-type: Simple

library
    default-language: GHC2021
    hs-source-dirs: build
    build-depends:
        base
        , ihp
        , basic-prelude
        , text
        , bytestring
        , time
        , uuid
        , aeson
        , postgresql-simple
        , deepseq
        , data-default
        , scientific
        , string-conversions
        , hasql
        , hasql-dynamic-statements
        , hasql-implicits
        , hasql-mapping
        , hasql-postgresql-types
        , hasql-pool
        , unordered-containers
    exposed-modules:
CABAL_EOF

            # Add each module to exposed-modules
            for mod in $MODULES; do
                echo "        $mod" >> ${appName}-models.cabal
            done

            # Add default extensions matching the generated code
            cat >> ${appName}-models.cabal <<'CABAL_EOF'
    default-extensions:
        OverloadedStrings
        NoImplicitPrelude
        ImplicitParams
        TypeSynonymInstances
        FlexibleInstances
        FlexibleContexts
        InstanceSigs
        MultiParamTypeClasses
        TypeFamilies
        DataKinds
        TypeOperators
        UndecidableInstances
        ConstraintKinds
        StandaloneDeriving
        DuplicateRecordFields
        OverloadedLabels
        OverloadedRecordDot
    ghc-options: -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches
CABAL_EOF
        '';
        installPhase = ''
            mkdir -p $out/build
            cp -r build/Generated $out/build/
            cp ${appName}-models.cabal $out/
        '';
        disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
    };

    # Inline mkDerivation instead of callCabal2nix to avoid IFD (Import From Derivation).
    # The dependencies here must match the .cabal template generated in modelsPackageSrc above.
    modelsPackage = pkgs.haskell.lib.disableLibraryProfiling (pkgs.haskell.lib.dontHaddock (
        ghc.callPackage ({ mkDerivation, base, ihp, basic-prelude, text, bytestring, time, uuid, aeson, postgresql-simple, deepseq, data-default, scientific, string-conversions, hasql, hasql-dynamic-statements, hasql-implicits, hasql-mapping, hasql-postgresql-types, hasql-pool, unordered-containers }: mkDerivation {
            pname = "${appName}-models";
            version = "0.1.0";
            src = modelsPackageSrc;
            libraryHaskellDepends = [
                base
                ihp
                basic-prelude
                text
                bytestring
                time
                uuid
                aeson
                postgresql-simple
                deepseq
                data-default
                scientific
                string-conversions
                hasql
                hasql-dynamic-statements
                hasql-implicits
                hasql-mapping
                hasql-postgresql-types
                hasql-pool
                unordered-containers
            ];
            license = pkgs.lib.licenses.free;
        }) {}
    ));

    allHaskellPackages =
        (if withHoogle
        then ghc.ghcWithHoogle
        else ghc.ghcWithPackages) (p: haskellDeps p ++ [ modelsPackage ]);

    allNativePackages = builtins.concatLists [
        (otherDeps pkgs)
    ];

    # Common derivation attributes
    commonBuildInputs = [ allHaskellPackages ];
    commonNativeBuildInputs = [ pkgs.gnumake ];

    prodGhcOptions = "-funbox-strict-fields -fconstraint-solver-iterations=100 -fdicts-strict -with-rtsopts=\"${rtsFlags}\"";

    appSrc = filter { root = pkgs.nix-gitignore.gitignoreSource [] projectPath; include = [filter.isDirectory "Makefile" (filter.matchExt "hs")]; exclude = ["static" "Frontend"]; name = "${appName}-source"; };

    scriptDir = projectPath + "/Application/Script";

    scriptNames =
        if builtins.pathExists scriptDir
        then
            let
                entries = builtins.readDir scriptDir;
                names = builtins.attrNames entries;
                hsFiles =
                    builtins.filter
                        (n:
                            entries.${n} == "regular"
                            && pkgs.lib.hasSuffix ".hs" n
                            && n != "Prelude.hs"
                        )
                        names;
            in
                map (n: pkgs.lib.removeSuffix ".hs" n) hsFiles
        else
            [];

    # Generate .cabal file for the app library package.
    # build-depends is populated at derivation build time by querying ghc-pkg
    # for all registered package names, ensuring all transitive deps of ihp are available.
    appLibSrc = pkgs.stdenv.mkDerivation {
        name = "${appName}-lib-src";
        src = appSrc;
        nativeBuildInputs = [ pkgs.findutils allHaskellPackages ];
        buildPhase = ''
            # Find all .hs modules excluding entry points and tests
            # Modules under Config/ are found via the "Config" source dir,
            # so their module names are relative to Config/ (e.g. Config/Config.hs -> Config)
            CONFIG_MODULES=""
            if [ -d Config ]; then
                CONFIG_MODULES=$(find Config -name '*.hs' \
                    | sed 's|^Config/||' \
                    | sed 's|\.hs$||' \
                    | sed 's|/|.|g' \
                    | sort)
            fi

            # All other modules are relative to "." source dir
            OTHER_MODULES=$(find . -name '*.hs' \
                -not -name 'Main.hs' \
                -not -name 'Setup.hs' \
                -not -path './build/*' \
                -not -path './Config/*' \
                -not -path './Test/*' \
                | sed 's|^\./||' \
                | sed 's|\.hs$||' \
                | sed 's|/|.|g' \
                | sort)

            # Get all registered package names from the GHC environment
            # This includes ihp and ALL its transitive deps (aeson, bytestring, lens, etc.)
            # Filter out internal sub-libraries (z-* prefixed names) as they are private
            ALL_PKG_NAMES=$(ghc-pkg list --simple-output | tr ' ' '\n' | sed 's/-[0-9].*//' | sort -u | grep -v '^$' | grep -v '^z-')

            cat > ${appName}-lib.cabal <<'CABAL_HEADER'
cabal-version: 2.2
name: ${appName}-lib
version: 0.1.0
build-type: Simple

library
    default-language: GHC2021
    hs-source-dirs: . Config
    build-depends:
CABAL_HEADER

            # Add all registered packages as build-depends
            FIRST=true
            for pkg in $ALL_PKG_NAMES; do
                if [ "$FIRST" = true ]; then
                    echo "        $pkg" >> ${appName}-lib.cabal
                    FIRST=false
                else
                    echo "        , $pkg" >> ${appName}-lib.cabal
                fi
            done

            echo "    exposed-modules:" >> ${appName}-lib.cabal

            for mod in $OTHER_MODULES $CONFIG_MODULES; do
                echo "        $mod" >> ${appName}-lib.cabal
            done

            cat >> ${appName}-lib.cabal <<'CABAL_EOF'
    default-extensions:
        GHC2021
        OverloadedStrings
        NoImplicitPrelude
        ImplicitParams
        Rank2Types
        DisambiguateRecordFields
        NamedFieldPuns
        DuplicateRecordFields
        OverloadedLabels
        FlexibleContexts
        TypeSynonymInstances
        FlexibleInstances
        QuasiQuotes
        TypeFamilies
        PackageImports
        ScopedTypeVariables
        RecordWildCards
        TypeApplications
        DataKinds
        InstanceSigs
        DeriveGeneric
        MultiParamTypeClasses
        TypeOperators
        DeriveDataTypeable
        MultiWayIf
        UndecidableInstances
        BlockArguments
        PartialTypeSignatures
        LambdaCase
        DefaultSignatures
        EmptyDataDeriving
        BangPatterns
        FunctionalDependencies
        StandaloneDeriving
        DerivingVia
        TemplateHaskell
        DeepSubsumption
        OverloadedRecordDot
    ghc-options:
        -Wno-unsafe
        -Wno-name-shadowing
        -Wno-monomorphism-restriction
        -Wno-safe
        -Wno-missing-local-signatures
        -Wno-missing-home-modules
        -Wno-partial-type-signatures
        -Werror=missing-fields
        -fwarn-incomplete-patterns
CABAL_EOF
        '';
        installPhase = ''
            mkdir -p $out

            # Copy all source files preserving directory structure (excluding entry points and tests)
            find . -name '*.hs' -not -name 'Main.hs' -not -name 'Setup.hs' -not -path './build/*' -not -path './Test/*' | while read f; do
                mkdir -p "$out/$(dirname "$f")"
                cp "$f" "$out/$f"
            done

            cp ${appName}-lib.cabal $out/
        '';
        disallowedReferences = [ ihp ];
    };

    appLibPackage = pkgs.haskell.lib.disableLibraryProfiling (pkgs.haskell.lib.dontHaddock (
        ghc.callPackage ({ mkDerivation, base }: mkDerivation {
            pname = "${appName}-lib";
            version = "0.1.0";
            src = appLibSrc;
            libraryHaskellDepends = [ base modelsPackage ] ++ builtins.filter (p: p != null) (haskellDeps ghc);
            license = pkgs.lib.licenses.free;
        }) {}
    ));

    allHaskellPackagesWithAppLib = ghc.ghcWithPackages (p: [ appLibPackage ]);

    hasJobs =
        let
            isHsFile = entries: name:
                entries.${name} == "regular" && pkgs.lib.hasSuffix ".hs" name;

            anyJobHsIn =
                dir:
                    let
                        entries = builtins.readDir dir;
                        names = builtins.attrNames entries;

                        subdirs =
                            builtins.filter (n: entries.${n} == "directory") names;

                        hereIsJobDir = pkgs.lib.toLower (builtins.baseNameOf (toString dir)) == "job";

                        hasHsHere =
                            hereIsJobDir && builtins.any (isHsFile entries) names;

                        hasHsBelow =
                            builtins.any (n: anyJobHsIn (dir + "/${n}")) subdirs;
                    in
                        hasHsHere || hasHsBelow;
        in
            anyJobHsIn projectPath;

    binaries =
        pkgs.stdenv.mkDerivation {
            name = "${appName}-binaries";
            src = appSrc;

            buildInputs = [ allHaskellPackagesWithAppLib ];
            nativeBuildInputs = commonNativeBuildInputs;

            buildPhase = ''
                mkdir -p build/bin build/obj
                ${ihpEnvSetup}

                # Delete all .hs files except Main.hs so GHC uses the library package
                # instead of recompiling from source
                find . -name '*.hs' -not -name 'Main.hs' -not -path './build/*' -delete

                # Build RunProdServer from Main.hs
                ghc -j"''${NIX_BUILD_CORES:-1}" +RTS -N -RTS \
                    -O${if optimized then optimizationLevel else "0"} ${splitSections} \
                    $(make print-ghc-options) \
                    ${if optimized then prodGhcOptions else ""} \
                    Main.hs -o build/bin/RunProdServer \
                    -odir build/obj -hidir build/obj

            '' + pkgs.lib.optionalString hasJobs ''
                # Generate and build RunJobs
                cat > build/RunJobs.hs <<'EOF'
                module RunJobs (main) where
                import Application.Script.Prelude
                import IHP.ScriptSupport
                import IHP.Job.Runner
                import qualified Config
                import Main ()
                main :: IO ()
                main = runScript Config.config (runJobWorkers (workers RootApplication))
                EOF

                ghc -j"''${NIX_BUILD_CORES:-1}" +RTS -N -RTS \
                    -O${if optimized then optimizationLevel else "0"} ${splitSections} \
                    -main-is 'RunJobs.main' \
                    $(make print-ghc-options) \
                    ${if optimized then prodGhcOptions else ""} \
                    build/RunJobs.hs -o build/bin/RunJobs \
                    -odir build/obj -hidir build/obj

            '' + builtins.concatStringsSep "" (map (scriptName: ''
                # Build script: ${scriptName}
                mkdir -p build/Script/Main
                cat > build/Script/Main/${scriptName}.hs <<'EOF'
                module Main (main) where
                import IHP.ScriptSupport
                import qualified Config
                import Application.Script.${scriptName} (run)
                main = runScript Config.config run
                EOF

                ghc -j"''${NIX_BUILD_CORES:-1}" +RTS -N -RTS \
                    -O${if optimized then optimizationLevel else "0"} ${splitSections} \
                    $(make print-ghc-options) \
                    ${if optimized then prodGhcOptions else ""} \
                    build/Script/Main/${scriptName}.hs -o build/bin/${scriptName} \
                    -odir build/obj -hidir build/obj

            '') scriptNames);

            installPhase = ''
                mkdir -p $out/bin
                cp build/bin/* $out/bin/
            '';

            enableParallelBuilding = true;
            disallowedReferences = [ ihp ];
        };
in
    pkgs.runCommand appName { inherit static binaries; nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
            # Hash that changes only when `static` changes:
            INPUT_HASH="$(basename ${static} | cut -d- -f1)"
            makeWrapper ${binaries}/bin/RunProdServer $out/bin/RunProdServer \
                --set-default IHP_ASSET_VERSION $INPUT_HASH \
                --set-default APP_STATIC ${static} \
                --set-default IHP_STATIC ${ihp-static} \
                --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}

            # Copy job runner binary to bin/ if we built it
            if [ -f ${binaries}/bin/RunJobs ]; then
                makeWrapper ${binaries}/bin/RunJobs $out/bin/RunJobs \
                    --set-default IHP_ASSET_VERSION $INPUT_HASH \
                    --set-default APP_STATIC ${static} \
                    --set-default IHP_STATIC ${ihp-static} \
                    --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}
            fi;

            # Copy other binaries, excluding RunProdServer and RunJobs
            find ${binaries}/bin/ -maxdepth 1 -type f -not -name 'RunProdServer' -not -name 'RunJobs' -print0 |
                while read -d $'\0' binary; do
                    binary_basename=$(basename "$binary")
                    cp "$binary" "$out/bin/$binary_basename";
                done
    ''