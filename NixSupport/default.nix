{ compiler ? "ghc98"
, additionalNixpkgsOptions ? {}
, pkgs ? import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; additionalNixpkgsOptions = additionalNixpkgsOptions; }
, ghc ? pkgs.haskell.packages.${compiler}
, ihp
, haskellDeps ? (p: [])
, otherDeps ? (p: [])
, projectPath ? ./.
, withHoogle ? false
, optimized ? false
, rtsFlags ? ""
, appName ? "app"
, optimizationLevel ? "2"
, filter
, ihp-env-var-backwards-compat
, static
}:

let
    allHaskellPackages =
        (if withHoogle
        then ghc.ghcWithHoogle
        else ghc.ghcWithPackages) haskellDeps;
    allNativePackages = builtins.concatLists [
        (otherDeps pkgs)
    ];

    splitSections = if !pkgs.stdenv.hostPlatform.isDarwin then "-split-sections" else "";

    schemaObjectFiles =
        let
            self = projectPath;
        in
            pkgs.stdenv.mkDerivation {
                name = appName + "-schema";
                buildPhase = ''
                    mkdir -p build/Generated
                    build-generated-code

                    export IHP=${ihp-env-var-backwards-compat}
                    ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} $(make print-ghc-options) --make build/Generated/Types.hs -odir build/RunProdServer -hidir build/RunProdServer

                    cp -r build $out
                '';
                src = filter { root = self; include = ["Application/Schema.sql" "Makefile"]; name = "schemaObjectFiles-source"; };
                nativeBuildInputs =
                    [ (ghc.ghcWithPackages (p: [ p.ihp-ide ])) # Needed for build-generated-code
                    ]
                ;
                dontInstall = true;
                dontFixup = false;
                disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
            };

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


    mkScript =
        scriptName:
            pkgs.stdenv.mkDerivation {
                name = "${appName}-script-${scriptName}";
                src = appSrc;

                buildInputs = [ allHaskellPackages ];
                nativeBuildInputs = [ pkgs.gnumake schemaObjectFiles ];

                buildPhase = ''
                    mkdir -p build/bin build/Generated build/RunProdServer
                    cp -r ${schemaObjectFiles}/RunProdServer build/
                    cp -r ${schemaObjectFiles}/Generated build/
                    chmod -R +w build/RunProdServer build/Generated || true

                    export IHP_LIB=${ihp-env-var-backwards-compat}
                    export IHP=${ihp-env-var-backwards-compat}

                    mkdir -p build/Script/Main
                    cat > build/Script/Main/${scriptName}.hs <<'EOF'
                    module Main (main) where
                    import IHP.ScriptSupport
                    import qualified Config
                    import Application.Script.${scriptName} (run)
                    main = runScript Config.config run
                    EOF

                    mkdir -p build/RunScript/${scriptName}

                    ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} \
                        $(make print-ghc-options) \
                        ${if optimized then prodGhcOptions else ""} \
                        build/Script/Main/${scriptName}.hs \
                        -o build/bin/${scriptName} \
                        -odir build/RunScript/${scriptName} \
                        -hidir build/RunScript/${scriptName}
                '';

                installPhase = ''
                    mkdir -p $out/bin
                    mv build/bin/${scriptName} $out/bin/${scriptName}
                '';

                disallowedReferences = [ ihp ];
            };

    scriptPackages = map mkScript scriptNames;
    allScripts = pkgs.symlinkJoin { name = "${appName}-scripts"; paths = scriptPackages; };

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
            anyJobHsIn appSrc;

    runJobs =
        pkgs.stdenv.mkDerivation {
            name = "${appName}-run-jobs";
            src = appSrc;

            buildInputs = [ allHaskellPackages ];
            nativeBuildInputs = [ pkgs.gnumake schemaObjectFiles ];

            buildPhase = ''
                mkdir -p build/Generated build/RunProdServer build/bin
                cp -r ${schemaObjectFiles}/RunProdServer build/
                cp -r ${schemaObjectFiles}/Generated build/
                chmod -R +w build/RunProdServer build/Generated || true

                export IHP_LIB=${ihp-env-var-backwards-compat}
                export IHP=${ihp-env-var-backwards-compat}

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

                ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} \
                    -main-is 'RunJobs.main' \
                    $(make print-ghc-options) \
                    ${if optimized then prodGhcOptions else ""} \
                    build/RunJobs.hs -o build/bin/RunJobs \
                    -odir build/RunProdServer -hidir build/RunProdServer
            '';

            installPhase = ''
                mkdir -p $out/bin
                mv build/bin/RunJobs $out/bin/RunJobs
            '';

            disallowedReferences = [ ihp ];
        };

    runServer =
        pkgs.stdenv.mkDerivation {
            name = appName + "-binaries";
            buildPhase = ''
                mkdir -p build/Generated build/RunProdServer
                cp -r ${schemaObjectFiles}/RunProdServer build/
                cp -r ${schemaObjectFiles}/Generated build/

                chmod -R +w build/RunProdServer/*

                export IHP_LIB=${ihp-env-var-backwards-compat}
                export IHP=${ihp-env-var-backwards-compat}

                mkdir -p build/bin build/RunUnoptimizedProdServer

                ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} Main.hs -o build/bin/RunProdServer -odir build/RunProdServer -hidir build/RunProdServer
            '';
            installPhase = ''
                mkdir -p "$out"
                mkdir -p $out/bin $out/lib

                mv build/bin/RunProdServer $out/bin/RunProdServer
            '';
            src = appSrc;
            buildInputs = [allHaskellPackages];
            nativeBuildInputs = [schemaObjectFiles];
            enableParallelBuilding = true;
            disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
        };

    binaries =
        pkgs.symlinkJoin {
            name = "${appName}-binaries";
            paths = [ runServer allScripts ] ++ pkgs.lib.optional hasJobs runJobs;
        };
in
    pkgs.runCommandNoCC appName { inherit static binaries; nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
            # Hash that changes only when `static` changes:
            INPUT_HASH="$(basename ${static} | cut -d- -f1)"
            makeWrapper ${binaries}/bin/RunProdServer $out/bin/RunProdServer \
                --set-default IHP_ASSET_VERSION $INPUT_HASH \
                --set-default APP_STATIC ${static} \
                --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}

            # Copy job runner binary to bin/ if we built it
            if [ -f ${binaries}/bin/RunJobs ]; then
                makeWrapper ${binaries}/bin/RunJobs $out/bin/RunJobs \
                    --set-default IHP_ASSET_VERSION $INPUT_HASH \
                    --set-default APP_STATIC ${static} \
                    --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}
            fi;

            # Copy other binaries, excluding RunProdServer and RunJobs
            find ${binaries}/bin/ -type l -not -name 'RunProdServer' -not -name 'RunJobs' -print0 |
                while read -d $'\0' binary; do
                    binary_basename=$(basename "$binary")
                    cp "$binary" "$out/bin/$binary_basename";
                done
    ''