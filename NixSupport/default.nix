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

    binaries =
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

                echo ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} Main.hs -o build/bin/RunProdServer -odir build/RunProdServer -hidir build/RunProdServer
                ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} Main.hs -o build/bin/RunProdServer -odir build/RunProdServer -hidir build/RunProdServer

                # Build job runner if there are any jobs
                if find -type d -iwholename \*/Job|grep .; then
                    echo "module RunJobs (main) where" > build/RunJobs.hs
                    echo "import Application.Script.Prelude" >> build/RunJobs.hs
                    echo "import IHP.ScriptSupport" >> build/RunJobs.hs
                    echo "import IHP.Job.Runner" >> build/RunJobs.hs
                    echo "import qualified Config" >> build/RunJobs.hs
                    echo "import Main ()" >> build/RunJobs.hs
                    echo "main :: IO ()" >> build/RunJobs.hs
                    echo "main = runScript Config.config (runJobWorkers (workers RootApplication))" >> build/RunJobs.hs
                    ghc -O${if optimized then optimizationLevel else "0"} ${splitSections} -main-is 'RunJobs.main' $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} build/RunJobs.hs -o build/bin/RunJobs -odir build/RunProdServer -hidir build/RunProdServer
                fi;

                # Build all scripts if there are any
                mkdir -p Application/Script
                SCRIPT_TARGETS=`find Application/Script -type f -iwholename '*.hs' -not -name 'Prelude.hs' -exec basename {} .hs ';' | sed 's#^#build/bin/Script/#' | tr "\n" " "`
                if [[ ! -z "$SCRIPT_TARGETS" ]]; then
                    # Need to use -j1 here to avoid race conditions of temp files created by GHC.
                    #
                    # These errors look like:
                    #
                    #   <no location info>: error:
                    #   build/RunUnoptimizedProdServer/Application/Script/Prelude.o.tmp: renameFile:renamePath:rename: does not exist (No such file or directory)
                    #
                    make -j1 $SCRIPT_TARGETS;
                fi;
            '';
            installPhase = ''
                mkdir -p "$out"
                mkdir -p $out/bin $out/lib

                mv build/bin/RunProdServer $out/bin/RunProdServer

                # Copy job runner binary to bin/ if we built it
                if [ -f build/bin/RunJobs ]; then
                    mv build/bin/RunJobs $out/bin/RunJobs;
                fi;

                # Copy IHP Script binaries to bin/
                mkdir -p build/bin/Script
                find build/bin/Script/ -type f -print0 |
                    while read -d $'\0' script; do
                        script_basename=$(basename "$script")
                        mv "build/bin/Script/$script_basename" "$out/bin/$script_basename";
                    done
            '';
            src = filter { root = pkgs.nix-gitignore.gitignoreSource [] projectPath; include = [filter.isDirectory "Makefile" (filter.matchExt "hs")]; exclude = ["static" "Frontend"]; name = "${appName}-source"; };
            buildInputs = [allHaskellPackages];
            nativeBuildInputs = [schemaObjectFiles];
            enableParallelBuilding = true;
            disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
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
            find ${binaries}/bin/ -type f -not -name 'RunProdServer' -not -name 'RunJobs' -print0 |
                while read -d $'\0' binary; do
                    binary_basename=$(basename "$binary")
                    cp "$binary" "$out/bin/$binary_basename";
                done
    ''