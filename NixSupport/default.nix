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
}:

let
    allHaskellPackages =
        (if withHoogle
        then ghc.ghcWithHoogle
        else ghc.ghcWithPackages) haskellDeps;
    allNativePackages = builtins.concatLists [
        (otherDeps pkgs)
    ];

    ihpLibWithMakefile = filter { root = ihp; include = ["lib/IHP/Makefile.dist"]; name = "ihpLibWithMakefile"; };
    ihpLibWithMakefileAndStatic = filter { root = ihp; include = ["lib/IHP/Makefile.dist" "lib/IHP/static"]; name = "ihpLibWithMakefileAndStatic"; };

    schemaObjectFiles =
        let
            self = projectPath;
        in
            pkgs.stdenv.mkDerivation {
                name = appName + "-schema";
                buildPhase = ''
                    mkdir -p build/Generated
                    build-generated-code

                    export IHP=${ihpLibWithMakefile}/lib/IHP
                    ghc -O${if optimized then optimizationLevel else "0"} $(make print-ghc-options) --make build/Generated/Types.hs -odir build/RunProdServer -hidir build/RunProdServer

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

                export IHP_LIB=${ihpLibWithMakefile}/lib/IHP
                export IHP=${ihpLibWithMakefile}/lib/IHP

                mkdir -p build/bin build/RunUnoptimizedProdServer

                echo ghc -O${if optimized then optimizationLevel else "0"} $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} Main.hs -o build/bin/RunProdServer -odir build/RunProdServer -hidir build/RunProdServer
                ghc -O${if optimized then optimizationLevel else "0"} $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} Main.hs -o build/bin/RunProdServer -odir build/RunProdServer -hidir build/RunProdServer

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
                    ghc -O${if optimized then optimizationLevel else "0"} -main-is 'RunJobs.main' $(make print-ghc-options) ${if optimized then prodGhcOptions else ""} build/RunJobs.hs -o build/bin/RunJobs -odir build/RunProdServer -hidir build/RunProdServer
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
            dontFixup = true;
            src = filter { root = pkgs.nix-gitignore.gitignoreSource [] projectPath; include = [filter.isDirectory "Makefile" (filter.matchExt "hs")]; exclude = ["static" "Frontend"]; name = "${appName}-source"; };
            buildInputs = [allHaskellPackages];
            nativeBuildInputs = [ pkgs.makeWrapper schemaObjectFiles];
            enableParallelBuilding = true;
            disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
        };
in
    pkgs.stdenv.mkDerivation {
        name = appName;
        buildPhase = ''
            runHook preBuild

            # When npm install is executed by the project's makefile it will fail with:
            #
            #     EACCES: permission denied, mkdir '/homeless-shelter'
            #
            # To avoid this error we use /tmp as our home directory for the build
            #
            # See https://github.com/svanderburg/node2nix/issues/217#issuecomment-751311272
            export HOME=/tmp

            export IHP_LIB=${ihpLibWithMakefileAndStatic}/lib/IHP
            export IHP=${ihpLibWithMakefileAndStatic}/lib/IHP

            make -j static/app.css static/app.js

            runHook postBuild
        '';
        installPhase = ''
            runHook preInstall

            mkdir -p "$out"
            mkdir -p $out/bin $out/lib

            INPUT_HASH="$((basename $out) | cut -d - -f 1)"
            makeWrapper ${binaries}/bin/RunProdServer $out/bin/RunProdServer --set-default IHP_ASSET_VERSION $INPUT_HASH --set-default IHP_LIB ${ihpLibWithMakefileAndStatic}/lib/IHP --run "cd $out/lib" --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}

            # Copy job runner binary to bin/ if we built it
            if [ -f ${binaries}/bin/RunJobs ]; then
                makeWrapper ${binaries}/bin/RunJobs $out/bin/RunJobs --set-default IHP_ASSET_VERSION $INPUT_HASH --set-default IHP_LIB ${ihpLibWithMakefileAndStatic}/lib/IHP --run "cd $out/lib" --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}
            fi;

            # Copy other binaries, excluding RunProdServer and RunJobs
            find ${binaries}/bin/ -type f -not -name 'RunProdServer' -not -name 'RunJobs' -print0 |
                while read -d $'\0' binary; do
                    binary_basename=$(basename "$binary")
                    cp "$binary" "$out/bin/$binary_basename";
                done

            mv static "$out/lib/static"

            runHook postInstall
        '';
        dontFixup = true;
        src = pkgs.nix-gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [ allNativePackages ];
        nativeBuildInputs = builtins.concatLists [
            [ pkgs.makeWrapper
              pkgs.cacert # Needed for npm install to work from within the IHP build process
              [allHaskellPackages] 
            ]
        ];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
        enableParallelBuilding = true;
        impureEnvVars = pkgs.lib.fetchers.proxyImpureEnvVars; # Needed for npm install to work from within the IHP build process
        disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
    }