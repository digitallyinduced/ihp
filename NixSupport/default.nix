{ compiler ? "ghc98"
, additionalNixpkgsOptions ? {}
, pkgs ? import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; additionalNixpkgsOptions = additionalNixpkgsOptions; }
, ghc ? pkgs.haskell.packages.${compiler}
, ihp
, haskellDeps ? (p: [])
, otherDeps ? (p: [])
, projectPath ? ./.
, withHoogle ? false
, postgresExtensions ? (p: [])
, optimized ? false
, includeDevTools ? !optimized # Include Postgres?
, rtsFlags ? ""
, optimizationLevel ? "2"
}:

let
    allHaskellPackages =
      (if withHoogle
      then ghc.ghcWithHoogle
      else ghc.ghcWithPackages) haskellDeps;
    allNativePackages = builtins.concatLists [
      (otherDeps pkgs)
    ];

    appBinary = if optimized
      then "build/bin/RunOptimizedProdServer"
      else "build/bin/RunUnoptimizedProdServer";

    jobsBinary = if optimized
      then "build/bin/RunJobsOptimized"
      else "build/bin/RunJobs";
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        buildPhase = ''
          mkdir -p build

          # When npm install is executed by the project's makefile it will fail with:
          #
          #     EACCES: permission denied, mkdir '/homeless-shelter'
          #
          # To avoid this error we use /tmp as our home directory for the build
          #
          # See https://github.com/svanderburg/node2nix/issues/217#issuecomment-751311272
          export HOME=/tmp

          export IHP_LIB=${ihp}/lib/IHP
          export IHP=${ihp}/lib/IHP
          export APP_RTS_FLAGS="${rtsFlags}"
          export OPTIMIZATION_LEVEL="${optimizationLevel}"

          make -j ${appBinary}

          # Build job runner if there are any jobs
          if find -type d -iwholename \*/Job|grep .; then
            make -j ${jobsBinary};
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

          mv ${appBinary} $out/bin/RunProdServerWithoutOptions

          INPUT_HASH="$((basename $out) | cut -d - -f 1)"
          makeWrapper $out/bin/RunProdServerWithoutOptions $out/bin/RunProdServer --set-default IHP_ASSET_VERSION $INPUT_HASH --set-default IHP_LIB ${ihp}/lib/IHP --run "cd $out/lib" --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}

          # Copy job runner binary to bin/ if we built it
          if [ -f ${jobsBinary} ]; then
            mv ${jobsBinary} $out/bin/RunJobsWithoutOptions;
            makeWrapper $out/bin/RunJobsWithoutOptions $out/bin/RunJobs --set-default IHP_ASSET_VERSION $INPUT_HASH --set-default IHP_LIB ${ihp}/lib/IHP --run "cd $out/lib" --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}
          fi;

          # Copy IHP Script binaries to bin/
          mkdir -p build/bin/Script
          find build/bin/Script/ -type f -print0 |
            while read -d $'\0' script; do
              script_basename=$(basename "$script")
              mv "build/bin/Script/$script_basename" "$out/bin/$script_basename";
            done

          mv static "$out/lib/static"
        '';
        dontFixup = true;
        src = pkgs.nix-gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [ [allHaskellPackages] allNativePackages ];
        nativeBuildInputs = builtins.concatLists [
          [ pkgs.makeWrapper
            pkgs.cacert # Needed for npm install to work from within the IHP build process
          ]
          (if includeDevTools then [(pkgs.postgresql_13.withPackages postgresExtensions)] else [])
        ];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
        enableParallelBuilding = true;
        impureEnvVars = pkgs.lib.fetchers.proxyImpureEnvVars; # Needed for npm install to work from within the IHP build process
    }
