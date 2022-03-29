{ compiler ? "ghc8107"
, ihp
, haskellDeps ? (p: [])
, otherDeps ? (p: [])
, projectPath ? ./.
, withHoogle ? false
, additionalNixpkgsOptions ? {}
, postgresExtensions ? (p: [])
, optimized ? false
}:

let
    pkgs = import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; additionalNixpkgsOptions = additionalNixpkgsOptions; };
    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages =
      (if withHoogle
      then ghc.ghcWithHoogle
      else ghc.ghcWithPackages)
        (p: builtins.concatLists [ [p.haskell-language-server] (haskellDeps p) ] );
    allNativePackages = builtins.concatLists [ (otherDeps pkgs)
    [pkgs.postgresql_14 pkgs.makeWrapper] (if pkgs.stdenv.isDarwin then [] else []) ];

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
          rm -f build/ihp-lib

          mkdir -p IHP
          ln -s "${ihp}/lib/IHP" build/ihp-lib
          ln -s "${ihp}/lib" IHP/lib # Avoid the Makefile calling 'which RunDevServer'

          # When npm install is executed by the project's makefile it will fail with:
          #
          #     EACCES: permission denied, mkdir '/homeless-shelter'
          #
          # To avoid this error we use /tmp as our home directory for the build
          #
          # See https://github.com/svanderburg/node2nix/issues/217#issuecomment-751311272
          export HOME=/tmp

          make ${appBinary}

          # Build job runner if there are any jobs
          if find -type d -iwholename \*/Job|grep .; then
            make ${jobsBinary};
          fi;

          # Build all scripts if there are any
          mkdir -p Application/Script
          SCRIPT_TARGETS=`find Application/Script -type f -iwholename '*.hs' -not -name 'Prelude.hs' -exec basename {} .hs ';' | sed 's#^#build/bin/Script/#' | tr "\n" " "`
          if [[ ! -z "$SCRIPT_TARGETS" ]]; then
            make $SCRIPT_TARGETS;
          fi;
        '';
        installPhase = ''
          mkdir -p "$out"
          mkdir -p $out/bin

          mv ${appBinary} $out/bin/RunProdServerWithoutOptions
          mv build/bin/RunDevServer $out/bin/RunDevServerWithoutWrapper
          makeWrapper $out/bin/RunDevServerWithoutWrapper $out/bin/RunDevServer --prefix PATH : ${pkgs.lib.makeBinPath allNativePackages}

          INPUT_HASH="$((basename $out) | cut -d - -f 1)"
          makeWrapper $out/bin/RunProdServerWithoutOptions $out/bin/RunProdServer --set-default IHP_ASSET_VERSION $INPUT_HASH --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}

          # Copy job runner binary to bin/ if we built it
          if [ -f ${jobsBinary} ]; then
            mv ${jobsBinary} $out/bin/RunJobsWithoutOptions;
            makeWrapper $out/bin/RunJobsWithoutOptions $out/bin/RunJobs --set-default IHP_ASSET_VERSION $INPUT_HASH --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}
          fi;

          # Copy IHP Script binaries to bin/
          mkdir -p build/bin/Script
          find build/bin/Script/ -type f -print0 |
            while read -d $'\0' script; do
              script_basename=$(basename "$script")
              mv "build/bin/Script/$script_basename" "$out/bin/$script_basename";
            done

          mkdir -p "$out/lib/build"
          cp -R "${ihp}/lib/IHP" "$out/lib/build/ihp-lib"
          mv static "$out/lib/static"

          cp Application/Schema.sql "$out/lib/Schema.sql"
        '';
        dontFixup = true;
        src = (import <nixpkgs> {}).nix-gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [ [allHaskellPackages] allNativePackages ];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }
