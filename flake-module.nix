/*
flake-parts module for IHP apps
can be imported in a flake inside an IHP app repo
*/

# with ihpFlake we receive arguments from the IHP flake in this repo itself
ihpFlake:

# these arguments on the other hand are from the flake where this module is imported
# i.e. from the flake of any particular IHP app
{ self, inputs, flake-parts-lib, lib, config, ... }:

{

    imports = [
        inputs.devenv.flakeModule
    ];

    # the app can configure IHP using these options from its flake
    options.perSystem = flake-parts-lib.mkPerSystemOption (
        { config, pkgs, system, ...}: {
            options.ihp = {
                enable = lib.mkEnableOption "Enable IHP support";

                ghcCompiler = lib.mkOption {
                    description = ''
                        The GHC compiler to use for IHP.
                    '';
                    default = pkgs.haskell.packages.ghc98;
                };

                packages = lib.mkOption {
                    description = ''
                        List of packages that should be included in the IHP environment.
                    '';
                    type = lib.types.listOf (lib.types.package);
                    default = [];
                };

                haskellPackages = lib.mkOption {
                    description = ''
                        Function returning a list of Haskell packages to be installed in the IHP environment.
                        The set of Haskell packages is passed to the function.
                    '';
                    default = p: [
                        p.cabal-install
                        p.base
                        p.wai
                        p.text
                        p.hlint
                        p.ihp
                    ];
                };

                appName = lib.mkOption {
                    description = '' 
                        The derivation name.
                    '';
                    type = lib.types.str;
                    default = "app";
                };

                projectPath = lib.mkOption {
                    description = ''
                        Path to the IHP project. You likely want to set this to `./.`.
                    '';
                    type = lib.types.path;
                };

                withHoogle = lib.mkOption {
                    description = ''
                        Enable Hoogle support. Adds `hoogle` command to PATH.
                    '';
                    type = lib.types.bool;
                    default = false;
                };

                dontCheckPackages = lib.mkOption {
                    description = ''
                        List of Haskell package names whose tests are skipped during build.
                    '';
                    type = lib.types.listOf (lib.types.str);
                    default = [];
                };

                doJailbreakPackages = lib.mkOption {
                    description = ''
                        List of Haskell package names who are jailbreaked before build.
                    '';
                    type = lib.types.listOf (lib.types.str);
                    default = [];
                };

                dontHaddockPackages = lib.mkOption {
                    description = ''
                        List of Haskell package names whose haddock is not built during app build.
                    '';
                    type = lib.types.listOf (lib.types.str);
                    default = [];
                };

                rtsFlags = lib.mkOption {
                    description = ''
                        GHC RTS Flags used for compiled binaries (unoptimized-prod-server and optimized-prod-server)
                    '';
                    type = lib.types.str;
                    default = "-A96m -N";
                };

                optimizationLevel = lib.mkOption {
                    description = ''
                        With optimizationLevel = 2, will pass -O2 to GHC when compiling optimized-prod-server
                    '';
                    type = lib.types.str;
                    default = "1";
                };

                static.extraDirs = lib.mkOption {
                    type = lib.types.attrsOf (lib.types.oneOf [ lib.types.path lib.types.package ]);
                    default = {};
                    description = "Map of subdir name -> derivation/path to be mounted under static/";
                    example = ''
                        {
                            Frontend = self.packages.${system}.frontend;   # -> /static/Frontend/...
                            Bootstrap = "${pkgs.bootstrap5}/dist";         # mount subdir of a package
                        }
                    '';
                };

                static.makeBundling = lib.mkOption {
                    type = lib.types.bool;
                    default = true;
                    description = ''
                        Whether to build static files using the Makefile provided by IHP.
                        If your app doesn't use the Makefile to bundle the CSS, you can disable this for faster builds.
                    '';
                };
            };
        }
    );

    config = {
        perSystem = { self', lib, pkgs, system, config, ... }: let
            cfg = config.ihp;
            ihp = ihpFlake.inputs.self;
            ghcCompiler = pkgs.ghc;
            hsDataDir = package:
                    let
                        ghcName   = package.passthru.compiler.haskellCompilerName;         # e.g. "ghc-9.8.4"
                        shareRoot = "${package.data}/share/${ghcName}";
                        # Pick the only dir ending with "-${ghcName}", e.g. "aarch64-osx-ghc-9.8.4"
                        sys = lib.head (lib.filter (n: lib.hasSuffix "-${ghcName}" n) (builtins.attrNames (builtins.readDir shareRoot)));
                    in
                        "${shareRoot}/${sys}/${package.name}";
        in lib.mkIf cfg.enable {
            _module.args.pkgs = import inputs.nixpkgs { inherit system; overlays = config.devenv.shells.default.overlays; config = { }; };

            # release build package
            packages = {
                default = self'.packages.unoptimized-prod-server;

                optimized-prod-server = import "${ihp}/NixSupport/default.nix" {
                    ihp = ihp;
                    haskellDeps = cfg.haskellPackages;
                    otherDeps = p: cfg.packages;
                    projectPath = cfg.projectPath;
                    # Set optimized = true to get more optimized binaries, but slower build times
                    optimized = true;
                    ghc = ghcCompiler;
                    pkgs = pkgs;
                    rtsFlags = cfg.rtsFlags;
                    optimizationLevel = cfg.optimizationLevel;
                    appName = cfg.appName;
                    filter = ihpFlake.inputs.nix-filter.lib;
                    ihp-env-var-backwards-compat = ihpFlake.inputs.self.packages.${system}.ihp-env-var-backwards-compat;
                    static = self'.packages.static;
                };

                unoptimized-prod-server = import "${ihp}/NixSupport/default.nix" {
                    ihp = ihp;
                    haskellDeps = cfg.haskellPackages;
                    otherDeps = p: cfg.packages;
                    projectPath = cfg.projectPath;
                    optimized = false;
                    ghc = ghcCompiler;
                    pkgs = pkgs;
                    rtsFlags = cfg.rtsFlags;
                    optimizationLevel = "0";
                    appName = cfg.appName;
                    filter = ihpFlake.inputs.nix-filter.lib;
                    ihp-env-var-backwards-compat = ihpFlake.inputs.self.packages.${system}.ihp-env-var-backwards-compat;
                    static = self'.packages.static;
                };

                static =
                    let
                        # Turn { Frontend = drv; Admin = drv2; } into a farm
                        extraStaticFarm = pkgs.linkFarm "${cfg.appName}-extra-static" (lib.mapAttrsToList (name: path: { inherit name path; }) cfg.static.extraDirs);
                    in
                        pkgs.symlinkJoin {
                            name = "${cfg.appName}-static";
                            paths = [
                                (if cfg.static.makeBundling
                                    then self'.packages.staticFilesCompiledByMake
                                    else (let filter = ihpFlake.inputs.nix-filter.lib; in filter { root = cfg.projectPath; include = ["static"]; name = "static-directory"; }) + "/static"
                                )
                                extraStaticFarm
                            ];
                        };

                unoptimized-docker-image = pkgs.dockerTools.buildImage {
                    name = "ihp-app";
                    config = { Cmd = [ "${self'.packages.unoptimized-prod-server}/bin/RunProdServer" ]; };
                };
                
                optimized-docker-image = pkgs.dockerTools.buildImage {
                    name = "ihp-app";
                    config = { Cmd = [ "${self'.packages.optimized-prod-server}/bin/RunProdServer" ]; };
                };


                migrate = ghcCompiler.ihp-migrate;

                ihp-schema = pkgs.stdenv.mkDerivation {
                    name = "ihp-schema";
                    src = ihp;
                    phases = [ "unpackPhase" "installPhase" ];
                    nativeBuildInputs = [ghcCompiler.ihp-ide.data];
                    installPhase = ''
                        mkdir $out
                        cp ${hsDataDir ghcCompiler.ihp-ide.data}/IHPSchema.sql $out/
                    '';
                    allowedReferences = [];
                };


                schema = pkgs.stdenv.mkDerivation {
                    name = "schema";
                    src = cfg.projectPath;
                    phases = [ "unpackPhase" "installPhase" ];
                    installPhase = ''
                        mkdir $out
                        cp Application/Schema.sql $out/
                    '';
                };
            } // (if cfg.static.makeBundling then {
                staticFilesCompiledByMake = pkgs.stdenv.mkDerivation {
                    name = "${config.ihp.appName}-staticFilesCompiledByMake";
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

                        export IHP_LIB=${ihpFlake.inputs.self.packages.${system}.ihp-env-var-backwards-compat}
                        export IHP=${ihpFlake.inputs.self.packages.${system}.ihp-env-var-backwards-compat}

                        make -j static/app.css static/app.js
                        runHook postBuild
                    '';
                    installPhase = ''
                        cp -R static/. $out
                    '';
                    src = pkgs.nix-gitignore.gitignoreSource [] cfg.projectPath;
                    buildInputs = cfg.packages;
                    nativeBuildInputs = builtins.concatLists [
                        [ pkgs.makeWrapper
                          pkgs.cacert # Needed for npm install to work from within the IHP build process
                        ]
                    ];
                    enableParallelBuilding = true;
                    impureEnvVars = pkgs.lib.fetchers.proxyImpureEnvVars; # Needed for npm install to work from within the IHP build process
                    disallowedReferences = [ ihp ]; # Prevent including the large full IHP source code
                };
            } else {})
            // (if builtins.pathExists "${cfg.projectPath}/Test/Main.hs"
                then {
                    tests = pkgs.stdenv.mkDerivation {
                            name = "${config.ihp.appName}-tests";
                            src = builtins.path { path = config.ihp.projectPath; name = "source"; };
                            nativeBuildInputs = with pkgs; [ (ghcCompiler.ghcWithPackages (p: cfg.haskellPackages p ++ [p.ihp-ide])) ];
                            buildPhase = ''
                                # shellcheck disable=SC2046
                                make -f ${hsDataDir ghcCompiler.ihp-ide.data}/lib/IHP/Makefile.dist build/Generated/Types.hs

                                # shellcheck disable=SC2046
                                runghc $(make -f ${hsDataDir ghcCompiler.ihp-ide.data}/lib/IHP/Makefile.dist print-ghc-extensions) -i. -ibuild -iConfig Test/Main.hs
                                touch $out
                            '';
                        };
                } else {});

            devenv.shells.default = lib.mkIf cfg.enable {
                packages = [ ghcCompiler.ihp ghcCompiler.ihp-ide pkgs.gnumake ihpFlake.inputs.self.packages."${system}".run-script ]
                    ++ cfg.packages
                    ++ [pkgs.mktemp] # Without this 'make build/bin/RunUnoptimizedProdServer' fails on macOS
                    ++ [(let cfg = config.devenv.shells.default.services.postgres; in
                        if cfg.extensions != null
                        then
                          if builtins.hasAttr "withPackages" cfg.package
                          then cfg.package.withPackages cfg.extensions
                          else
                            builtins.throw ''
                              Cannot add extensions to the PostgreSQL package.
                              `services.postgres.package` is missing the `withPackages` attribute. Did you already add extensions to the package?
                            ''
                        else cfg.package
                    )]
                    ;

                /*
                we currently don't use devenv containers, and they break nix flake show
                without the proper inputs set
                https://github.com/cachix/devenv/issues/528
                */
                containers = lib.mkForce {};

                languages.haskell.enable = true;
                languages.haskell.package = (if cfg.withHoogle
                                             then ghcCompiler.ghc.withHoogle
                                             else ghcCompiler.ghc.withPackages) cfg.haskellPackages;

                languages.haskell.stack.enable = false; # Stack is not used in IHP

                scripts.start.exec = ''
                    IHP_STATIC=${hsDataDir ghcCompiler.ihp.data}/static ${ghcCompiler.ihp-ide}/bin/RunDevServer
                '';

                processes.ihp.exec = "start";

                # Disabled for now
                # Can be re-enabled once postgres is provided by devenv instead of IHP
                # env.IHP_DEVENV = "1";
                # env.DATABASE_URL = "postgres:///app?host=${config.env.PGHOST}";

                # Disabled for now
                # As the devenv postgres uses a different location for the socket
                # this would break lots of known commands such as `make db`
                services.postgres.enable = false;
                services.postgres.package = pkgs.postgresql_13;
                services.postgres.initialDatabases = [
                    {
                    name = "app";
                    schema = pkgs.runCommand "ihp-schema" {} ''
                        touch $out

                        echo "-- IHPSchema.sql" >> $out
                        echo "" >> $out
                        cat ${ghcCompiler.ihp-ide.data + "/IHPSchema.sql"} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
                        echo "" >> $out
                        echo "-- Application/Schema.sql" >> $out
                        echo "" >> $out
                        cat ${cfg.projectPath + "/Application/Schema.sql"} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
                        echo "" >> $out
                        echo "-- Application/Fixtures.sql" >> $out
                        echo "" >> $out
                        cat ${cfg.projectPath + "/Application/Fixtures.sql"} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
                    '';
                    }
                ];

                # Used in the Makefile https://github.com/digitallyinduced/ihp-boilerplate/blob/master/Makefile
                env.IHP = ihpFlake.inputs.self.packages.${system}.ihp-env-var-backwards-compat;

                # Used by .ghci https://github.com/digitallyinduced/ihp-boilerplate/blob/master/.ghci
                env.IHP_LIB = config.devenv.shells.default.env.IHP;

                scripts.deploy-to-nixos.exec = ''
                    if [[ $# -eq 0 || $1 == "--help" ]]; then
                        echo "usage: deploy-to-nixos <target-host>"
                        echo "example: deploy-to-nixos staging.example.com"
                        exit 0
                    fi

                    ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch \
                        -j auto \
                        --use-substitutes \
                        --fast \
                        --flake .#$1 \
                        --target-host $1 \
                        --build-host $1 \
                        --option sandbox false \
                        --option extra-substituters "https://digitallyinduced.cachix.org" \
                        --option extra-trusted-public-keys "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
                    ssh $1 systemctl start migrate
                '';

                overlays = [ihp.overlays.default];
            };

            checks = (lib.filterAttrs (n: v:
                   n != "unoptimized-docker-image" && n != "optimized-docker-image" # Docker imagee builds are very slow, so we ignore them
                && n != "migrate"
                && lib.isDerivation v
                ) self.packages.${system});
        };

        # Nix requires this to be interactively allowed on first use for security reasons
        # This can be automated by using the --accept-flake-config flag
        # E.g. nix develop --accept-flake-config
        # Or by putting accept-flake-config = true into the system's nix.conf
        flake.nixConfig = {
            extra-substituters = "https://digitallyinduced.cachix.org";
            extra-trusted-public-keys = "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE=";
        };
    };

}
