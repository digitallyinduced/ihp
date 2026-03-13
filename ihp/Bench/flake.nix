{
    description = "Core size benchmark for IHP using ihp-forum";

    inputs = {
        ihp.url = "github:digitallyinduced/ihp";
        ihp-forum.url = "github:digitallyinduced/ihp-forum/feature/upgrade-to-latest-ihp";
        nixpkgs.follows = "ihp/nixpkgs";
    };

    outputs = { self, ihp, ihp-forum, nixpkgs }:
    let
        systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
        forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in {
        packages = forAllSystems (system:
            let
                lib = nixpkgs.lib;

                mkPkgs = ihpFlake: import nixpkgs {
                    inherit system;
                    overlays = [ ihpFlake.overlays.default ];
                    config = { };
                };

                pkgs = mkPkgs ihp;

                hsDataDir = package:
                    let
                        ghcName   = package.passthru.compiler.haskellCompilerName;
                        shareRoot = "${package.data}/share/${ghcName}";
                        dirs = builtins.filter (d: d != "doc") (builtins.attrNames (builtins.readDir shareRoot));
                        sys = lib.head dirs;
                    in
                        "${shareRoot}/${sys}/${package.name}";

                mkForumCoreSizeBench = name: ghcPkgs: pkgs.stdenv.mkDerivation {
                    inherit name;
                    src = ihp-forum;
                    nativeBuildInputs = [
                        (ghcPkgs.ghc.withPackages (p: with p; [
                            ihp ihp-mail ihp-schema-compiler
                            base wai text mmark mmark-ext wreq neat-interpolation
                        ]))
                        pkgs.gnumake
                        pkgs.gawk
                    ];
                    buildPhase = ''
                        export IHP_LIB=${hsDataDir ghcPkgs.ihp-ide.data}

                        # Generate types from Schema.sql
                        make -f $IHP_LIB/lib/IHP/Makefile.dist build/Generated/Types.hs

                        # Get GHC extensions
                        GHC_EXTS=$(make -f $IHP_LIB/lib/IHP/Makefile.dist print-ghc-extensions | sed 's/-fbyte-code//g')

                        # Compile with -O1 and dump Core
                        mkdir -p dumps
                        ghc --make \
                            $GHC_EXTS \
                            -O1 -ddump-simpl -ddump-to-file -dumpdir dumps \
                            -fforce-recomp \
                            -i. -ibuild -iConfig \
                            -package-env - \
                            -package ihp -package ihp-mail \
                            -Wno-partial-fields \
                            Main.hs -o /dev/null \
                            +RTS -s 2>ghc-rts-stats.txt

                        # Extract compile allocations (deterministic metric)
                        grep 'bytes allocated in the heap' ghc-rts-stats.txt \
                            | sed 's/,//g' | awk '{print $1}' > compile-allocations

                        # Measure total Core size
                        TOTAL=$(find dumps -name '*.dump-simpl' -exec cat {} + | wc -c | tr -d ' ')
                        echo "$TOTAL" > core-size

                        # Per-module breakdown (CSV sorted by size descending)
                        echo "module,bytes" > modules.csv
                        find dumps -name '*.dump-simpl' | while read -r f; do
                            MOD=$(echo "$f" | sed 's|^dumps/||; s|\.dump-simpl$||; s|/|.|g')
                            SIZE=$(wc -c < "$f" | tr -d ' ')
                            echo "$MOD,$SIZE" >> modules.csv
                        done
                        sort -t, -k2 -rn -o modules.csv modules.csv
                    '';
                    installPhase = ''
                        mkdir -p $out
                        cp core-size modules.csv compile-allocations $out/
                    '';
                };

                masterPkgs = mkPkgs (builtins.getFlake "github:digitallyinduced/ihp");
            in {
                default = mkForumCoreSizeBench "forum-core-size-bench" pkgs.ghc;
                baseline = mkForumCoreSizeBench "forum-core-size-bench-baseline" masterPkgs.ghc;
            }
        );
    };

    nixConfig = {
        extra-trusted-public-keys = "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE=";
        extra-substituters = "https://digitallyinduced.cachix.org";
    };
}
