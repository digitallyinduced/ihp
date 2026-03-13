#!/usr/bin/env bash
#
# Builds all flag-configuration benchmarks and prints a comparison table.
# Run from ihp/Bench/:
#   ./run-flag-bench.sh
#
# Uses --override-input ihp path:../.. to benchmark the local IHP checkout.
#
set -euo pipefail

CONFIGS=(
    baseline
    dicts-strict
    specialise-aggressively
    expose-unfoldings
    unfolding-threshold-120
    static-arg-transform
    late-specialise
    spec-constr-keen
    unbox-strict-fields
    full-combo
)

echo "=== Building ${#CONFIGS[@]} flag configurations ==="
echo ""

PIDS=()
for cfg in "${CONFIGS[@]}"; do
    echo "Starting build: $cfg"
    nix build ".#${cfg}" \
        --override-input ihp path:../.. \
        -o "result-${cfg}" \
        --impure \
        -L &
    PIDS+=($!)
done

echo ""
echo "Waiting for all builds to finish..."

FAILED=0
for i in "${!CONFIGS[@]}"; do
    cfg="${CONFIGS[$i]}"
    pid="${PIDS[$i]}"
    if wait "$pid"; then
        echo "  OK: $cfg"
    else
        echo "  FAILED: $cfg"
        FAILED=1
    fi
done

if [ "$FAILED" = "1" ]; then
    echo ""
    echo "Some builds failed. Showing results for successful builds only."
fi

echo ""

# Read baseline values for comparison
BASELINE_CORE=""
BASELINE_ALLOC=""
if [ -f "result-baseline/core-size" ]; then
    BASELINE_CORE=$(cat result-baseline/core-size)
    BASELINE_ALLOC=$(cat result-baseline/compile-allocations)
fi

# Print markdown table
echo "## Flag Benchmark Results"
echo ""
echo "| Configuration | Core Size | Core % | Compile Allocs | Allocs % | Flags |"
echo "|---------------|-----------|--------|----------------|----------|-------|"

FLAG_DESC=(
    "baseline|(none)"
    "dicts-strict|-fdicts-strict"
    "specialise-aggressively|-fspecialise-aggressively"
    "expose-unfoldings|-fexpose-all-unfoldings"
    "unfolding-threshold-120|-funfolding-use-threshold=120"
    "static-arg-transform|-fstatic-argument-transformation"
    "late-specialise|-flate-specialise"
    "spec-constr-keen|-fspec-constr-keen"
    "unbox-strict-fields|-funbox-strict-fields"
    "full-combo|all combined"
)

for entry in "${FLAG_DESC[@]}"; do
    cfg="${entry%%|*}"
    flags="${entry#*|}"

    result_dir="result-${cfg}"
    if [ ! -f "${result_dir}/core-size" ]; then
        echo "| ${cfg} | FAILED | - | FAILED | - | \`${flags}\` |"
        continue
    fi

    core=$(cat "${result_dir}/core-size")
    alloc=$(cat "${result_dir}/compile-allocations")

    if [ -n "$BASELINE_CORE" ] && [ "$BASELINE_CORE" -gt 0 ]; then
        core_pct=$(awk "BEGIN { printf \"%.1f\", ($core - $BASELINE_CORE) / $BASELINE_CORE * 100 }")
        alloc_pct=$(awk "BEGIN { printf \"%.1f\", ($alloc - $BASELINE_ALLOC) / $BASELINE_ALLOC * 100 }")

        # Add +/- prefix for clarity
        if [ "$cfg" = "baseline" ]; then
            core_pct="—"
            alloc_pct="—"
        else
            core_pct="${core_pct}%"
            alloc_pct="${alloc_pct}%"
        fi
    else
        core_pct="—"
        alloc_pct="—"
    fi

    echo "| ${cfg} | ${core} | ${core_pct} | ${alloc} | ${alloc_pct} | \`${flags}\` |"
done

echo ""
echo "Done."
