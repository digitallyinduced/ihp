#!/usr/bin/env bash
#
# Builds flag-variant forum servers and benchmarks HTTP latency with hyperfine.
# Run from ihp/Bench/:
#   ./run-runtime-bench.sh
#
# Uses --override-input ihp path:../.. to benchmark the local IHP checkout.
#
set -euo pipefail

CONFIGS=(
    default
    dicts-strict
    specialise-aggressively
    unbox-strict-fields
    late-specialise
    spec-constr-keen
    static-arg-transform
    no-full-laziness
    unfolding-use-threshold-40
    no-specialise
    ignore-interface-pragmas
    runtime-combo
)

FLAG_DESC=(
    "default|(none — baseline)"
    "dicts-strict|-fdicts-strict"
    "specialise-aggressively|-fspecialise-aggressively"
    "unbox-strict-fields|-funbox-strict-fields"
    "late-specialise|-flate-specialise"
    "spec-constr-keen|-fspec-constr-keen"
    "static-arg-transform|-fstatic-argument-transformation"
    "no-full-laziness|-fno-full-laziness"
    "unfolding-use-threshold-40|-funfolding-use-threshold=40"
    "no-specialise|-fno-specialise"
    "ignore-interface-pragmas|-fignore-interface-pragmas"
    "runtime-combo|dicts-strict + specialise-aggressively + unbox-strict-fields + spec-constr-keen"
)

echo "=== Building ${#CONFIGS[@]} configurations ==="
echo ""

# Build all configs in parallel
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

FAILED_CFGS=()
for i in "${!CONFIGS[@]}"; do
    cfg="${CONFIGS[$i]}"
    pid="${PIDS[$i]}"
    if wait "$pid"; then
        echo "  OK: $cfg"
    else
        echo "  FAILED: $cfg"
        FAILED_CFGS+=("$cfg")
    fi
done

if [ "${#FAILED_CFGS[@]}" -gt 0 ]; then
    echo ""
    echo "Failed builds: ${FAILED_CFGS[*]}"
    echo "Continuing with successful builds only."
fi

# Get tools
export PATH="$(nix build nixpkgs#hyperfine --print-out-paths --no-link)/bin:$PATH"
export PATH="$(nix build nixpkgs#jq --print-out-paths --no-link)/bin:$PATH"
PG_OUTPUTS=$(nix build nixpkgs#postgresql --print-out-paths --no-link)
PG_OUT=$(echo "$PG_OUTPUTS" | grep -v '\-lib' | grep -v '\-man' | grep -v '\-doc' | head -1)
export PATH="$PG_OUT/bin:$PATH"

# Setup PostgreSQL
PGHOST="$(mktemp -d)"
PGDATA="$(mktemp -d)"
echo "Setting up PostgreSQL (socket: $PGHOST, data: $PGDATA)"
initdb -D "$PGDATA" --no-locale --encoding=UTF8 >/dev/null
echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"
echo "listen_addresses = ''" >> "$PGDATA/postgresql.conf"
pg_ctl -D "$PGDATA" -l pg.log start >/dev/null

cleanup() {
    pg_ctl -D "$PGDATA" stop >/dev/null 2>&1 || true
    rm -rf "$PGDATA" "$PGHOST"
}
trap cleanup EXIT

echo ""
echo "=== Running HTTP latency benchmarks ==="
echo ""

BASE_PORT=8100
declare -A MEAN_MS MEDIAN_MS MIN_MS MAX_MS STDDEV_MS

for i in "${!CONFIGS[@]}"; do
    cfg="${CONFIGS[$i]}"
    result_dir="result-${cfg}"

    # Skip failed builds
    if [ ! -f "${result_dir}/bin/forum-server" ]; then
        echo "Skipping $cfg (no binary)"
        continue
    fi

    PORT=$((BASE_PORT + i))
    DB_NAME="forum_bench_${i}"

    echo "--- $cfg (port $PORT) ---"

    # Create database and load schema
    dropdb -h "$PGHOST" --if-exists "$DB_NAME" 2>/dev/null
    createdb -h "$PGHOST" "$DB_NAME"
    psql -h "$PGHOST" -d "$DB_NAME" -f "$(readlink -f "$result_dir")/IHPSchema.sql" >/dev/null
    psql -h "$PGHOST" -d "$DB_NAME" -f "$(readlink -f "$result_dir")/Application/Schema.sql" >/dev/null

    # Start server
    DATABASE_URL="postgresql:///$DB_NAME?host=$PGHOST" \
    PORT=$PORT \
    IHP_SESSION_SECRET="benchmarkkeybenchmarkkeybenchmarkkeybenchmarkkey" \
    IHP_BASEURL="http://localhost:$PORT" \
        "$(readlink -f "$result_dir")/bin/forum-server" +RTS -N2 -A64m -RTS &
    SERVER_PID=$!

    # Wait for ready
    for attempt in $(seq 1 30); do
        curl -sf "http://localhost:$PORT/" >/dev/null 2>&1 && break
        sleep 0.5
    done

    if ! curl -sf "http://localhost:$PORT/" >/dev/null 2>&1; then
        echo "  Server failed to start, skipping"
        kill $SERVER_PID 2>/dev/null; wait $SERVER_PID 2>/dev/null || true
        continue
    fi

    # Benchmark
    hyperfine \
        --warmup 100 \
        --runs 500 \
        --export-json "bench-${cfg}.json" \
        "curl -sf http://localhost:$PORT/" 2>&1 | grep -E 'Time|Range'

    # Extract metrics
    eval "$(jq -r '.results[0] | "MEAN_MS[\(env.cfg)]=\(.mean*1000) MEDIAN_MS[\(env.cfg)]=\(.median*1000) MIN_MS[\(env.cfg)]=\(.min*1000) MAX_MS[\(env.cfg)]=\(.max*1000) STDDEV_MS[\(env.cfg)]=\(.stddev*1000)"' \
        --arg cfg "$cfg" "bench-${cfg}.json" 2>/dev/null || echo "")"

    kill $SERVER_PID 2>/dev/null; wait $SERVER_PID 2>/dev/null || true
    echo ""
done

# Print results table
echo ""
echo "## Runtime + Compile Benchmark Results"
echo ""
echo "| Configuration | Mean (ms) | Median (ms) | Stddev (ms) | Min (ms) | Max (ms) | Core Size | Core % | Flags |"
echo "|---------------|-----------|-------------|-------------|----------|----------|-----------|--------|-------|"

BASELINE_CORE=""
BASELINE_MEAN=""
if [ -f "result-default/core-size" ]; then
    BASELINE_CORE=$(cat result-default/core-size)
fi

for entry in "${FLAG_DESC[@]}"; do
    cfg="${entry%%|*}"
    flags="${entry#*|}"
    result_dir="result-${cfg}"

    if [ ! -f "${result_dir}/core-size" ]; then
        echo "| ${cfg} | FAILED | - | - | - | - | FAILED | - | \`${flags}\` |"
        continue
    fi

    core=$(cat "${result_dir}/core-size")

    if [ -n "$BASELINE_CORE" ] && [ "$BASELINE_CORE" -gt 0 ] && [ "$cfg" != "default" ]; then
        core_pct=$(awk "BEGIN { printf \"%.1f\", ($core - $BASELINE_CORE) / $BASELINE_CORE * 100 }")%
    else
        core_pct="—"
    fi

    mean="${MEAN_MS[$cfg]:-—}"
    median="${MEDIAN_MS[$cfg]:-—}"
    stddev="${STDDEV_MS[$cfg]:-—}"
    min="${MIN_MS[$cfg]:-—}"
    max="${MAX_MS[$cfg]:-—}"

    if [ "$mean" != "—" ]; then
        mean=$(printf "%.2f" "$mean")
        median=$(printf "%.2f" "$median")
        stddev=$(printf "%.2f" "$stddev")
        min=$(printf "%.2f" "$min")
        max=$(printf "%.2f" "$max")
    fi

    echo "| ${cfg} | ${mean} | ${median} | ${stddev} | ${min} | ${max} | ${core} | ${core_pct} | \`${flags}\` |"
done

echo ""
echo "Done."
