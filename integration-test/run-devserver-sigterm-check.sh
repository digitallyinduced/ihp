#!/usr/bin/env bash
set -euo pipefail

app_dir="$(pwd)/integration-test"
log_file="${TMPDIR:-/tmp}/run-devserver.log"
devserver_pid=""
ghci_pid=""

cleanup() {
    if [ -n "${ghci_pid:-}" ] && kill -0 "$ghci_pid" 2>/dev/null; then
        kill -KILL "$ghci_pid" 2>/dev/null || true
    fi

    if [ -n "${devserver_pid:-}" ] && kill -0 "$devserver_pid" 2>/dev/null; then
        kill -KILL "$devserver_pid" 2>/dev/null || true
    fi

    if [ -n "${PGDATA:-}" ]; then
        pg_ctl -D "$PGDATA" stop >/dev/null 2>&1 || true
    fi
}
trap cleanup EXIT

find_ghci_child() {
    ps -axo pid=,ppid=,command= | awk -v ppid="$devserver_pid" '
        $2 == ppid && $0 ~ /--interactive/ { print $1; exit }
    '
}

export HOME="${TMPDIR:-/tmp}/home"
mkdir -p "$HOME"

export PGDATA="${TMPDIR:-/tmp}/pgdata"
export PGHOST="${TMPDIR:-/tmp}/pghost"
mkdir -p "$PGHOST"
initdb -D "$PGDATA" --no-locale --encoding=UTF8
echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"
echo "listen_addresses = ''" >> "$PGDATA/postgresql.conf"
pg_ctl -D "$PGDATA" -l "${TMPDIR:-/tmp}/pg.log" start
createdb -h "$PGHOST" app

export DATABASE_URL="postgresql:///app?host=$PGHOST"
export IHP_BROWSER=true

cd "$app_dir"
"$RUN_DEVSERVER" >"$log_file" 2>&1 &
devserver_pid="$!"

for _ in $(seq 1 200); do
    ghci_pid="$(find_ghci_child || true)"
    if [ -n "$ghci_pid" ]; then
        break
    fi
    sleep 0.1
done

if [ -z "$ghci_pid" ]; then
    echo "RunDevServer never spawned the GHCi child" >&2
    cat "$log_file" >&2
    exit 1
fi

sleep 1

if grep -Eq 'modules (loaded|reloaded)\.|Server started' "$log_file"; then
    echo "RunDevServer reached steady state before SIGTERM; fixture is too fast" >&2
    cat "$log_file" >&2
    exit 1
fi

kill -TERM "$devserver_pid"

for _ in $(seq 1 100); do
    if ! kill -0 "$devserver_pid" 2>/dev/null; then
        break
    fi
    sleep 0.1
done

if kill -0 "$devserver_pid" 2>/dev/null; then
    echo "RunDevServer did not exit after SIGTERM" >&2
    cat "$log_file" >&2
    exit 1
fi

for _ in $(seq 1 20); do
    if ! kill -0 "$ghci_pid" 2>/dev/null; then
        break
    fi
    sleep 0.1
done

if kill -0 "$ghci_pid" 2>/dev/null; then
    echo "Orphaned GHCi process survived RunDevServer SIGTERM" >&2
    ps -o pid=,ppid=,command= -p "$ghci_pid" >&2 || true
    cat "$log_file" >&2
    exit 1
fi
