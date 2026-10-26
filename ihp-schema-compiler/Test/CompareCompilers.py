"""Compare IHP compilers and verify generated output and cache invalidation."""

import argparse
import hashlib
import os
from pathlib import Path
import statistics
import subprocess
import tempfile
import time


FIXTURE = """
CREATE TYPE moods AS ENUM ('happy', 'very happy', 'sad');
CREATE TYPE other_moods AS ENUM ('happy', 'calm');
CREATE TYPE empty_enum AS ENUM ();
CREATE TABLE companies (id UUID PRIMARY KEY, name TEXT NOT NULL UNIQUE);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    company_id UUID,
    email TEXT NOT NULL UNIQUE,
    mood moods,
    previous_moods moods[],
    created_at TIMESTAMP WITH TIME ZONE NOT NULL
);
ALTER TABLE users ADD CONSTRAINT users_company_fk FOREIGN KEY (company_id) REFERENCES companies (id);
CREATE TABLE referrals (id UUID PRIMARY KEY, user_id UUID NOT NULL, referred_user_id UUID);
ALTER TABLE referrals ADD CONSTRAINT referrals_user_fk FOREIGN KEY (user_id) REFERENCES users (id);
ALTER TABLE referrals ADD CONSTRAINT referrals_referred_user_fk FOREIGN KEY (referred_user_id) REFERENCES users (id);
CREATE TABLE profiles (id UUID PRIMARY KEY, user_email TEXT, other_mood other_moods);
ALTER TABLE profiles ADD CONSTRAINT profiles_email_fk FOREIGN KEY (user_email) REFERENCES users (email);
CREATE TABLE audit_events (id BIGSERIAL PRIMARY KEY, user_id UUID NOT NULL, "raw/payload" TEXT);
COMMENT ON TABLE audit_events IS 'ihp:no-codegen';
ALTER TABLE audit_events ADD CONSTRAINT audit_user_fk FOREIGN KEY (user_id) REFERENCES users (id);
CREATE TABLE event_links (id UUID PRIMARY KEY, audit_event_id BIGINT);
ALTER TABLE event_links ADD CONSTRAINT event_links_audit_fk FOREIGN KEY (audit_event_id) REFERENCES audit_events (id);
CREATE TABLE memberships (user_id UUID NOT NULL, company_id UUID NOT NULL, PRIMARY KEY (user_id, company_id));
ALTER TABLE memberships ADD CONSTRAINT memberships_user_fk FOREIGN KEY (user_id) REFERENCES users (id);
ALTER TABLE memberships ADD CONSTRAINT memberships_company_fk FOREIGN KEY (company_id) REFERENCES companies (id);
CREATE TABLE base_records (id SERIAL PRIMARY KEY, title TEXT NOT NULL);
CREATE TABLE child_records (id SERIAL PRIMARY KEY, extra TEXT) INHERITS (base_records);
CREATE TABLE without_primary_key (value TEXT);
"""


def compile_schema(binary, directory, relations):
    environment = dict(os.environ, IHP_RELATION_SUPPORT=str(int(relations)))
    # Developer shells can set RTS limits that upstream's executable disallows.
    environment.pop("GHCRTS", None)
    start = time.perf_counter()
    result = subprocess.run(
        [binary], cwd=directory, env=environment, capture_output=True, text=True
    )
    elapsed = time.perf_counter() - start
    if result.returncode:
        raise AssertionError(result.stdout + result.stderr)
    return elapsed


def outputs(directory):
    return {
        str(path.relative_to(directory)): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in (directory / "build/Generated").rglob("*.hs")
    }


def check_invalidation(baseline, optimized, directories, relations):
    reference, actual = directories
    expected = outputs(reference)
    module = next((actual / "build/Generated").rglob("*.hs"))
    previous = module.stat()
    body = module.read_bytes()
    module.write_bytes(bytes([body[0] ^ 1]) + body[1:])
    os.utime(module, ns=(previous.st_atime_ns, previous.st_mtime_ns))
    compile_schema(optimized, actual, relations)
    assert outputs(actual) == expected, "Content change with preserved size/mtime was missed"
    module.unlink()
    compile_schema(optimized, actual, relations)
    assert outputs(actual) == expected, "Deleted module was not restored"
    cache = actual / "build/.ihp-schema-compiler-cache"
    cache.write_bytes(cache.read_bytes()[:-1])
    compile_schema(optimized, actual, relations)
    assert outputs(actual) == expected, "Truncated cache was not repaired"
    cache.write_bytes(b"invalid cache")
    compile_schema(optimized, actual, relations)
    assert outputs(actual) == expected, "Invalid cache was not repaired"
    # A change to schema text must invalidate the entire generation result.
    for directory in directories:
        schema = directory / "Application/Schema.sql"
        schema.write_bytes(schema.read_bytes() + b"\nCREATE TABLE cache_tests (id SERIAL PRIMARY KEY, name TEXT);\n")
    compile_schema(baseline, reference, relations)
    compile_schema(optimized, actual, relations)
    assert outputs(actual) == outputs(reference), "Schema edit did not invalidate the cache"
    compile_schema(baseline, reference, not relations)
    compile_schema(optimized, actual, not relations)
    assert outputs(actual) == outputs(reference), "Relation option did not invalidate the cache"


def compare(baseline, optimized, schema, name, relations, runs):
    with tempfile.TemporaryDirectory(prefix="ihp-schema-compiler-") as temp:
        root = Path(temp)
        directories = [root / "baseline", root / "optimized"]
        for directory in directories:
            (directory / "Application").mkdir(parents=True)
            (directory / "Application/Schema.sql").write_bytes(schema)
        durations = [[], []]
        for _ in range(runs):
            # Both compilers regenerate from an empty output directory each run.
            # Moving it aside keeps deletion out of the timed operation.
            for index, (binary, directory) in enumerate(zip([baseline, optimized], directories)):
                generated = directory / "build"
                if generated.exists():
                    generated.rename(directory / f"previous-{len(durations[index])}")
                durations[index].append(compile_schema(binary, directory, relations))
            expected, actual = map(outputs, directories)
            assert expected, f"{name}: no generated files"
            assert expected == actual, (
                f"{name}, relations={relations}: changed files: "
                + repr(sorted(key for key in expected.keys() | actual.keys() if expected.get(key) != actual.get(key)))
            )

        optimized_dir = directories[1]
        mtimes = {p: p.stat().st_mtime_ns for p in (optimized_dir / "build").rglob("*.hs")}
        cache = optimized_dir / "build/.ihp-schema-compiler-cache"
        assert cache.is_file(), "No cache was written"
        cache_mtime = cache.stat().st_mtime_ns
        warm = [[], []]
        for _ in range(runs):
            for index, (binary, directory) in enumerate(zip([baseline, optimized], directories)):
                warm[index].append(compile_schema(binary, directory, relations))
        assert all(p.stat().st_mtime_ns == mtime for p, mtime in mtimes.items()), "Unchanged files were rewritten"
        assert cache.stat().st_mtime_ns == cache_mtime, "Unchanged run missed the cache"
        before, after = map(statistics.median, durations)
        warm_before, warm_after = map(statistics.median, warm)
        print(
            f"{name}, relations={int(relations)}: {len(actual)} identical files; "
            f"fresh {before:.3f}s -> {after:.3f}s ({before / after:.2f}x); "
            f"unchanged {warm_before:.3f}s -> {warm_after:.3f}s ({warm_before / warm_after:.2f}x)",
            flush=True,
        )
        if name == "fixture":
            check_invalidation(baseline, optimized, directories, relations)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline")
    parser.add_argument("optimized")
    parser.add_argument("schema", type=Path, nargs="?", help="Optional SQL schema to benchmark in addition to the regression fixture")
    parser.add_argument("--runs", type=int, default=1)
    arguments = parser.parse_args()
    if arguments.runs < 1:
        parser.error("--runs must be positive")
    for relations in [False, True]:
        compare(arguments.baseline, arguments.optimized, FIXTURE.encode(), "fixture", relations, 1)
        if arguments.schema is not None:
            compare(arguments.baseline, arguments.optimized, arguments.schema.read_bytes(), "schema", relations, arguments.runs)


if __name__ == "__main__":
    main()
