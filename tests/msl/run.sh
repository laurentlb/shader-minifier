#!/bin/bash
# Minify each tests/msl/*.metal with --msl and verify the output compiles
# with `xcrun metal -c`. Exits non-zero on the first failure.
set -u
cd "$(dirname "$0")/../.."

CLI=artifacts/bin/ShaderMinifier/release/ShaderMinifier.dll
# List of shaders currently expected to pass. ref_param.metal is known to
# fail due to an orthogonal renamer bug; re-add once that is fixed.
PASS=(atomic const_array texture_write)
KNOWN_FAIL=(ref_param)

fails=0
for base in "${PASS[@]}"; do
  src="tests/msl/${base}.metal"
  min="/tmp/${base}.min.metal"
  dotnet "$CLI" --msl --format text "$src" -o "$min" >/dev/null 2>&1
  if ! xcrun metal -c "$min" -o /tmp/out.air 2>/tmp/err; then
    echo "FAIL: $base"
    sed 's/^/    /' /tmp/err
    fails=$((fails+1))
  else
    echo "PASS: $base"
  fi
done

for base in "${KNOWN_FAIL[@]}"; do
  src="tests/msl/${base}.metal"
  min="/tmp/${base}.min.metal"
  dotnet "$CLI" --msl --format text "$src" -o "$min" >/dev/null 2>&1
  if xcrun metal -c "$min" -o /tmp/out.air 2>/dev/null; then
    echo "UNEXPECTED PASS: $base (remove from KNOWN_FAIL)"
    fails=$((fails+1))
  else
    echo "xfail:  $base"
  fi
done

exit $fails
