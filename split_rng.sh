#!/usr/bin/env bash
set -euo pipefail

# Split the first occurrence of each MODULE and the first PROGRAM from src/rng.f90
# into separate files under src/, then remove the corrupted rng.f90.

root="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
src="$root/src"
tools="$root/tools"

[ -f "$src/rng.f90" ] || { echo "ERROR: $src/rng.f90 not found"; exit 1; }

mkdir -p "$tools"
cp "$src/rng.f90" "$src/rng.f90.bak"

awk -v outdir="$src" '
BEGIN {
  IGNORECASE=1
  in=0; keep=0; kind=""; name=""
}
function start_block(t,n,    path) {
  in=1; kind=t; name=tolower(n)
  if (t=="module") {
    if (!(name in seen_mod)) {
      keep=1; seen_mod[name]=1
      path=sprintf("%s/%s.f90", outdir, name)
      out=path; print "" > out
    } else keep=0
  } else if (t=="program") {
    if (!seen_prog) {
      keep=1; seen_prog=1
      out=sprintf("%s/main.f90", outdir)
      print "" > out
    } else keep=0
  }
}
function end_block() {
  if (in && keep) print $0 >> out
  in=0; keep=0; kind=""; name=""
}
# module start
/^[ \t]*module[ \t]+[A-Za-z_][A-Za-z0-9_]*/ {
  match($0,/^[ \t]*module[ \t]+([A-Za-z_][A-Za-z0-9_]*)/,m)
  start_block("module", m[1])
}
# program start
/^[ \t]*program[ \t]+[A-Za-z_][A-Za-z0-9_]*/ {
  match($0,/^[ \t]*program[ \t]+([A-Za-z_][A-Za-z0-9_]*)/,m)
  start_block("program", m[1])
}
# write inside kept block
{
  if (in && keep) print $0 >> out
}
# ends (with or without names)
/^[ \t]*end[ \t]+module([ \t]+[A-Za-z_][A-Za-z0-9_]*)?([ \t!].*)?$/ { if (in && kind=="module") end_block() }
/^[ \t]*end[ \t]+program([ \t]+[A-Za-z_][A-Za-z0-9_]*)?([ \t!].*)?$/ { if (in && kind=="program") end_block() }
' "$src/rng.f90"

# Remove the corrupted monolith
rm -f "$src/rng.f90"

echo "Done. Created:"
ls -1 "$src" | sed 's/^/  - /'

echo
echo "Tip: now run:"
echo "  make clean && make"
