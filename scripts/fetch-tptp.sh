#!/usr/bin/env bash
# Downloads the pinned TPTP library release once and unpacks the FOF problems and all axiom files,
# for the full mutation run and the corpus test. Prints the library directory.
set -euo pipefail
version="${TPTP_VERSION:-v9.3.1}"
cache="${XDG_CACHE_HOME:-$HOME/.cache}/tptp-lsp"
dir="$cache/TPTP-$version"
archive="$cache/TPTP-$version.tgz"
mkdir -p "$cache"
if [[ ! -d "$dir/Problems" ]]; then
  if [[ ! -f "$archive" ]]; then
    echo "Downloading TPTP $version (about 1 GB) to $archive ..." >&2
    curl -fL --progress-bar -o "$archive.part" "https://tptp.org/TPTP/Distribution/TPTP-$version.tgz"
    mv "$archive.part" "$archive"
  fi
  echo "Unpacking FOF problems and axioms ..." >&2
  tar -xzf "$archive" -C "$cache" --wildcards "TPTP-$version/Problems/*+*.p" "TPTP-$version/Axioms/*"
fi
echo "$dir"
