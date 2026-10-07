#!/usr/bin/env bash
# Re-creates the committed TPTP sample used by the mutation tests (core/src/test/resources/corpus).
# The sample is part of this project in cooperation with TPTP (https://tptp.org).
# Files are taken from tptp.org's SeeTPTP pages; every axiom file a problem includes is fetched too.
set -euo pipefail

out="$(cd "$(dirname "$0")/.." && pwd)/core/src/test/resources/corpus"
problems=(
  PUZ001+1 PUZ031+2 SYN000+1 SYN036+2 GRP001+6 GRP396+1 NUM006+1 NUM292+1
  KRS018+1 SWV010+1 ALG014+1 GEO080+1 SET002+4 SET016+1 SET580+3
)

# Prints the text of a TPTP file shown on a SeeTPTP page (the <pre> block without HTML markup).
fetch() {
  curl -sfL "https://tptp.org/cgi-bin/SeeTPTP?$1" \
    | sed -n '/<pre>/,/<\/pre>/p' | sed -e '1s/.*<pre>//' -e '$s/<\/pre>.*//' \
    | sed -e 's/<[^>]*>//g' -e 's/&lt;/</g' -e 's/&gt;/>/g' -e 's/&quot;/"/g' -e 's/&amp;/\&/g'
}

rm -rf "$out" && mkdir -p "$out"
declare -A seen=()
fetch_axioms() {
  while read -r ax; do
    [[ -n "${seen[$ax]:-}" ]] && continue
    seen[$ax]=1
    mkdir -p "$out/$(dirname "$ax")"
    fetch "Category=Axioms&File=$(basename "$ax")" > "$out/$ax"
    echo "  $ax ($(wc -c < "$out/$ax") bytes)"
    fetch_axioms "$out/$ax"
  done < <(grep -oE "^include\('Axioms/[^']+'" "$1" | sed -E "s/^include\('//; s/'$//" || true)
}
for p in "${problems[@]}"; do
  domain="${p:0:3}"
  mkdir -p "$out/Problems/$domain"
  fetch "Category=Problems&Domain=$domain&File=$p.p" > "$out/Problems/$domain/$p.p"
  echo "Problems/$domain/$p.p ($(wc -c < "$out/Problems/$domain/$p.p") bytes)"
  fetch_axioms "$out/Problems/$domain/$p.p"
done
