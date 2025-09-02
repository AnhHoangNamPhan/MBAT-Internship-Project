#!/usr/bin/env bash
# Scrape all Jet stations in Germany

set -euo pipefail

OUT_DIR="data_jet_germany"
CACHE_DIR="cache_jet_germany"
TIMESTAMP="$(date +%Y-%m-%d_%H-%M)"
mkdir -p "$OUT_DIR" "$CACHE_DIR"

# Dependency check
if ! command -v jq >/dev/null 2>&1; then
  echo "ERROR: jq not found. Please install jq." >&2
  exit 1
fi

if ! command -v pup >/dev/null 2>&1; then
  echo "ERROR: pup (HTML parser) not found. Install with: brew install pup" >&2
  exit 1
fi

echo "[STEP 1] Scraping station list from Jet Germany"
STATION_LIST_HTML="${CACHE_DIR}/jet_station_list_${TIMESTAMP}.html"
curl -sS "https://www.jet.de/tankstellen-suche" -o "$STATION_LIST_HTML"

echo "[STEP 2] Extracting station IDs from HTML"
STATION_IDS=$(pup 'a[href^="/tankstellen/id/"] attr{href}' < "$STATION_LIST_HTML" | sed -E 's#.*/id/([a-f0-9-]+).*#\1#' | sort -u)

TOTAL_IDS=$(echo "$STATION_IDS" | wc -l | xargs)
echo "[INFO] Found $TOTAL_IDS station IDs"

echo "$STATION_IDS" > "${CACHE_DIR}/jet_station_ids_${TIMESTAMP}.txt"

echo "[STEP 3] Fetching station details..."
count=0
for ID in $STATION_IDS; do
  ((count+=1))
  echo -n "[${count}/${TOTAL_IDS}] $ID ... "

  OUT_JSON="${OUT_DIR}/jet_station_${ID}_${TIMESTAMP}.json"
  STATION_URL="https://www.jet.de/api/stations/id/${ID}"

  if curl -sS "$STATION_URL" -o "$OUT_JSON"; then
    if jq -e '.id' "$OUT_JSON" > /dev/null 2>&1; then
      echo "ok"
      gzip -f "$OUT_JSON"
    else
      echo "invalid"
    fi
  else
    echo "error"
  fi
  sleep 0.3
done

echo "[DONE] Files written to $OUT_DIR and $CACHE_DIR"