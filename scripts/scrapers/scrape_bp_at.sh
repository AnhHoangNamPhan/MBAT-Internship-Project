#!/usr/bin/env bash
# Scrape all BP stations in Austria

set -euo pipefail

COUNTRY="at"
LOCALE="de_AT"
OUT_DIR="data_bp_at"
CACHE_DIR="cache_bp_at"
SLEEP_SECS=1
TIMESTAMP="$(date +%Y-%m-%d_%H-%M)"
mkdir -p "$OUT_DIR" "$CACHE_DIR"

if ! command -v jq >/dev/null 2>&1; then
  echo "ERROR: jq not found. Please install jq." >&2
  exit 1
fi

########################################
# 0) Define Bounding Boxes for Austria
########################################
declare -a BBOXES
BBOXES+=("46.4,9.5,47.7,13.5")   # SW
BBOXES+=("46.4,13.5,47.7,17.5")  # SE
BBOXES+=("47.7,9.5,49.0,13.5")   # NW
BBOXES+=("47.7,13.5,49.0,17.5")  # NE

########################################
# 1) Discover BP station IDs
########################################
IDS_FILE="${CACHE_DIR}/bp_ids_${COUNTRY}_${TIMESTAMP}.txt"
RAW_LOG="${CACHE_DIR}/within_bounds_${COUNTRY}_${TIMESTAMP}.log"
: > "$IDS_FILE"
: > "$RAW_LOG"

tile_idx=0
for bbox in "${BBOXES[@]}"; do
  IFS=',' read -r sw_lat sw_lng ne_lat ne_lng <<< "$bbox"
  ((tile_idx+=1))
  echo "[IDs] Tile ${tile_idx}: SW(${sw_lat},${sw_lng}) NE(${ne_lat},${ne_lng})"

  URL="https://bpretaillocator.geoapp.me/api/v2/locations/within_bounds?sw%5B%5D=${sw_lat}&sw%5B%5D=${sw_lng}&ne%5B%5D=${ne_lat}&ne%5B%5D=${ne_lng}&locale=${LOCALE}&format=json"
  RAW_JSON="${CACHE_DIR}/within_bounds_bp_${tile_idx}_${TIMESTAMP}.json"

  curl -sS "$URL" -H 'Accept: application/json' -o "$RAW_JSON"

  if jq -e '.locations | type == "array"' "$RAW_JSON" > /dev/null; then
    count=$(jq -r '.locations[].id' "$RAW_JSON" | tee -a "$IDS_FILE" | wc -l | xargs)
    echo "[IDs]   -> ${count} IDs from tile ${tile_idx}" | tee -a "$RAW_LOG"
  else
    echo "[WARN] No locations in tile ${tile_idx}" | tee -a "$RAW_LOG"
  fi

  sleep "$SLEEP_SECS"
done

sort -u "$IDS_FILE" -o "$IDS_FILE"
TOTAL_IDS=$(wc -l < "$IDS_FILE" | xargs)
echo "[IDs] Unique BP IDs: ${TOTAL_IDS} (saved to $IDS_FILE)"

########################################
# 2) Fetch station details
########################################
echo "[DETAILS] Fetching BP station details for ${TOTAL_IDS} IDs"
count=0
while read -r ID; do
  [[ -z "$ID" ]] && continue
  ((count+=1))
  printf "[DETAILS] (%d/%d) %s ... " "$count" "$TOTAL_IDS" "$ID"

  STATION_URL="https://bpretaillocator.geoapp.me/api/v2/locations/${ID}?locale=${LOCALE}&format=json"
  OUT_JSON="${OUT_DIR}/bp_${COUNTRY}_station_${ID}_${TIMESTAMP}.json"

  if curl -sS "$STATION_URL" -H 'Accept: application/json' -o "$OUT_JSON"; then
    if jq -e '.status == 404' "$OUT_JSON" > /dev/null 2>&1; then
      echo "404"
    else
      echo "ok"
      gzip -f "$OUT_JSON"
    fi
  else
    echo "FAIL"
  fi

  sleep "$SLEEP_SECS"
done < "$IDS_FILE"

echo "[DONE] Files written to: $OUT_DIR (gzipped) and $CACHE_DIR"