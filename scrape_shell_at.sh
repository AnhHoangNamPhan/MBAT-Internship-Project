#!/usr/bin/env bash

set -euo pipefail

########################
# Config (override via env)
########################
COUNTRY="${COUNTRY:-at}"        
LOCALE="${LOCALE:-de_AT}"        
OUT_DIR="${OUT_DIR:-data_shell}"
CACHE_DIR="${CACHE_DIR:-cache_shell}"
BBOX_FILE="${BBOX_FILE:-}"       
SLEEP_SECS="${SLEEP_SECS:-1}"  

TIMESTAMP="$(date +%Y-%m-%d_%H-%M)"

mkdir -p "$OUT_DIR" "$CACHE_DIR"

if ! command -v jq >/dev/null 2>&1; then
  echo "ERROR: jq not found. Please install jq." >&2
  exit 1
fi

########################################
# 0) Build bounding boxes
########################################
declare -a BBOXES
if [[ -n "$BBOX_FILE" && -f "$BBOX_FILE" ]]; then
  # Read bboxes from file
  while IFS=, read -r sw_lat sw_lng ne_lat ne_lng; do
    sw_lat="$(echo "$sw_lat" | xargs)"; sw_lng="$(echo "$sw_lng" | xargs)"
    ne_lat="$(echo "$ne_lat" | xargs)"; ne_lng="$(echo "$ne_lng" | xargs)"
    [[ -z "$sw_lat$sw_lng$ne_lat$ne_lng" ]] && continue
    BBOXES+=("${sw_lat},${sw_lng},${ne_lat},${ne_lng}")
  done < "$BBOX_FILE"
else
  # Default 4 tiles for Austria (2x2 grid)
  BBOXES+=("46.4,9.5,47.7,13.5")   # SW
  BBOXES+=("46.4,13.5,47.7,17.5")  # SE
  BBOXES+=("47.7,9.5,49.0,13.5")   # NW
  BBOXES+=("47.7,13.5,49.0,17.5")  # NE
fi

########################################
# 1) Discover station IDs (within_bounds)
########################################
IDS_FILE="${CACHE_DIR}/shell_ids_${TIMESTAMP}.txt"
RAW_LOG="${CACHE_DIR}/within_bounds_${TIMESTAMP}.log"
: > "$IDS_FILE"
: > "$RAW_LOG"

tile_idx=0
for bbox in "${BBOXES[@]}"; do
  IFS=',' read -r sw_lat sw_lng ne_lat ne_lng <<< "$bbox"
  ((tile_idx+=1))
  echo "[IDs] Tile ${tile_idx}: SW(${sw_lat},${sw_lng}) NE(${ne_lat},${ne_lng})"

  URL="https://shellgsllocator.geoapp.me/api/v2/locations/within_bounds?sw%5B%5D=${sw_lat}&sw%5B%5D=${sw_lng}&ne%5B%5D=${ne_lat}&ne%5B%5D=${ne_lng}&locale=${LOCALE}&format=json&driving_distances=false"

  RAW_JSON="${CACHE_DIR}/within_bounds_${tile_idx}_${TIMESTAMP}.json"
  curl -sS "$URL" \
    -H 'Accept: application/json' \
    -H 'Connection: keep-alive' \
    -o "$RAW_JSON"

  count=$(jq -r '.locations[].id' "$RAW_JSON" | tee -a "$IDS_FILE" | wc -l | xargs)
  echo "[IDs]   -> ${count} IDs from tile ${tile_idx}" | tee -a "$RAW_LOG"
  sleep "$SLEEP_SECS"
done

# Deduplicate IDs
sort -u "$IDS_FILE" -o "$IDS_FILE"
TOTAL_IDS=$(wc -l < "$IDS_FILE" | xargs)
echo "[IDs] Unique Shell IDs: ${TOTAL_IDS} (saved to $IDS_FILE)"

########################################
# 2) Fetch per-station details (find.shell.com)
########################################
echo "[DETAILS] Fetching station details for ${TOTAL_IDS} IDs (country=${COUNTRY})"
count=0
while read -r ID; do
  [[ -z "$ID" ]] && continue
  ((count+=1))
  printf "[DETAILS] (%d/%d) %s ... " "$count" "$TOTAL_IDS" "$ID"

  STATION_URL="https://find.shell.com/${COUNTRY}/fuel/${ID}.json"
  OUT_JSON="${OUT_DIR}/shell_${COUNTRY}_station_${ID}_${TIMESTAMP}.json"

  if curl -sS "$STATION_URL" \
        -H 'Accept: application/json' \
        -H 'Connection: keep-alive' \
        -o "$OUT_JSON"; then
    echo "ok"
    gzip -f "$OUT_JSON"
  else
    echo "FAIL"
  fi

  sleep "$SLEEP_SECS"
done < "$IDS_FILE"

echo "[DONE] Files written to: $OUT_DIR (gzipped) and $CACHE_DIR"