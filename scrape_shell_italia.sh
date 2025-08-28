#!/usr/bin/env bash
# Scrape all Shell stations in Italy

set -euo pipefail

COUNTRY="${COUNTRY:-it}"
LOCALE="${LOCALE:-it_IT}"
OUT_DIR="${OUT_DIR:-data_shell_italia}"
CACHE_DIR="${CACHE_DIR:-cache_shel_italial}"
SLEEP_SECS="${SLEEP_SECS:-1}"
BBOX_FILE="${BBOX_FILE:-}"  

TIMESTAMP="$(date +%Y-%m-%d_%H-%M)"
mkdir -p "$OUT_DIR" "$CACHE_DIR"

if ! command -v jq >/dev/null 2>&1; then
  echo "ERROR: jq not found. Please install jq." >&2
  exit 1
fi

########################################
# 0) Define Bounding Boxes for ALL of Italy
########################################
declare -a BBOXES
if [[ -n "$BBOX_FILE" && -f "$BBOX_FILE" ]]; then
  while IFS=, read -r sw_lat sw_lng ne_lat ne_lng; do
    sw_lat="$(echo "$sw_lat" | xargs)"
    sw_lng="$(echo "$sw_lng" | xargs)"
    ne_lat="$(echo "$ne_lat" | xargs)"
    ne_lng="$(echo "$ne_lng" | xargs)"
    [[ -z "$sw_lat$sw_lng$ne_lat$ne_lng" ]] && continue
    BBOXES+=("${sw_lat},${sw_lng},${ne_lat},${ne_lng}")
  done < "$BBOX_FILE"
else
  # Mainland
  BBOXES+=("36.0,6.5,39.0,10.5")
  BBOXES+=("36.0,10.5,39.0,14.5")
  BBOXES+=("39.0,6.5,42.0,10.5")
  BBOXES+=("39.0,10.5,42.0,14.5")
  BBOXES+=("42.0,6.5,45.0,10.5")
  BBOXES+=("42.0,10.5,45.0,14.5")
  BBOXES+=("45.0,6.5,47.5,10.5")
  BBOXES+=("45.0,10.5,47.5,14.5")
  BBOXES+=("44.0,14.5,47.5,18.5")  # Far northeast (Trieste region)

  # Islands
  BBOXES+=("38.5,8.0,41.5,9.8")    # Sardinia
  BBOXES+=("36.4,12.0,38.5,15.8")  # Sicily
fi

########################################
# 1) Discover Shell station IDs
########################################
IDS_FILE="${CACHE_DIR}/shell_ids_${COUNTRY}_${TIMESTAMP}.txt"
RAW_LOG="${CACHE_DIR}/within_bounds_${COUNTRY}_${TIMESTAMP}.log"
: > "$IDS_FILE"
: > "$RAW_LOG"

tile_idx=0
for bbox in "${BBOXES[@]}"; do
  IFS=',' read -r sw_lat sw_lng ne_lat ne_lng <<< "$bbox"
  ((tile_idx+=1))
  echo "[IDs] Tile ${tile_idx}: SW(${sw_lat},${sw_lng}) NE(${ne_lat},${ne_lng})"

  URL="https://shellgsllocator.geoapp.me/api/v2/locations/within_bounds?sw%5B%5D=${sw_lat}&sw%5B%5D=${sw_lng}&ne%5B%5D=${ne_lat}&ne%5B%5D=${ne_lng}&locale=${LOCALE}&format=json&driving_distances=false"
  RAW_JSON="${CACHE_DIR}/within_bounds_${COUNTRY}_${tile_idx}_${TIMESTAMP}.json"

  curl -sS "$URL" -H 'Accept: application/json' -H 'Connection: keep-alive' -o "$RAW_JSON"

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
echo "[IDs] Unique Shell IDs: ${TOTAL_IDS} (saved to $IDS_FILE)"

########################################
# 2) Fetch station details
########################################
echo "[DETAILS] Fetching station details for ${TOTAL_IDS} IDs (country=${COUNTRY})"
count=0
while read -r ID; do
  [[ -z "$ID" ]] && continue
  ((count+=1))
  printf "[DETAILS] (%d/%d) %s ... " "$count" "$TOTAL_IDS" "$ID"

  STATION_URL="https://find.shell.com/${COUNTRY}/fuel/${ID}.json"
  OUT_JSON="${OUT_DIR}/shell_${COUNTRY}_station_${ID}_${TIMESTAMP}.json"

  if curl -sS "$STATION_URL" -H 'Accept: application/json' -H 'Connection: keep-alive' -o "$OUT_JSON"; then
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