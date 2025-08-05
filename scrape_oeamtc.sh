#!/bin/bash

cd /home/nikolas/repos/tankerkaiser/

# Create timestamp in format YYYY-MM-DD_HH-MM-SS
TIMESTAMP=$(date +%Y-%m-%d_%H-%M)

echo "Obtaining fuel data"
curl 'https://www.oeamtc.at/routenplaner/api/gis-fuel/fuel/search?count=10000&include=LIST_HEADER,LIST_RESULTS,DATA_HEADER,DATA_GEODATA,DATA_OPENINGS,DATA_FACILITIES,DATA_OPERATOR,DATA_PRICES,DATA_RATINGS,DATA_UTILIZATION,DETAIL_HEADER,DATA_ACCESSIBILITY' \
  -X POST \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -H 'Origin: https://www.oeamtc.at' \
  --data-raw '{
    "geographic": {
      "boundingBox": {
        "longitudeMin": 9.0,
        "longitudeMax": 17.5,
        "latitudeMin": 46.0,
        "latitudeMax": 49.5
      }
    },
    "fuels": {
      "fuels": {
        "values": ["DIESEL", "GASOLINE", "GASOLINE_SUPER"]
      }
    }
  }' \
  > "data_alt/fuel_${TIMESTAMP}.json"

gzip "data_alt/fuel_${TIMESTAMP}.json"

echo "Obtaining e-fuel data"
curl 'https://www.oeamtc.at/routenplaner/api/gis-efuel/efuel/search?count=10000&include=DATA_GEODATA,DATA_PRICES,DATA_RATINGS,LIST_HEADER' \
  -X POST \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -H 'Origin: https://www.oeamtc.at' \
  --data-raw '{
    "geographic": {
      "boundingBox": {
        "longitudeMin": 9.0,
        "longitudeMax": 17.5,
        "latitudeMin": 46.0,
        "latitudeMax": 49.5
      }
    },
    "efuels": {
      "plugTypes": {}
    }
  }' \
  > "data_alt/e-fuel_${TIMESTAMP}.json"

gzip "data_alt/e-fuel_${TIMESTAMP}.json"

curl "http://kuma.kuschnig.eu/api/push/yocuEBSCDf?status=up&msg=OK&ping="