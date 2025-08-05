#!/bin/bash

cd /home/nikolas/repos/tankerkaiser/

# Create timestamp in format YYYY-MM-DD_HH-MM-SS
TIMESTAMP=$(date +%Y-%m-%d_%H-%M)

# Pages beyond 23 are probably empty, but it doens't hurt to check them
for PAGE in {1..25}; do
  echo "Obtaining fuel data for page ${PAGE}"
  curl "https://goriva.si/api/v1/search/?position=&radius=&franchise=&name=&o=&page=${PAGE}" \
    -H 'Accept: application/json' \
    -H 'Connection: keep-alive' \
    -H 'Content-Type: application/json' \
    -H 'Referer: https://goriva.si/?position=&radius=&franchise=&name=&o=' \
    > "data_slo/fuel_page-${PAGE}_${TIMESTAMP}.json"
  gzip "data_slo/fuel_page-${PAGE}_${TIMESTAMP}.json"
  sleep 1
done

curl "http://kuma.kuschnig.eu/api/push/HmgrMY7MRB?status=up&msg=OK&ping="