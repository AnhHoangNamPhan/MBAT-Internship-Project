#!/bin/bash

cd /home/nikolas/repos/tankerkaiser/

# Create timestamp in format YYYY-MM-DD_HH-MM-SS
TIMESTAMP=$(date +%Y-%m-%d_%H-%M)

# for REGION in {1..20}; do
#   curl "https://carburanti.mise.gov.it/ospzApi/registry/province?regionId=${REGION}" \
#   > "region-${REGION}.json"
# done

for REGION in {1..20}; do
  echo "Obtaining fuel data for region ${REGION}"
  curl 'https://carburanti.mise.gov.it/ospzApi/search/area' \
    -H 'Accept: application/json' \
    -H 'Connection: keep-alive' \
    -H 'Content-Type: application/json' \
    -H 'Origin: https://carburanti.mise.gov.it' \
    --data-raw "{\"region\":${REGION}}" \
    > "data_ita/fuel_region-${REGION}_${TIMESTAMP}.json"
  gzip "data_ita/fuel_region-${REGION}_${TIMESTAMP}.json"
  sleep 1
done

curl "http://kuma.kuschnig.eu/api/push/5czufRLUhV?status=up&msg=OK&ping="