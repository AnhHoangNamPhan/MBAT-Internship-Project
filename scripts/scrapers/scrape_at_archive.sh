#!/bin/bash

base_url="https://archiv.spritvergleich.at"
outdir="./scraped_data/data_at_archive/2025"
mkdir -p "$outdir"

for month in {01..12}; do
  days=$(cal $month 2025 | awk 'NF {DAYS=$NF} END {print DAYS}')

  for day in $(seq -w 1 $days); do
    folder=$(printf "%02d.%02d.2025" "$day" "$month")

    diesel_url="$base_url/$folder/Dieselpreise/$folder.zip"
    diesel_file="$outdir/Diesel_2025-$month-$day.zip"

    benzin_url="$base_url/$folder/Benzinpreise/$folder.zip"
    benzin_file="$outdir/Benzin_2025-$month-$day.zip"

    # Function to download if file exists
    download_if_exists () {
      url=$1
      outfile=$2
      if curl --head --silent --fail "$url" > /dev/null; then
        echo "✅ Downloading: $url"
        wget -q -O "$outfile" "$url"
      else
        echo "❌ Not found: $url"
      fi
    }

    download_if_exists "$diesel_url" "$diesel_file"
    download_if_exists "$benzin_url" "$benzin_file"
  done
done