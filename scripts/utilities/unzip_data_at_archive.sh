#!/bin/bash

# ==================================================
# Unzip Austrian Archive Fuel Price Data
# ==================================================
# This script unzips all ZIP files in data_at_archive/
# Each ZIP contains 9 TXT files (one per Austrian state)
# ==================================================

DATA_DIR="/Users/alexphan/Desktop/MBAT-Internship-Project/data_at_archive"

echo "=== Unzipping Austrian Archive Fuel Data ==="
echo ""

# Find all .zip files
ZIP_FILES=$(find "$DATA_DIR" -name "*.zip")
NUM_ZIP_FILES=$(echo "$ZIP_FILES" | wc -l | tr -d ' ')

echo "Found $NUM_ZIP_FILES ZIP files to process"
echo ""

UNZIPPED_COUNT=0
SKIPPED_COUNT=0

for zip_file in $ZIP_FILES; do
  # Get directory where the zip file is located
  zip_dir=$(dirname "$zip_file")
  zip_basename=$(basename "$zip_file" .zip)
  
  # Create a folder for this date's data
  extract_dir="${zip_dir}/${zip_basename}"
  
  # Check if already extracted
  if [ -d "$extract_dir" ]; then
    SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
  else
    # Extract to its own folder
    mkdir -p "$extract_dir"
    unzip -q "$zip_file" -d "$extract_dir"
    UNZIPPED_COUNT=$((UNZIPPED_COUNT + 1))
    
    # Show progress every 50 files
    if [ $((UNZIPPED_COUNT % 50)) -eq 0 ]; then
      echo "✅ Unzipped $UNZIPPED_COUNT files..."
    fi
  fi
done

echo ""
echo "=== Unzip Summary ==="
echo "✅ Unzipped: $UNZIPPED_COUNT files"
echo "⏭️  Skipped: $SKIPPED_COUNT files (already extracted)"
echo "📊 Total processed: $((UNZIPPED_COUNT + SKIPPED_COUNT)) files"
echo ""
echo "🎉 Unzip process completed!"




