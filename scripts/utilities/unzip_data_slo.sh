#!/bin/bash

# Unzip Slovenian fuel data files
# This script unzips .json.gz files in the data_slo directory

echo "=== Unzipping Slovenian fuel data files ==="

# Change to the data_slo directory
cd data_slo

# Count total .gz files
total_gz=$(ls *.gz 2>/dev/null | wc -l)
echo "Found $total_gz compressed files"

if [ $total_gz -eq 0 ]; then
    echo "No .gz files found to unzip"
    exit 0
fi

# Count existing .json files
existing_json=$(ls *.json 2>/dev/null | wc -l)
echo "Found $existing_json existing .json files"

# Unzip files that don't have corresponding .json files
unzipped_count=0
skipped_count=0

for gz_file in *.gz; do
    # Get the corresponding .json filename
    json_file="${gz_file%.gz}"
    
    # Check if .json file already exists
    if [ -f "$json_file" ]; then
        echo "⏭️  Skipping $gz_file (already unzipped)"
        ((skipped_count++))
    else
        echo "📦 Unzipping $gz_file"
        if gzip -d "$gz_file"; then
            ((unzipped_count++))
        else
            echo "❌ Failed to unzip $gz_file"
        fi
    fi
done

echo ""
echo "=== Unzip Summary ==="
echo "✅ Unzipped: $unzipped_count files"
echo "⏭️  Skipped: $skipped_count files (already exist)"
echo "📊 Total processed: $((unzipped_count + skipped_count)) files"

# Count final .json files
final_json=$(ls *.json 2>/dev/null | wc -l)
echo "📁 Final .json files: $final_json"

echo ""
echo "🎉 Unzip process completed!"
