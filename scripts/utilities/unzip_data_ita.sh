#!/usr/bin/env bash
# Unzip all .gz files in data_ita directory

set -euo pipefail

DATA_DIR="data_ita"
TIMESTAMP="$(date +%Y-%m-%d_%H-%M)"

echo "🗜️  Unzipping all .gz files in $DATA_DIR directory..."
echo "Started at: $(date)"

# Check if directory exists
if [[ ! -d "$DATA_DIR" ]]; then
    echo "❌ Error: Directory $DATA_DIR does not exist!"
    exit 1
fi

# Count total .gz files
TOTAL_GZ=$(find "$DATA_DIR" -name "*.json.gz" | wc -l | xargs)
echo "📊 Found $TOTAL_GZ .gz files to unzip"

if [[ $TOTAL_GZ -eq 0 ]]; then
    echo "ℹ️  No .gz files found in $DATA_DIR"
    exit 0
fi

# Create log file
LOG_FILE="unzip_data_ita_${TIMESTAMP}.log"
echo "📝 Logging to: $LOG_FILE"

# Counter for progress
count=0
success_count=0
error_count=0

# Process each .gz file
find "$DATA_DIR" -name "*.json.gz" | while read -r gz_file; do
    ((count++))
    
    # Get the base filename without .gz extension
    json_file="${gz_file%.gz}"
    
    # Check if .json file already exists
    if [[ -f "$json_file" ]]; then
        echo "[$count/$TOTAL_GZ] ⚠️  Skipping $gz_file (already exists: $json_file)" | tee -a "$LOG_FILE"
        continue
    fi
    
    # Unzip the file
    printf "[%d/%d] Unzipping %s ... " "$count" "$TOTAL_GZ" "$(basename "$gz_file")"
    
    if gunzip -c "$gz_file" > "$json_file" 2>/dev/null; then
        echo "✅ OK" | tee -a "$LOG_FILE"
        ((success_count++))
    else
        echo "❌ FAILED" | tee -a "$LOG_FILE"
        ((error_count++))
        # Remove the failed file if it was created
        [[ -f "$json_file" ]] && rm -f "$json_file"
    fi
done

echo ""
echo "🎉 Unzipping completed!"
echo "📊 Summary:"
echo "   ✅ Successfully unzipped: $success_count files"
echo "   ❌ Failed: $error_count files"
echo "   📁 Total processed: $count files"
echo "   📝 Log saved to: $LOG_FILE"
echo "   ⏰ Finished at: $(date)"

# Show disk usage
echo ""
echo "💾 Disk usage:"
du -sh "$DATA_DIR" 2>/dev/null || echo "Could not calculate disk usage"

echo ""
echo "🔍 Sample of unzipped files:"
find "$DATA_DIR" -name "*.json" -not -name "*.gz" | head -5 | while read -r file; do
    size=$(du -h "$file" | cut -f1)
    echo "   📄 $(basename "$file") ($size)"
done
