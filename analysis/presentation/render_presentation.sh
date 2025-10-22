#!/bin/bash

# Script to render the Quarto presentation
# Usage: ./render_presentation.sh

echo "🎯 Rendering Fuel Price Prediction Presentation..."
echo "================================================"

# Check if Quarto is installed
if ! command -v quarto &> /dev/null; then
    echo "❌ Quarto is not installed. Please install Quarto first:"
    echo "   Visit: https://quarto.org/docs/getting-started/installation.html"
    exit 1
fi

# Check if the presentation file exists
if [ ! -f "fuel_price_prediction_presentation.qmd" ]; then
    echo "❌ Presentation file not found: fuel_price_prediction_presentation.qmd"
    exit 1
fi

echo "📊 Rendering presentation..."
quarto render fuel_price_prediction_presentation.qmd

if [ $? -eq 0 ]; then
    echo "✅ Presentation rendered successfully!"
    echo "📁 Output file: fuel_price_prediction_presentation.html"
    echo "🌐 Open the HTML file in your browser to view the presentation"
    
    # Try to open the presentation in the default browser (macOS)
    if command -v open &> /dev/null; then
        echo "🚀 Opening presentation in browser..."
        open fuel_price_prediction_presentation.html
    fi
else
    echo "❌ Error rendering presentation. Check the error messages above."
    exit 1
fi
