# Fuel Price Database - Central Europe

A comprehensive database of fuel prices across Central Europe, focusing on Germany, Austria, Italy, Slovenia. This project collects daily fuel price data from multiple sources to enable analysis of pricing trends and market dynamics.

## Project Goal

The main objective is to build a model to predict fuel prices given data information. The collected data serves as the foundation for developing machine learning models that can forecast fuel price movements across different regions and fuel types.

## Data Sources

### Austria

**ARBÖ and ÖAMTC** provide data from the E-Control API, which shows the fuel price on station level per region in Austria. Both sources are scraped to ensure redundancy and data reliability. ÖAMTC also includes e-fuel prices alongside standard fuel types.

**OMV, Avanti, and Diskont** stations are scraped using a specialized method that reads price information from images on their websites. This covers approximately 400+ stations across Austria with complete pricing data.

**LM Energy** operates around 28 stations, including several BP-branded locations. Their WordPress API provides complete station metadata including coordinates, addresses, and current fuel prices for all locations.

**Tankerkönig-Jet** manages about 156 Jet stations across Austria. Their API provides detailed information about each station including current prices and services.

**Shell and BP Austria** station networks are tracked for location data and station metadata. While not all stations consistently report prices, the data is collected to maintain a complete picture of the market.

**Historical Archive** data from spritvergleich.at provides past fuel prices from E-Control, useful for long-term trend analysis.

### Italy

The Italian government provides comprehensive fuel price data covering all 20 regions. This is the most complete source available for Italian fuel prices, with extensive station and price information.

Shell and BP networks in Italy are also tracked, providing additional coverage of these major brands across the country.

### Slovenia

Slovenia offers complete national fuel price data through a public API. The data is paginated across multiple pages and provides full station and price coverage.

### Germany

Tankerkönig provides the most comprehensive fuel price dataset available for Germany, accessible through their Azure DevOps repository. This represents complete market coverage with detailed pricing information.

## Current Status

The project currently maintains 13 active scrapers covering thousands of fuel stations across Austria, Italy, Slovenia, and Germany. Daily data collection runs smoothly, with data being incrementally added to the databases for ongoing analysis and model development.

