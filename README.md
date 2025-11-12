# Fuel Price Database — Central Europe

A comprehensive and harmonised database of retail fuel prices across Central Europe, focusing on **Germany, Austria, Italy, and Slovenia**.  
This project consolidates daily, station-level fuel price data from multiple public and semi-public sources to enable analysis of **pricing trends, spatial competition, and temporal dynamics**.

---

## Project Overview

This repository is part of a broader research effort on **transparent and interpretable fuel price modelling** across European markets.  
It combines data engineering, feature generation, and statistical modelling pipelines (GAM, XGBoost, DNN) to produce high-resolution estimates of retail diesel and gasoline prices under station level.

Key components include:

- **Data collection and harmonisation:** Standardising station-level data from heterogeneous national APIs and web sources.  
- **Modelling and evaluation:** Comparative fitting of linear, semi-parametric (GAM), and machine-learning (XGBoost, DNN) models using rolling 12-month training windows.  
- **Deployment and interpretability:** Integration with a Shiny web app for real-time estimation.

---

## Project Goals

- Build an **integrated, reproducible database** of station-level fuel prices across Central Europe.  
- Develop models capable of **forecasting short-term fuel price movements** with interpretable drivers (temporal lags, competition, geography).  
- Provide a framework for **continuous retraining and deployment** of predictive models through a user-facing Shiny application.

---

## Data Sources by Country

### Austria

- **ARBÖ and ÖAMTC APIs (E-Control)** — official price data at station level; redundancy ensures completeness.  
- **OMV, Avanti, and Diskont** — scraped price data extracted via image-based parsing.  
- **LM Energy** — WordPress API providing coordinates, addresses, and live prices.  
- **Tankerkönig-Jet** — ~156 stations with detailed service and pricing data.  
- **Shell and BP Austria** — network metadata to supplement market coverage.  
- **Historical Archive (spritvergleich.at)** — long-term E-Control price history for trend analysis.

### Italy

- **Ministero delle Imprese e del Made in Italy (MIMIT)** — full national coverage with daily updates for all 20 regions.  
- **Shell and BP Italy** — supplementary datasets providing brand-level validation.

### Slovenia

- **Public national API** — complete national dataset providing hourly station-level price updates.

### Germany

- **Tankerkönig (via Azure DevOps)** — comprehensive feed with near-complete station and price coverage; used for modelling regions such as Bavaria and North Rhine–Westphalia.

---

## Current Status

- Active scrapers: **13** covering thousands of stations across all four countries.  
- Daily data collection and harmonisation are fully automated.  
- Modelling pipelines operational for **Bavaria** and **North Rhine–Westphalia**, with ongoing generalisation to Central Europe.

---

## Methodological Framework

| Component | Description |
|------------|--------------|
| **Feature Engineering** | Generates temporal lags, spatial competition measures, and brand identifiers. |
| **Models** | Linear (LM-3), Generalised Additive Model (GAM), XGBoost, Deep Neural Network (DNN). |
| **Evaluation Metrics** | RMSE, BIC, and SHAP-based feature importance. |
| **Deployment** | Rolling retraining pipeline and Shiny web interface for real-time estimation. |

---

## License

Code in this repository is released under the [MIT License](./LICENSE).  
Written materials, figures, and reports are distributed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-green.svg)
