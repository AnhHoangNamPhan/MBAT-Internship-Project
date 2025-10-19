# Fuel Brand Categorization Reference

**Document Purpose**: Reference for categorizing fuel brands in the unified multi-country dataset  
**Last Updated**: October 16, 2025  
**Data Sources**: Germany, Austria, Italy, Slovenia fuel price databases

---

## Categorization Strategy

We use **International/National/Regional/Unknown** categorization based on:
1. **Actual presence in our dataset** (verified across countries)
2. **Market presence and operations** (known international vs. local brands)
3. **Station count** within each country

---

## Category Definitions

### 1. **International** 
Major multinational oil companies operating across multiple European countries

**Criteria**: 
- Operates in 2+ countries in our dataset, OR
- Known global/European major brand

**Brands**:
- **Shell**: Present in Germany, Austria, Italy (verified in dataset)
- **BP**: Present in Austria (in dataset), known international brand
- **Esso**: Present in Germany, Italy (verified in dataset)
- **Total/TotalEnergies**: Present in Germany, known international brand
- **OMV**: Present in Austria, known Central European international brand
- **Eni/AgipEni**: Present in Germany, Austria, Italy (verified in dataset)
- **Q8**: Present in Italy, known European brand (Kuwait Petroleum)

### 2. **National**
Major brands primarily operating in one country, with significant market share

**Criteria**:
- Primarily operates in one country
- Large station network (>500 stations in Germany, >200 in other countries)

**Brands**:
- **ARAL** (Germany): 2,458 stations, German market leader
- **Tamoil** (Italy): 1,509 stations, major Italian brand
- **Api-Ip** (Italy): 3,864 stations, major Italian brand
- **PompeBianche** (Italy): 3,305 stations, Italian independent network

### 3. **Regional**
Regional chains or mid-size brands

**Criteria**:
- Operates regionally within country/countries
- Moderate station network (100-700 in Germany, 50-200 in other countries)

**Brands**:
- **JET**: Present in Germany (716), Austria (156) - regional player
- **AVIA**: Present in Germany (727), Austria (92) - regional network
- **HEM** (Germany): 428 stations
- **STAR** (Germany): 497 stations
- **BFT** (Germany): 491 stations (combined)
- **Q1** (Germany): 124 stations
- **Raiffeisen** (Austria/Germany): Regional cooperative

### 4. **Unknown**
No brand information available or unidentified stations

---

## Brand Presence by Country (from Dataset)

### Germany (Tankerkönig Database)
| Brand | Stations | Category |
|-------|----------|----------|
| ARAL | 2,458 | National |
| Shell | 1,881 | International |
| ESSO | 1,304 | International |
| TotalEnergies | 785 | International |
| AVIA | 727 | Regional |
| JET | 716 | Regional |
| STAR | 497 | Regional |
| ENI | 497 | International |
| Raiffeisen | 462 | Regional |
| HEM | 428 | Regional |

### Austria (ARBÖ Database)
| Brand | Stations | Category |
|-------|----------|----------|
| BP | 186 | International |
| JET | 144 | Regional |
| OMV | 142 | International |
| Shell | 140 | International |
| AVIA | 92 | Regional |
| ENI | 52 | International |
| Unknown | 1,675 | Unknown |

### Austria (OMV Dedicated)
- **OMV**: 419 stations (International)

### Austria (JET Dedicated)
- **JET**: 156 stations (Regional)

### Italy (National Database)
| Brand | Stations | Category |
|-------|----------|----------|
| AgipEni | 3,992 | International |
| Api-Ip | 3,864 | National |
| PompeBianche | 3,305 | National |
| Q8 | 2,773 | International |
| Esso | 2,109 | International |
| Tamoil | 1,509 | National |
| Shell | 155 | International |

### Slovenia (National Database)
| Franchise ID | Stations | Category | Brand Name (Verified) |
|--------------|----------|----------|----------------------|
| 1 | 306 | National | Petrol (Slovenia's national brand) |
| 4 | 133 | International | MOL (Hungarian Oil & Gas) |
| 5 | 48 | International | Shell (Royal Dutch Shell) |
| 3 | 21 | Regional | Maxen (automated 24/7 service) |
| 6 | 1 | Regional | Eko-Nafta |
| 11 | 4 | Regional | Eco Oil |
| 15 | 2 | Regional | Discont Oil |
| 16 | 2 | Regional | Agas |
| 19 | 9 | Regional | Local Services |
| Others | 12 | Regional | Various smaller operators |
| **Total** | **538** | | |

**Verified Sources:**
- Petrol: ~300 stations, Slovenia's leading fuel retailer (evignetteslovenia.si)
- MOL: 100+ stations, significant market share (evignetteslovenia.si)
- Shell: International brand operating in Slovenia
- Maxen: Automated fuel service operating 24/7 (evignetteslovenia.si)

---

## Categorization Logic in Code

```r
# International brands (operate in multiple countries)
international_brands <- c(
  'Shell', 'BP', 'Esso', 'ESSO', 'Total', 'TotalEnergies', 'TOTAL',
  'OMV', 'Eni', 'ENI', 'AgipEni', 'AGIP ENI', 'Q8'
)

# National brands (major in one country)
national_brands <- c(
  'ARAL',           # Germany
  'Api-Ip',         # Italy
  'PompeBianche',   # Italy
  'Tamoil'          # Italy
)

# Regional brands (mid-size, regional presence)
regional_brands <- c(
  'JET', 'AVIA', 'AVIA XPress',
  'STAR', 'HEM', 'BFT', 'bft', 'Q1',
  'Raiffeisen', 'Westfalen', 'Hoyer', 'Sprint', 'BayWa', 'ED', 'team',
  'OIL!', 'ORLEN', 'CLASSIC',
  'Europam', 'SanMarcoPetroli', 'Retitalia', 'Energas', 'KEROPETROL'
)
```

---

## Notes and Considerations

1. **Cross-Country Consistency**: International brands (Shell, BP, OMV, Eni) maintain similar market positioning across countries

2. **Market Size Differences**: 
   - Germany: ~14,000 stations total
   - Austria: ~2,500 stations total (estimated)
   - Italy: ~20,000+ stations total (estimated)
   - Slovenia: ~500 stations total

3. **Data Limitations**:
   - Some brands may appear only in certain data sources
   - Station counts may vary by data source and time period
   - Slovenian data uses numeric codes requiring mapping

4. **Modeling Implications**:
   - International brands may show similar pricing patterns across countries
   - National brands reflect country-specific market conditions
   - Regional brands may have more localized pricing strategies

---

## References

**Data Sources**:
- German fuel data: Tankerkönig API (https://creativecommons.tankerkoenig.de)
- Austrian fuel data: ARBÖ, ÖAMTC, OMV, JET station networks
- Italian fuel data: Italian National Fuel Price Database
- Slovenian fuel data: Slovenian National Price Monitoring

**Verification Method**: 
- Brand presence verified by analyzing station counts in actual databases
- Multi-country presence confirmed by cross-referencing datasets
- Station counts extracted from database queries (2024-2025 data)

---

## Changelog

- **2025-10-16**: Initial categorization based on dataset analysis
  - Verified Shell, Eni, AVIA, JET presence in multiple countries
  - Categorized brands based on station counts and market presence
  - Established International/National/Regional framework

