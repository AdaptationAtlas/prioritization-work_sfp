# Prioritization Prototype tool

## Overview

This repository contains the analysis results and documentation for the SFP
Prioritization Prototype.

This repository is designed to share reproducible results and documentation,
while excluding the large raw datasets. The `results/` directory contains
processed outputs derived from the raw data, but not the raw data itself. The
included Makefile references the full workflow, but it will not run successfully
without access to the raw data files.

Two Quarto documents (.qmd) are included and can be rendered without issue, as
they rely only on the results already present in the repository.

```sh
quarto render country_analysis.qmd
# or
quarto render sfp_report.qmd
```

> `country_analysis.qmd` contains the final results with countries split into
> primary and secondary priorities, along with 10 countries for each AOW. It
> also contains priority zones/groups for each country.
>
> `sfp_report.qmd` is a more in depth report covering the analysis and the
> variables included, but it does not contain any rankings of countries.

## Indicator Overview

### Urgency Group

**Vulnerability**

- Poverty (Global Gridded Relative Deprivation Index) — _NASA SEDAC_
- Education Attainment — _University of Washington – IHME_
- Stunting — _University of Washington – IHME_

**Exposure**

- Rural Population Density — _WorldPop & Global Human Settlement Layer_
- Value of Production (2010) per km² — _MapSpam_
- Livestock Unit per km² — _GLW_

---

### **AOW1 – AI Enabled Advisories**

**Advisory Need Index**

- Fertilizer Use Per Unit of Agricultural Production — _FAOSTAT_
- Crop Value of Production Per km² — _MapSpam 2010_
- Crop Yield Gap — _FAO GAEZ4_

**Delivery Capacity Index**

- Weather Station Density (KDE) — _NOAA, OSCAR, WMO Networks_
- Education Attainment — _University of Washington – IHME_
- Internet Download Speed — _OOKLA_
- Ease of Doing Agri-business — _World Bank EBA_
- Network Readiness Index — _Network Readiness Index_

---

### **AOW2 – Managing Shocks**

**Shock Exposure Index**

- Rainfall Variability (CV) — _CHIRPS_
- Drought Frequency (years SPEI < −1) — _SPEI_
- Flood Risk — _JRC Flood Hazard Map_
- Pest & Disease Impact on Yield — _Jason Rhor & lab (unpublished)_

**Resilience Capacity Index**

- Internet Download Speed — _OOKLA_
- Geospatial Resilience to Disasters Index (GRDI) — _NASA SEDAC_
- Rural Population Bank Account Ownership — _World Bank Findex_

---

### **AOW3 – Binding Constraints and Opportunities**

**Constraint Burden Index**

- Crop Diversity (Disease Proxy) — _FAO CropGrids & Shannon Diversity Index_
- Soil Carbon — _SoilGrids_
- Soil pH — _SoilGrids_
- Green Water Scarcity - _Liyin He and Lorenzo Rosa(2023)_

**Enabling Environment Index**

- Time to City (Market Access Proxy) — _Nelson et al. 2019_
- Rural Population Density — _WorldPop & Global Human Settlement Layer_
- Machinery per Unit of Agricultural Land — _USDA & Our World in Data_
- Rural Population Bank Account Ownership — _World Bank Findex_
