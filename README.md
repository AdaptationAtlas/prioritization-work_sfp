# Prioritization Prototype tool

## Overview
This repository contains the analysis results and documentation for the SFP Prioritization Prototype. 

This repository is designed to share reproducible results and documentation, while excluding the large raw datasets.
The `results/` directory contains processed outputs derived from the raw data, but not the raw data itself.
The included Makefile references the full workflow, but it will not run successfully without access to the raw data files.

Two Quarto documents (.qmd) are included and can be rendered without issue, as they rely only on the results already present in the repository.
> 
> `country_analysis.qmd` contains the final results with countries split into primary and secondary priorities, along with 10 countries for each AOW.
> It also contains priority zones/groups for each country.
> 
> `sfp_report.qmd` is a more in depth report covering the analysis and the variables included, but it does not contain any rankings of countries.
>

## Indicator Overview

### Urgency Group

**Vulnerability**

* Poverty (Global Gridded Relative Deprivation Index) — *NASA SEDAC*
* Education Attainment — *University of Washington – IHME*
* Stunting — *University of Washington – IHME*

**Exposure**

* Rural Population Density — *WorldPop & Global Human Settlement Layer*
* Value of Production (2010) per km² — *MapSpam*
* Livestock Unit per km² — *GLW*

---

### **AOW1 – AI Enabled Advisories**

**Advisory Need Index**

* Fertilizer Use Per Unit of Agricultural Production — *FAOSTAT*
* Crop Value of Production Per km² — *MapSpam 2010*
* Crop Yield Gap — *FAO GAEZ4*

**Delivery Capacity Index**

* Weather Station Density (KDE) — *NOAA, OSCAR, WMO Networks*
* Education Attainment — *University of Washington – IHME*
* Internet Download Speed — *OOKLA*
* Ease of Doing Agri-business — *World Bank EBA*
* Network Readiness Index — *Network Readiness Index*

---

### **AOW2 – Managing Shocks**

**Shock Exposure Index**

* Rainfall Variability (CV) — *CHIRPS*
* Drought Frequency (years SPEI < −1) — *SPEI*
* Flood Risk — *JRC Flood Hazard Map*
* Pest & Disease Impact on Yield — *Jason Rhor & lab (unpublished)*

**Resilience Capacity Index**

* Internet Download Speed — *OOKLA*
* Geospatial Resilience to Disasters Index (GRDI) — *NASA SEDAC*
* Rural Population Bank Account Ownership — *World Bank Findex*

---

### **AOW3 – Binding Constraints and Opportunities**

**Constraint Burden Index**

* Crop Diversity (Disease Proxy) — *FAO CropGrids & Shannon Diversity Index*
* Soil Carbon — *SoilGrids*
* Soil pH — *SoilGrids*
* Green Water Scarcity - *Liyin He and Lorenzo Rosa(2023)*

**Enabling Environment Index**

* Time to City (Market Access Proxy) — *Nelson et al. 2019*
* Rural Population Density — *WorldPop & Global Human Settlement Layer*
* Machinery per Unit of Agricultural Land — *USDA & Our World in Data*
* Rural Population Bank Account Ownership — *World Bank Findex*
