# Spartina-alterniflora-Root-Tensile-Strength-Sapelo-Island-GA-October-2024

This repository contains R code used to analyze Spartina alterniflora root tensile strength across marsh transects on Sapelo Island, Georgia, USA. The workflow includes data filtering, statistical modeling, correlation analyses, and figure generation for publication-quality outputs.

Analyses Included:

- Data filtering to remove roots that failed at clamp contact points
- Q-Q plot assessment of tensile strength normality
- ANCOVA modeling of tensile strength across marsh distance and site with cubic term
- Visualization of modeled and observed tensile strength patterns
- Site-level summaries of tensile strength vs. distance
- Pearson correlations and linear regressions between tensile strength and:
  - Aboveground height
  - Root length
  - Root diameter
  - Multi-panel figure assembly
- Elevation across distance at each transect (every 1 m)
- Relative tensile strength and elevation within each transect shape comparison
- Spearman correlations between elevation and tensile strength

Expected columns:

- Avg.TS.Nmm2: average tensile strength
- Break: did root break at the contact point? (Y or N)
- Site: sampling transect
- Distance.m: distance from marsh edge
- Height.cm: aboveground plant height
- Length.mm: root length
- Diameter.mm: root diameter

Author: Cody S. Quiroz
Used with data from: 10.5281/zenodo.20401507
Elevation data from USGS 3D Elevation Program 1-m DEM (2022): https://www.sciencebase.gov/catalog/item/629ae8e9d34ec53d276f4d6f
