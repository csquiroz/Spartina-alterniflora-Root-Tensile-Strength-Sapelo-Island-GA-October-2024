# Spartina-alterniflora-Root-Tensile-Strength-Sapelo-Island-GA-October-2024

This repository contains R code used to analyze Spartina alterniflora root tensile strength across marsh transects on Sapelo Island, Georgia, USA. The workflow includes data filtering, statistical modeling, correlation analyses, and figure generation for publication-quality outputs.

Analyses Included:

- Data filtering to remove roots that failed at clamp contact points
- Q-Q plot assessment of tensile strength normality
- ANCOVA modeling of tensile strength across marsh distance and site
- Visualization of modeled and observed tensile strength patterns
- Site-level summaries of tensile strength vs. distance
- Pearson correlations and linear regressions between tensile strength and:
  - Aboveground height
  - Root length
  - Root diameter
  - Multi-panel figure assembly

Expected columns:

- Avg.TS: average tensile strength
- Break: did root break at the contact point? (Y or N)
- Site: sampling transect
- Distance: distance from marsh edge
- Height: aboveground plant height
- Length: root length
- Diameter: root diameter

Note:

File paths in the script should be updated before running.

Author: Cody S. Quiroz
