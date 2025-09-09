# Climate Indicators Shiny App

This Shiny web application provides interactive visualization and statistical analysis of multiple climate indicators, including surface and satellite temperature records, ocean heat, solar irradiance, atmospheric CO₂, ENSO variability, and polar sea ice extent.

The app supports **theme switching** (light/dark via Bootswatch) and **color palette selection** (default, viridis, Okabe-Ito), making all plots accessible and visually consistent.

---

## Features

- **Global Surface Temperature**
  - Data sources: GISS, HadCRUT5, and Berkeley Earth (BEST).
  - Options for trend fitting (linear, LOESS) with uncertainty/confidence intervals.
  - Displays overall net change since user-defined start year.

- **Satellite Temperature Records**
  - Data sources: RSS and UAH satellite lower troposphere (TLT) datasets.
  - Annualized anomalies with smoothing and trend estimates.

- **Ocean Temperatures**
  - Data source: HadSST4 global ocean anomalies.
  - Interactive trends and year-to-year variability.

- **Solar Variability**
  - Data sources:
    - Sunspot numbers (SIDC).
    - Reconstructed Total Solar Irradiance (TSI):
      - **SATIRE-T** (Wu+ 2018) back to 1642.
      - **SATIRE-S** (Yeo, Krivova, Solanki) post-1974.
      - Combined record seamlessly merged at August 1974.
  - Annual mean irradiance plotted with linear/LOESS trends.

- **Atmospheric CO₂**
  - Data source: NOAA Mauna Loa annual mean CO₂ (ppm).
  - Both linear and quadratic fits supported.
  - Net change since chosen start year displayed.

- **ENSO (El Niño–Southern Oscillation)**
  - Data source: NOAA ONI (Oceanic Niño Index).
  - Monthly anomalies plotted with neutral, El Niño, and La Niña thresholds.
  - Trend analysis with LOESS smoothing.

- **Sea Ice Extent/Area**
  - Data sources: NOAA PSL monthly sea ice **area** (Arctic and Antarctic).
  - Units: million square kilometers.
  - User-selectable months (individual or “All”).
  - Linear/LOESS trend fits plus net change since start year.

---

## Technology

- **Frontend/Server Framework:** [Shiny](https://shiny.posit.co/)
- **Plotting:** ggplot2 + Plotly for interactive visualization
- **Styling:** Bootswatch themes via `{bslib}`
- **Palette Options:** Default, Viridis (color-blind friendly), Okabe-Ito
- **Data Wrangling:** tidyverse, lubridate, readr

---

## Installation

1. Clone or download this repository.
2. Make sure you have R (≥ 4.1) installed.
3. Install required packages:

```r
install.packages(c(
  "shiny", "plotly", "tidyverse", "readr",
  "lubridate", "naniar", "bslib", "viridisLite"
))
