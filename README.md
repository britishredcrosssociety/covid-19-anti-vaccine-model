# COVID-19 Anti-Vaccination Model

## Overview

This repository contains snippets of code from the COVID-19 anti-vaccination model. This model is based on a pre-print study by [Paul, Steptoe & Fancourt](https://doi.org/10.1101/2020.10.21.20216218). The model aims to replicate the pre-print study by aligning various data sources at the Local Authority geographic level and computing an overall vaccine willingness score. The code in this repository is to serve as a blueprint of the work done to date to facilitate collaboration. It will not run without error.

## Instructions

Below is a list of six indicators and their aligning raw data sources/proxies in parentheses used in the model:

-   flu vaccine uptake (2020 flu vaccination rates)
-   compliance with restrictions (Google mobility data)
-   low incomes (CACI data)
-   female (Governmental population statistics, e.g., ONS)
-   living with children (CACI data)
-   refused a recommended vaccine (First dose of MMR at 24 months)

The `vaccine-map-pep.R` file prepares these indicators for the model (e.g., via scraping/cleaning/aligning geographical areas, etc.). This file is then sourced from the `vaccine-map-build.R` file, which joins the various data sources and calls functions to compute an overall vaccine willingness score. The important function for review and consideration is `calc_vaccine_scores()`. This function can be found in the `functions.R` file which is also sourced in the `vaccine-map-build.R` file.

The logic for the `calc_vaccine_scores()` function is adapted from previous research on the [Indicies of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019). Specifically, please refer to pages 18-20 of the [technical report](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833951/IoD2019_Technical_Report.pdf).

In this instance, the six indicators highlighted above are thought of as six domains of vaccination willingness. These indicators are then: (i) independently ranked and scaled; (ii) weighted according to their relative risk ratios from the pre-print study; (iii) summed; (iv) ranked; (v) quantised into deciles.
