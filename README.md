# Desk-based assessment of urban vascular plant biodiversity in Piedmont, Italy — code and data

Code and checklist workbook.

## Contents
- `R/` — pipeline, run in numeric order from the project root (R 4.5.1; use `renv`):
  1. `01_GBIF.R` — GBIF download and cleaning for the study areas (needs `GBIF_USER`, `GBIF_PWD`, `GBIF_EMAIL` in `~/.Renviron`)
  2. `02_Reconciliation_Piemonte.R` — name parsing and WCVP/WFO reconciliation (Part A, manual review, Part B)
  3. `03_GBIF_flagging_v2.R` — per-taxon GBIF quality flags
  4. `04_Origin_status_assignment.R` — origin/status in Piedmont and Italy (Bartolucci et al. 2024; Galasso et al. 2024; WCVP distribution)
  5. `05_cultivation_flagging.R` — evidence-weight cultivation status
  6. `06_checklist_build.R` — per-area checklists and presence/absence matrix
  7. `07_analyses.R` — tables and figures of the paper (reads `output/checklists/checklists_by_area.xlsx`; downloads the GISCO-Eurostat 2024 FUA and LAU boundaries, so it needs an internet connection)
- `setup_renv.R`, `renv.lock`, `renv/` — R environment (see Reproducibility)
- `Input/` — see `Input/README_input.md` (third-party inputs are not redistributed)
- `checklists_by_area_release.xlsx` — release checklist workbook (Description, all_areas, 12 area sheets, presence/absence matrix)
- `output/checklists/checklists_all_areas.csv`, `checklists_column_dictionary.csv` — UTF-8 CSV copies of `all_areas` and `Description`
- `output/GBIF/summaries/gbif_download_log_with_doi.csv` — DOIs of the 12 GBIF downloads 

## Reproducing the analyses only
Scripts 01–06 need the third-party inputs and a manual review step. To reproduce the paper's tables and figures, run only `R/07_analyses.R` on the workbook above (internet connection required for the GISCO boundaries). Figure 2 is exported to `output/figures/` as interactive HTML; the static PNG needs the kaleido backend for `plotly::save_image()` and is skipped with a message if it is not available.

## Reproducibility
- R 4.5.1; packages are pinned with `renv` (`renv.lock`). To restore the environment, open R in the project root and run `source("setup_renv.R")` (or `renv::restore()`).
- Packages used: CoordinateCleaner, WorldFlora, data.table, dplyr, ggplot2, ggpubr, ggrepel, giscoR, htmlwidgets, lubridate, openxlsx, plotly, purrr, rWCVP, readr, readxl, rgbif, scales, sf, stringr, tibble, tidyr, writexl.
- `sessionInfo.txt` must be generated at the end of a full run (after all packages are loaded), e.g. `writeLines(capture.output(sessionInfo()), "sessionInfo.txt")`.
- GBIF data change over time: the 12 downloads used in the paper (10 March 2026) are cited by DOI in `output/GBIF/summaries/gbif_download_log_with_doi.csv`; reuse those DOIs instead of repeating the download.
- WCVP, WFO and the IPNI-WFO crosswalk are version-specific: use the versions and access dates listed in `Input/README_input.md`.

## Notes
- Run all scripts from the project root; paths are relative (`Input/`, `output/`).
- `02_Reconciliation_Piemonte.R` has two parts: Part A writes the unique names for manual review, Part B reloads the reviewed file (`Input/processed/Piedmont_name_reconciliation_reviewed.xlsx`) and writes `Piemonte_reconciled.xlsx`, which scripts 03-06 read.
- `06_checklist_build.R` writes the release checklist (internal diagnostic columns dropped). The columns `expert_review` and `expert_review_note` and the `Description` sheet are added by hand after the script.

## Citation / licence
- Cite as: 10.5281/zenodo.24284840
- Code (`R/`, `setup_renv.R`): MIT, see `LICENSE`.
- Checklists and manual-review files produced by the authors: CC BY 4.0, see `LICENSE_DATA.md`.
- Third-party inputs keep their original licences (`Input/README_input.md`). Licences of individual GBIF records are documented on the DOI pages of the downloads listed in `output/GBIF/summaries/gbif_download_log_with_doi.csv`.

## R packages and citations

The analyses were run in R 4.5.1 (R Core Team 2025). Package versions are
pinned in `renv.lock`. The packages used in the pipeline are listed below,
grouped by step, with the citation to be used for each.

### Data retrieval, spatial processing and cleaning
- **rgbif** — Chamberlain S, Oldoni D, Barve V, Desmet P, Geffert L, Ram K
  (2026). rgbif: Interface to the Global Biodiversity Information Facility API.
- **CoordinateCleaner** — Zizka A, Silvestro D, Andermann T, et al. (2019).
  CoordinateCleaner: Standardized cleaning of occurrence records from
  biological collection databases. Methods in Ecology and Evolution 10(5):
  744–751. https://doi.org/10.1111/2041-210X.13152
- **giscoR** — Hernangómez D (2026). giscoR: Download 'Eurostat' 'GISCO'
  Spatial Data. https://doi.org/10.5281/zenodo.24116752
- **sf** — Pebesma E (2018). Simple features for R: Standardized support for
  spatial vector data. The R Journal 10(1): 439–446.
  https://doi.org/10.32614/RJ-2018-009

### Taxonomic reconciliation and life forms
- **rWCVP** — Brown M, Walker B (2023). rWCVP: Working with Data from the
  World Checklist of Vascular Plants.
  https://doi.org/10.32614/CRAN.package.rWCVP
- **WorldFlora** — Kindt R (2020). WorldFlora: An R package for exact and
  fuzzy matching of plant names against the World Flora Online taxonomic
  backbone data. Applications in Plant Sciences 8(9): e11388.
  https://doi.org/10.1002/aps3.11388

### Data manipulation
- **data.table** — Dowle M, Srinivasan A (2016). data.table: Extension of
  data.frame. https://CRAN.R-project.org/package=data.table
- **dplyr** — Wickham H, François R, Henry L, Müller K, Vaughan D (2025).
  dplyr: A Grammar of Data Manipulation.
  https://CRAN.R-project.org/package=dplyr
- **lubridate** — Grolemund G, Wickham H (2011). Dates and times made easy
  with lubridate. Journal of Statistical Software 40(3): 1–25.
  https://doi.org/10.18637/jss.v040.i03
- **purrr** — Wickham H, Henry L (2015). purrr: Functional Programming Tools.
  https://CRAN.R-project.org/package=purrr
- **readr** — Wickham H, Hester J, Bryan J (2015). readr: Read Rectangular
  Text Data. https://CRAN.R-project.org/package=readr
- **readxl** — Wickham H, Bryan J (2015). readxl: Read Excel Files.
  https://CRAN.R-project.org/package=readxl
- **stringr** — Wickham H (2018). stringr: Simple, Consistent Wrappers for
  Common String Operations. https://CRAN.R-project.org/package=stringr
- **tibble** — Müller K, Wickham H (2023). tibble: Simple Data Frames.
  https://CRAN.R-project.org/package=tibble
- **tidyr** — Wickham H, Vaughan D, Girlich M (2023). tidyr: Tidy Messy Data.
  https://CRAN.R-project.org/package=tidyr

### Output and visualisation
- **ggplot2** — Wickham H (2011). ggplot2. Wiley Interdisciplinary Reviews:
  Computational Statistics 3(2): 180–185. https://doi.org/10.1002/wics.147
- **ggpubr** — Kassambara A (2016). ggpubr: 'ggplot2' Based Publication Ready
  Plots. https://CRAN.R-project.org/package=ggpubr
- **plotly** — Sievert C (2020). Interactive Web-Based Data Visualization with
  R, plotly, and shiny. Chapman and Hall/CRC. https://plotly-r.com
- **scales** — Wickham H (2016). scales: Scale Functions for Visualization.
  https://CRAN.R-project.org/package=scales
- **writexl** — Ooms J, Denney B (2017). writexl: Export Data Frames to Excel
  'xlsx' Format. https://CRAN.R-project.org/package=writexl
