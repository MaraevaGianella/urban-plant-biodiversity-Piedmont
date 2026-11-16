# Input files

All paths are relative to the project root. Scripts expect the file names below.

## Included in this repository

| File | Used by | Content |
|---|---|---|
| `References_BBPAO.xlsx` | none (reference copy) | Bibliographic references of the BBPAO records (sheet `References`, identical in structure to the sheet of the same name in `Records_BBPAO.xlsx`, which is the file read by 06) |
| `processed/Piedmont_name_reconciliation_reviewed.xlsx` | 02 (Part B) | Unique names after manual review of the WCVP/WFO matching (sheet `review_uniq`) |
| `processed/gbif_backbone_taxonomy_all.csv` | 06 | GBIF backbone taxonomy after manual check |

## Project data included in this repository

| File | Sheet | Used by | Source | Licence / reuse |
|---|---|---|---|---|
| `Records_BBPAO.xlsx` | `Entries`, `References` | 02, 06 | Species records compiled by the authors from the publications indexed in the BBPAO bibliographic database (Estivi et al. 2016); original sources are listed in sheet `References` | Compilation by the authors, released under CC BY 4.0 (`LICENSE_DATA.md`); the cited publications keep their own copyright |
| `UrbicaTorino1993Tables1e2.xlsx` | `Tab.1.Censite`, `Tab.2.Ornam` | 02 | Siniscalco & Barni (1993), *Allionia* 32: 163–180, Tables 1 and 2 (field-survey and ornamental species of Torino) | Species lists transcribed by the authors (two of whom are authors of the original paper) |
| `Alberi_coord_260313_filtered.csv` | | 02 | Alberate (street trees) of the Comune di Torino, Geoportale Piemonte, accessed 13 March 2026, filtered | CC BY 4.0 (stated in the dataset metadata); owner: Comune di Torino. Modified: filtered to the columns and records used here |
| `alberi_Alessandria_260313_filtered.csv` | | 02 | Alberi (street trees) of the Comune di Alessandria, https://github.com/ComunediAlessandria/open-data/, accessed 13 March 2026, filtered | CC BY 4.0 (repository licence, "where not otherwise specified"). Modified: filtered to the columns and records used here |

## Project data not redistributed

| File | Sheet | Used by | Source | Reason |
|---|---|---|---|---|
| `Alberi_monumentali_Piemonte.xlsx` | `LAU_FUA_only` | 02 | RAMI, Registro degli Alberi Monumentali Italiani (ilregistrodeglialberi.it), accessed 28 August 2025 | No open licence found on the website; [UPDATE if permission to redistribute is obtained] |
| `occ_IT_Plantae_all_clean.csv` | | 02 | Records downloaded from the EASIN portal (European Commission JRC), accessed 25 February 2026, for the EASIN species list | [CHECK the EASIN data policy before redistributing] |
| `gbif_clean_occ_herb.csv` | | 02 | Output of `01_GBIF.R` (`output/GBIF/summaries/`), copied here | Size and mixed licences of the GBIF records; regenerate it from the 12 GBIF downloads cited by DOI in `output/GBIF/summaries/gbif_download_log_with_doi.csv` |

## Third-party data to download (not redistributed)

| File | Sheet | Used by | Source | Version / access |
|---|---|---|---|---|
| `Flora_Bartolucci_native.xlsx` | `Native` | 02 | Bartolucci et al. 2024, Italian native vascular flora, Plant Biosystems 158(2): 219–296 | doi:10.1080/11263504.2024.2320126 |
| `Flora_Galasso_esotiche.xlsx` | `flora_esotiche` | 02 | Galasso et al. 2024, Italian alien vascular flora, Plant Biosystems 158(2): 297–340 | doi:10.1080/11263504.2024.2320129 |
| `Life_form_Drevojan_2023.xlsx` | `FloraVegEU-2023-01-03` | 02 | Dřevojan et al. 2023, FloraVeg.eu (Life form) | www.FloraVeg.eu; file dated 2023-01-03 |
| `wcvp_names_2606.csv` | | 02, 04, 06 | World Checklist of Vascular Plants (Kew), names file | doi:10.15468/6h8ucr; [CONFIRM release and download date: the manuscript reports access in February 2026, v15 of the Plants of the World website] |
| `wcvp_distribution_2606.csv` | | 04 | World Checklist of Vascular Plants (Kew), distribution file | [CONFIRM, as above] |
| `_DwC_backbone_R_2606/classification.csv` | | 02 | World Flora Online taxonomic backbone | doi:10.5281/zenodo.18007552; [CONFIRM release: the manuscript reports version 2025-12, accessed February 2026, while the folder name ends in 2606] |
| `ipni_to_wfo.csv` | | 02 | IPNI to WFO crosswalk | [ADD source URL/DOI and download date] |

BBPAO was consulted online at penelope.unito.it/bbpao (September 2025 to January 2026).

GBIF data are downloaded by `01_GBIF.R`; the download DOIs are in `output/GBIF/summaries/gbif_download_log_with_doi.csv`.
