# =============================================================================
# 03_GBIF_flagging_v2.R
# Per-taxon quality flags on reconciled GBIF occurrences (after Whitley et al. 2025, doi:10.1002/ppp3.70044)
# Piedmont urban plant biodiversity
#
# Input:  Piemonte_reconciled.xlsx (sheets GBIF and the tree/other source sheets)
# Output: output/GBIF/summaries/flagging_by_accepted_name.csv
#         output/GBIF/summaries/flagged_names_review.csv
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)


MIN_OCC                  <- 5
MAX_YEARS_SINCE_LAST_OCC <- 60
MIN_DATASOURCES          <- NULL

REF_YEAR <- 2026L  # fixed (year of the GBIF downloads and of the release) so that STALE does not depend on the run date

checklist_xlsx <- "Piemonte_reconciled.xlsx"
summary_dir    <- file.path("output", "GBIF", "summaries")
reconciled_dir <- "data/processed/reconciled"
dir.create(summary_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(reconciled_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1) Load GBIF sheet -----------------------------------------------------

if (!exists("all_steps")) {
  message("Loading sheet GBIF from ", checklist_xlsx, " ...")
  all_steps_raw <- read_excel(checklist_xlsx, sheet = "GBIF", col_types = "text")
  message(sprintf("Loaded: %d rows x %d cols", nrow(all_steps_raw), ncol(all_steps_raw)))
  
  all_steps <- all_steps_raw %>%
    rename(
      final_accepted_name = final_name_manual,
      wcvp_accepted_id    = wcvp_final_id,
      final_source        = source_final
    )
}

all_steps <- all_steps %>%
  mutate(
    year                = suppressWarnings(as.integer(year)),
    final_accepted_name = as.character(final_accepted_name),
    basisOfRecord        = as.character(basisOfRecord),
    datasetKey           = as.character(datasetKey),
    AREA_ID              = as.character(AREA_ID),
    spatial_unit         = case_when(LEVEL %in% c("CORE_CITY", "LAU_ONLY") ~ "LAU",
                                     LEVEL == "FUA" ~ "FUA",
                                     TRUE ~ NA_character_),
    taxonRank            = as.character(taxonRank),
    family               = as.character(family),
    genus                = as.character(genus),
    wcvp_accepted_id     = as.character(wcvp_accepted_id),
    final_source         = as.character(final_source),
    original_raw         = as.character(original_raw),
    group_key            = coalesce(wcvp_accepted_id, final_accepted_name)
  )

# ---- 2) Per-name (per-id) summary -------------------------------------------

no_name <- all_steps %>%
  filter(is.na(final_accepted_name) |
           final_accepted_name == "" | final_accepted_name == "NA")

message(sprintf(
  "Removing %d occurrences with no final_accepted_name (%.1f%% of total)",
  nrow(no_name), 100 * nrow(no_name) / nrow(all_steps)
))

write_csv(
  no_name %>% select(gbifID, AREA_ID, original_raw, final_source, wfo_final_basis),
  file.path(summary_dir, "removed_no_accepted_name.csv")
)

all_steps_clean <- all_steps %>%
  filter(!is.na(final_accepted_name),
         final_accepted_name != "", final_accepted_name != "NA")

message(sprintf("%d occurrences retained for flagging", nrow(all_steps_clean)))

drevojan_cols_present <- names(all_steps_clean)[str_starts(names(all_steps_clean), "drevojan_")]

if ("datasetName" %in% names(all_steps_clean)) {
  dataset_cs_lookup <- all_steps_clean %>%
    group_by(datasetKey) %>%
    summarise(
      is_citizen_science = any(str_detect(coalesce(datasetName, ""), regex("inaturalist|pl@ntnet|plantnet", ignore_case = TRUE))) |
        (mean(basisOfRecord %in% c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION"), na.rm = TRUE) > 0.5),
      .groups = "drop"
    )
} else {
  message("Column 'datasetName' not found: citizen-science classification based on basisOfRecord only.")
  dataset_cs_lookup <- all_steps_clean %>%
    group_by(datasetKey) %>%
    summarise(
      is_citizen_science = mean(basisOfRecord %in% c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION"), na.rm = TRUE) > 0.5,
      .groups = "drop"
    )
}
citsci_datasets <- dataset_cs_lookup$datasetKey[dataset_cs_lookup$is_citizen_science]

name_summary <- all_steps_clean %>%
  group_by(group_key) %>%
  summarise(
    final_accepted_name = names(sort(table(final_accepted_name), decreasing = TRUE))[1],
    n_occ_total       = n(),
    n_occ_herb        = sum(basisOfRecord == "PRESERVED_SPECIMEN",  na.rm = TRUE),
    n_occ_human_obs   = sum(basisOfRecord == "HUMAN_OBSERVATION",   na.rm = TRUE),
    n_occ_living      = sum(basisOfRecord == "LIVING_SPECIMEN",     na.rm = TRUE),
    n_occ_machine_obs = sum(basisOfRecord == "MACHINE_OBSERVATION", na.rm = TRUE),
    n_occ_no_year     = sum(is.na(year)),
    year_min          = suppressWarnings(min(year, na.rm = TRUE)),
    year_max          = suppressWarnings(max(year, na.rm = TRUE)),
    n_datasets        = n_distinct(datasetKey, na.rm = TRUE),
    datasets_list     = paste(sort(unique(na.omit(datasetKey))), collapse = "; "),
    n_datasets_citsci   = n_distinct(datasetKey[datasetKey %in% citsci_datasets], na.rm = TRUE),
    all_datasets_citsci = n_datasets_citsci == n_datasets,
    n_areas           = n_distinct(AREA_ID, na.rm = TRUE),
    areas_list        = paste(sort(unique(na.omit(AREA_ID))), collapse = "; "),
    n_areas_LAU       = n_distinct(AREA_ID[spatial_unit == "LAU"], na.rm = TRUE),
    n_areas_FUA       = n_distinct(AREA_ID[spatial_unit == "FUA"], na.rm = TRUE),
    areas_FUA_list    = paste(sort(unique(na.omit(AREA_ID[spatial_unit == "FUA"]))), collapse = "; "),
    n_original_names  = n_distinct(original_raw, na.rm = TRUE),
    original_names_list = paste(sort(unique(na.omit(original_raw))), collapse = " | "),
    final_source      = names(sort(table(final_source), decreasing = TRUE))[1],
    family            = first(na.omit(family)),
    genus             = first(na.omit(genus)),
    wcvp_accepted_id  = first(na.omit(wcvp_accepted_id)),
    lifeform_wcvp     = first(na.omit(lifeform_from_wcvp)),
    lifeform_drevojan = {
      if (length(drevojan_cols_present) == 0) NA_character_ else {
        row1 <- pick(all_of(drevojan_cols_present))[1, , drop = FALSE]
        hits <- drevojan_cols_present[!is.na(row1) & row1 == "1"]
        if (length(hits) == 0) NA_character_ else paste(str_remove(hits, "^drevojan_"), collapse = "; ")
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    year_min             = if_else(is.infinite(year_min), NA_integer_, as.integer(year_min)),
    year_max             = if_else(is.infinite(year_max), NA_integer_, as.integer(year_max)),
    years_since_last_occ = if_else(is.na(year_max), NA_integer_, as.integer(REF_YEAR - year_max))
  )

# ---- 3) Flag columns --------------------------------------------------------

name_flagged <- name_summary %>%
  mutate(
    flag_low_occ = n_occ_total <= MIN_OCC,
    
    flag_stale = !is.na(years_since_last_occ) & years_since_last_occ >= MAX_YEARS_SINCE_LAST_OCC,
    
    flag_obs_only = (n_occ_herb == 0) & (n_occ_total > 0),
    
    flag_no_year = (n_occ_no_year == n_occ_total),
    
    flag_few_sources = if (!is.null(MIN_DATASOURCES)) {
      n_datasets < MIN_DATASOURCES
    } else {
      FALSE
    },
    
    flag_single_area      = (n_areas == 1),
    flag_synonym_collapse = (n_original_names > 1),
    flag_wfo_only         = str_starts(final_source, "WFO"),
    
    flag_any   = flag_low_occ | flag_stale | flag_obs_only |
      flag_no_year | flag_few_sources,
    
    flag_count = as.integer(flag_low_occ) + as.integer(flag_stale) +
      as.integer(flag_obs_only) + as.integer(flag_no_year) +
      as.integer(flag_few_sources),
    
    flag_reasons = {
      r <- character(n())
      r <- if_else(flag_low_occ,     paste0(r, "LOW_OCC;"),    r)
      r <- if_else(flag_stale,       paste0(r, "STALE;"),       r)
      r <- if_else(flag_obs_only,    paste0(r, "OBS_ONLY;"),    r)
      r <- if_else(flag_no_year,     paste0(r, "NO_YEAR;"),     r)
      r <- if_else(flag_few_sources, paste0(r, "FEW_SOURCES;"), r)
      str_remove(r, ";$")
    }
  ) %>%
  arrange(desc(flag_count), desc(flag_any), final_accepted_name)

# ---- 3b) flag_also_planted -----------------------------------
planted_ids <- character(0)
for (sn in c("RAMI_alberi_monum", "Alberate_Torino", "Alberate_Alessandria")) {
  tryCatch({
    d <- read_excel(checklist_xlsx, sheet = sn, col_types = "text")
    if ("wcvp_final_id" %in% names(d)) planted_ids <- c(planted_ids, na.omit(d$wcvp_final_id))
  }, error = function(e) message("Sheet '", sn, "' not found, skipped (", conditionMessage(e), ")"))
}
planted_ids <- unique(planted_ids)
message(length(planted_ids), " accepted ids found in tree inventories (flag_also_planted)")

name_flagged <- name_flagged %>%
  mutate(flag_also_planted = wcvp_accepted_id %in% planted_ids)

# ---- 3c) flag_gbif_only --------------------------------------
evidence_sheets <- c("Bartolucci_native", "Galasso_esotiche", "RAMI_alberi_monum",
                     "Urbica_Censite", "Urbica_Ornamentali", "BBPAO", "EASIN",
                     "Alberate_Torino", "Alberate_Alessandria")
other_evidence_ids <- character(0)
for (sn in evidence_sheets) {
  tryCatch({
    d <- read_excel(checklist_xlsx, sheet = sn, col_types = "text")
    if ("wcvp_final_id" %in% names(d)) other_evidence_ids <- c(other_evidence_ids, na.omit(d$wcvp_final_id))
  }, error = function(e) message("Sheet '", sn, "' not found, skipped (", conditionMessage(e), ")"))
}
other_evidence_ids <- unique(other_evidence_ids)
message(length(other_evidence_ids), " accepted ids found in other sources (flag_gbif_only)")

name_flagged <- name_flagged %>%
  mutate(flag_gbif_only = !(wcvp_accepted_id %in% other_evidence_ids))

# ---- 4) Outputs -------------------------------------------------------------

write_csv(name_flagged, file.path(summary_dir, "flagging_by_accepted_name.csv"))
message("Written: flagging_by_accepted_name.csv")

flagged_review <- name_flagged %>%
  filter(flag_any | flag_single_area | flag_synonym_collapse | flag_wfo_only) %>%
  select(
    final_accepted_name, flag_count, flag_reasons,
    flag_low_occ, flag_stale, flag_obs_only, flag_no_year, flag_few_sources,
    flag_single_area, flag_synonym_collapse, flag_wfo_only, flag_also_planted, flag_gbif_only,
    n_occ_total, n_occ_herb, n_occ_human_obs,
    year_min, year_max, years_since_last_occ,
    n_datasets, n_datasets_citsci, all_datasets_citsci,
    n_areas, n_areas_LAU, n_areas_FUA, areas_FUA_list,
    n_original_names, original_names_list,
    family, genus, wcvp_accepted_id, final_source,
    lifeform_wcvp, lifeform_drevojan
  )

write_csv(flagged_review, file.path(summary_dir, "flagged_names_review.csv"))
message("Written: flagged_names_review.csv")

all_steps_flagged <- all_steps %>%
  left_join(
    name_flagged %>% select(
      group_key,
      flag_any, flag_count, flag_reasons,
      flag_low_occ, flag_stale, flag_obs_only,
      flag_no_year, flag_few_sources,
      flag_single_area, flag_synonym_collapse, flag_wfo_only, flag_also_planted, flag_gbif_only
    ),
    by = "group_key"
  )

write_csv(
  all_steps_flagged,
  file.path(reconciled_dir, "taxonomic_reconciliation_output_gbif_herb_flagged.csv")
)
message("Written: taxonomic_reconciliation_output_gbif_herb_flagged.csv")

# ---- 5) Summary report ------------------------------------------------------

total  <- nrow(name_flagged)
n_ok   <- sum(!name_flagged$flag_any, na.rm = TRUE)
n_flag <- sum( name_flagged$flag_any, na.rm = TRUE)

message("\n=== Flagging Summary (accepted id / name where id is missing) ===")
message(sprintf("Total accepted names/ids:        %d", total))
message(sprintf("Names passing all core checks:   %d (%.1f%%)", n_ok,   100 * n_ok   / total))
message(sprintf("Names with >=1 core flag:        %d (%.1f%%)", n_flag, 100 * n_flag / total))
message(sprintf("  - LOW_OCC  (<=%d occ):          %d", MIN_OCC, sum(name_flagged$flag_low_occ)))
message(sprintf("  - STALE    (>=%d yrs, dated records): %d", MAX_YEARS_SINCE_LAST_OCC, sum(name_flagged$flag_stale)))
message(sprintf("  - OBS_ONLY (no herbarium):      %d", sum(name_flagged$flag_obs_only)))
message(sprintf("  - NO_YEAR  (no dates):  %d", sum(name_flagged$flag_no_year)))
message("Info flags:")
message(sprintf("  - SINGLE_AREA:                  %d", sum(name_flagged$flag_single_area)))
message(sprintf("  - SYNONYM_COLLAPSE (>1 orig):   %d", sum(name_flagged$flag_synonym_collapse)))
message(sprintf("  - WFO_ONLY (not in WCVP):       %d", sum(name_flagged$flag_wfo_only)))
message(sprintf("  - ALSO_PLANTED (in alberate):   %d", sum(name_flagged$flag_also_planted)))
message(sprintf("  - GBIF_ONLY (no other source): %d", sum(name_flagged$flag_gbif_only)))
message(sprintf("  - all sources citizen-science:  %d", sum(name_flagged$all_datasets_citsci, na.rm = TRUE)))
message("=================================================\n")
