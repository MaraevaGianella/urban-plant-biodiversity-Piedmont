# =============================================================================
# 06_checklist_build.R
# Checklists for the 12 areas (8 LAU, 4 FUA) and presence/absence matrix
# Piedmont urban plant biodiversity
#
# Input:  Piemonte_reconciled.xlsx, Input/Records_BBPAO.xlsx, Input/processed/gbif_backbone_taxonomy_all.csv
#         outputs of scripts 03, 04 and 05
# Output: output/checklists/checklists_by_area.xlsx and checklists_all_areas.csv
# =============================================================================

library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(tidyr)
library(readr)
library(rgbif)

# ---- Part 1: GBIF backbone taxonomy and non-vascular filter ----

origin_status_csv <- "output/origin_status/origin_status_full.csv"
output_dir <- "output/checklists"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Taxa to check ----

taxa <- read_csv(origin_status_csv, show_col_types = FALSE) %>%
  mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
  filter(!is.na(final_reconciled_name)) %>%
  distinct(wcvp_final_id, final_reconciled_name)

message(nrow(taxa), " unique names to check against the GBIF backbone.")

# ---- Query the GBIF backbone ----

message("Querying the GBIF backbone for ", nrow(taxa), " names in batches with progressive saving ",
        "(if a batch times out, completed batches stay saved)...")

BATCH_SIZE <- 200
progress_csv <- file.path(output_dir, "gbif_backbone_taxonomy_RAW_PROGRESS.csv")

already_done <- if (file.exists(progress_csv)) {
  read_csv(progress_csv, show_col_types = FALSE) %>% mutate(wcvp_final_id = as.character(wcvp_final_id))
} else {
  tibble(wcvp_final_id = character(0))
}
taxa_remaining <- taxa %>% anti_join(already_done, by = "wcvp_final_id")
message(nrow(already_done), " names already completed, ", nrow(taxa_remaining), " remaining.")

n_batches <- ceiling(nrow(taxa_remaining) / BATCH_SIZE)
all_results <- list()

if (n_batches > 0) {
  for (b in seq_len(n_batches)) {
    idx <- ((b - 1) * BATCH_SIZE + 1):min(b * BATCH_SIZE, nrow(taxa_remaining))
    batch <- taxa_remaining[idx, ]
    message("Batch ", b, "/", n_batches, " (", nrow(batch), " names)...")
    
    batch_result <- tryCatch({
      name_backbone_checklist(name_data = batch %>% transmute(name = final_reconciled_name),
                              curlopts = list(http_version = 2, timeout = 120))
    }, error = function(e) {
      message("  ERROR in this batch: ", conditionMessage(e), "; skipped, retry later.")
      NULL
    })
    
    if (!is.null(batch_result)) {
      verbatim_col <- intersect(c("verbatim_name", "verbatim_scientificName", "name"), names(batch_result))[1]
      batch_result <- batch_result %>%
        rename(matched_name_input = all_of(verbatim_col)) %>%
        distinct(matched_name_input, .keep_all = TRUE)
      batch_joined <- batch %>%
        left_join(batch_result, by = c("final_reconciled_name" = "matched_name_input"))
      all_results[[b]] <- batch_joined
      write_csv(batch_joined, progress_csv, append = file.exists(progress_csv))
    }
  }
}

taxa_with_taxonomy_raw <- bind_rows(already_done, bind_rows(all_results))
message("\nCompleted: ", nrow(taxa_with_taxonomy_raw), " of ", nrow(taxa), " names.")

# ---- Export for manual review ----

write_csv(taxa_with_taxonomy_raw, file.path(output_dir, "gbif_backbone_taxonomy_RAW_for_review.csv"))

message("\nWritten: ", file.path(output_dir, "gbif_backbone_taxonomy_RAW_for_review.csv"))



# ---- Part 2: checklist assembly ----

checklist_xlsx <- "Piemonte_reconciled.xlsx"
bbpao_records_xlsx <- "Input/Records_BBPAO.xlsx"
gbif_backbone_csv <- "Input/processed/gbif_backbone_taxonomy_all.csv"
origin_status_csv <- "output/origin_status/origin_status_full.csv"
cultivation_csv <- "output/cultivation/cultivation_status_by_id.csv"
gbif_flags_csv <- "output/GBIF/summaries/flagging_by_accepted_name.csv"
output_dir <- "output/checklists"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

AREAS_LAU_ONLY <- c("Biella", "Cuneo", "Verbania", "Vercelli")
AREAS_WITH_FUA <- c("Torino", "Alessandria", "Asti", "Novara")
ALL_AREAS <- c(AREAS_LAU_ONLY, AREAS_WITH_FUA)

# ---- Initial diagnostics ----

sheets_available <- excel_sheets(checklist_xlsx)
message("Sheets found in '", checklist_xlsx, "':")
message(paste(" -", sheets_available, collapse = "\n"))

expected_sheets <- c("BBPAO", "Urbica_Censite", "Urbica_Ornamentali", "RAMI_alberi_monum",
                     "Alberate_Torino", "Alberate_Alessandria", "GBIF", "EASIN",
                     "checklist_finale")
missing_sheets <- setdiff(expected_sheets, sheets_available)
if (length(missing_sheets) > 0) {
  message("\nWARNING: expected sheets not found: ", paste(missing_sheets, collapse = ", "))
}

# ---- Helper functions ----

expand_area_scale <- function(city, scale_value, sources_without_scale_assume_lau = TRUE) {
  if (length(city) != 1 || is.na(city) || city == "") return(character(0))
  if (length(scale_value) == 0 || is.na(scale_value) || scale_value == "") {
    su <- if (sources_without_scale_assume_lau) "LAU" else NA_character_
  } else if (length(scale_value) == 1) {
    su <- scale_value
  } else {
    return(character(0))
  }
  has_fua <- city %in% AREAS_WITH_FUA
  if (is.na(su)) return(character(0))
  if (su == "LAU") {
    if (has_fua) return(c(paste0(city, "_LAU"), paste0(city, "_FUA"))) else return(paste0(city, "_LAU"))
  } else if (su == "FUA") {
    if (has_fua) return(paste0(city, "_FUA")) else return(character(0))
  } else {
    return(character(0))
  }
}

map_scale_value <- function(x) {
  case_when(
    x == "CORE_CITY" ~ "LAU",
    x == "FUA" ~ "FUA",
    TRUE ~ NA_character_
  )
}

classify_bbpao_note <- function(note) {
  nl <- str_to_lower(note)
  has_cult_specific <- str_detect(nl, "vaso|orto botanic|aiuola|aiuole")
  has_coltivat <- str_detect(nl, "coltivat")
  has_sfuggit <- str_detect(nl, "sfuggit")
  has_spontane <- str_detect(nl, "spontane")
  case_when(
    is.na(note) ~ NA_character_,
    (has_cult_specific | has_coltivat) & (has_sfuggit | has_spontane) ~ "cultivated_but_escaped_or_naturalized",
    has_cult_specific ~ "cultivated_high_confidence",
    has_sfuggit ~ "escaped_from_cultivation",
    has_coltivat ~ "cultivated_ambiguous",
    has_spontane ~ "explicitly_spontaneous",
    TRUE ~ NA_character_
  )
}

bbpao_note_to_status <- function(note_signal) {
  case_when(
    note_signal %in% c("cultivated_high_confidence", "cultivated_but_escaped_or_naturalized") ~ "cultivated",
    note_signal == "escaped_from_cultivation" ~ "cultivated",
    note_signal == "cultivated_ambiguous" ~ NA_character_,
    TRUE ~ "wild"
  )
}

summarize_drevojan <- function(df) {
  drevojan_cols <- names(df)[startsWith(names(df), "drevojan_")]
  if (length(drevojan_cols) == 0) return(rep(NA_character_, nrow(df)))
  apply(df[drevojan_cols], 1, function(row) {
    active <- drevojan_cols[!is.na(row) & row %in% c("1", "TRUE", "TRUE ")]
    if (length(active) == 0) return(NA_character_)
    paste(sub("^drevojan_", "", active), collapse = "; ")
  })
}

logical_to_01 <- function(df) {
  df %>% mutate(across(where(is.logical), ~ as.integer(.)))
}

extract_years_from_string <- function(x) suppressWarnings(as.integer(unlist(strsplit(x, ";\\s*"))))

build_gbif_flag_reasons <- function(low_occ, stale, obs_only, gbif_only) {
  mapply(function(a, b, c, d) {
    reasons <- c(if (coalesce(a, FALSE)) "low_occ", if (coalesce(b, FALSE)) "stale",
                 if (coalesce(c, FALSE)) "obs_only", if (coalesce(d, FALSE)) "gbif_only")
    if (length(reasons) == 0) NA_character_ else paste(reasons, collapse = "; ")
  }, low_occ, stale, obs_only, gbif_only)
}

# ---- Reference years by source ----

bbpao_references <- read_excel(bbpao_records_xlsx, sheet = "References") %>%
  select(ref_id, pub_year = year) %>%
  distinct(ref_id, .keep_all = TRUE)

# ---- Source configuration ----

source_config <- tibble::tribble(
  ~sheet_name,             ~area_field,     ~fixed_area,   ~scale_field,   ~reliability, ~default_status,
  "BBPAO",                 "province",      NA_character_, "FUA_LAU",      "high",       "wild",
  "Urbica_Censite",        NA_character_,   "Torino",      NA_character_,  "high",       "wild",
  "Urbica_Ornamentali",    NA_character_,   "Torino",      NA_character_,  "high",       "cultivated",
  "RAMI_alberi_monum",     "province",      NA_character_, "FUA_LAU",      "high",       "cultivated",
  "Alberate_Torino",       NA_character_,   "Torino",      NA_character_,  "high",       "cultivated",
  "Alberate_Alessandria",  NA_character_,   "Alessandria", NA_character_,  "high",       "cultivated",
  "GBIF",                  "NAME",          NA_character_, "LEVEL",        "variable",   "wild"
)

# ---- Load sources ----

load_source <- function(cfg_row) {
  sn <- cfg_row$sheet_name
  message("--- Loading '", sn, "' ---")
  
  if (!sn %in% sheets_available) {
    message("  SKIPPED: sheet '", sn, "' does not exist.")
    return(tibble())
  }
  
  d <- read_excel(checklist_xlsx, sheet = sn, col_types = "text")
  message("  raw rows: ", nrow(d), " | columns: ", ncol(d))
  
  if (!"wcvp_final_id" %in% names(d)) {
    message("  SKIPPED: column 'wcvp_final_id' not found.")
    return(tibble())
  }
  d <- d %>% filter(!is.na(wcvp_final_id))
  message("  rows with a valid wcvp_final_id: ", nrow(d))
  if (nrow(d) == 0) return(tibble())
  
  orig_col <- intersect(c("Taxon", "original_raw", "binomial", "descr_specie", "Essenza",
                          "taxon_as_cited", "Species", "Specie", "SpeciesName", "scientificName"),
                        names(d))[1]
  d$original_taxon_raw <- if (!is.na(orig_col) && !is.null(orig_col)) d[[orig_col]] else NA_character_
  
  if (!is.na(cfg_row$area_field) && cfg_row$area_field %in% names(d)) {
    d$area <- d[[cfg_row$area_field]]
  } else if (!is.na(cfg_row$fixed_area)) {
    d$area <- cfg_row$fixed_area
  } else {
    message("  SKIPPED: no area field and no fixed area.")
    return(tibble())
  }
  
  d$scale_raw <- if (!is.na(cfg_row$scale_field) && cfg_row$scale_field %in% names(d)) d[[cfg_row$scale_field]] else NA_character_
  if (sn %in% c("GBIF", "EASIN")) {
    d$scale_raw <- map_scale_value(d$scale_raw)
  }
  
  d$reliability <- cfg_row$reliability
  d$source_sheet <- sn
  
  if (sn == "BBPAO") {
    d$note_signal <- classify_bbpao_note(d$notes)
    d$row_status <- coalesce(bbpao_note_to_status(d$note_signal), cfg_row$default_status)
    d$bbpao_reference <- d$ref_id
    d <- d %>% left_join(bbpao_references, by = "ref_id")
    d$record_year <- d$pub_year
  } else if (sn %in% c("Urbica_Censite", "Urbica_Ornamentali")) {
    d$note_signal <- NA_character_
    d$row_status <- cfg_row$default_status
    d$record_year <- 1993L
    d$bbpao_reference <- NA_character_
  } else {
    d$note_signal <- NA_character_
    d$row_status <- cfg_row$default_status
    d$record_year <- NA_integer_
    d$bbpao_reference <- NA_character_
  }
  
  for (tax_col in c("kingdom", "phylum", "class", "order", "family")) {
    d[[paste0("taxo_", tax_col)]] <- if (tax_col %in% names(d)) d[[tax_col]] else NA_character_
  }
  
  d$gbif_basis_raw <- if ("basisOfRecord" %in% names(d)) d$basisOfRecord else NA_character_
  
  d <- d %>%
    select(wcvp_final_id, original_taxon_raw, area, scale_raw, source_sheet, reliability,
           row_status, note_signal, record_year, bbpao_reference, gbif_basis_raw, starts_with("taxo_"))
  
  n_before_area_filter <- nrow(d)
  d <- d %>% filter(area %in% ALL_AREAS)
  message("  rows with a valid area: ", nrow(d), " (discarded ", n_before_area_filter - nrow(d), ")")
  
  d <- d %>%
    { .$area_scale_sheet <- mapply(expand_area_scale, .$area, .$scale_raw, SIMPLIFY = FALSE); . } %>%
    tidyr::unnest(area_scale_sheet, keep_empty = FALSE) %>%
    select(-scale_raw, -area)
  
  message("  final rows after LAU/FUA expansion: ", nrow(d))
  d
}

message("\n=== Loading reconciled sources ===")
all_source_rows <- bind_rows(lapply(seq_len(nrow(source_config)), function(i) load_source(source_config[i, ])))
message("\nTotal rows from all sources: ", nrow(all_source_rows))

if (nrow(all_source_rows) == 0) {
  stop("No rows loaded from any source; check the messages above.")
}


wcvp_backbone_csv <- "Input/wcvp_names_2606.csv"  
wcvp_status_lookup <- data.table::fread(wcvp_backbone_csv, sep = "|", encoding = "UTF-8", quote = "") %>%
  select(plant_name_id, taxon_status, accepted_plant_name_id) %>%
  mutate(plant_name_id = as.character(plant_name_id),
         accepted_plant_name_id = as.character(accepted_plant_name_id))

resolve_to_accepted <- function(id, lookup, max_hops = 5) {
  current <- id
  for (i in seq_len(max_hops)) {
    row <- lookup[lookup$plant_name_id == current, ]
    if (nrow(row) == 0) return(current)
    if (row$taxon_status[1] == "Accepted") return(current)
    nxt <- row$accepted_plant_name_id[1]
    if (is.na(nxt) || nxt == current) return(current)
    current <- nxt
  }
  current
}

ids_to_check <- unique(all_source_rows$wcvp_final_id)
ids_to_check <- ids_to_check[!is.na(ids_to_check)]

id_remap <- tibble(wcvp_final_id = ids_to_check) %>%
  mutate(wcvp_final_id_corrected = vapply(wcvp_final_id, resolve_to_accepted, character(1), lookup = wcvp_status_lookup))

n_remapped <- sum(id_remap$wcvp_final_id != id_remap$wcvp_final_id_corrected)
message(n_remapped, " wcvp_final_id remapped to the true Accepted id (out of ", nrow(id_remap), " ids checked).")
if (n_remapped > 0) {
  write_csv(id_remap %>% filter(wcvp_final_id != wcvp_final_id_corrected),
            file.path(output_dir, "wcvp_id_remap_applied.csv"))
  message("Dettaglio rimappature salvato in: ", file.path(output_dir, "wcvp_id_remap_applied.csv"))
}

remap_vec <- setNames(id_remap$wcvp_final_id_corrected, id_remap$wcvp_final_id)
all_source_rows <- all_source_rows %>%
  mutate(wcvp_final_id = coalesce(remap_vec[wcvp_final_id], wcvp_final_id))


# ---- GBIF quality flags ----

gbif_flags <- read_csv(gbif_flags_csv, show_col_types = FALSE) %>%
  rename(wcvp_final_id = wcvp_accepted_id) %>%
  mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
  select(wcvp_final_id, n_occ_herb, n_occ_total, year_min, year_max, years_since_last_occ,
         flag_low_occ, flag_stale, flag_obs_only, flag_gbif_only)

all_source_rows <- all_source_rows %>%
  left_join(gbif_flags, by = "wcvp_final_id") %>%
  mutate(
    reliability = if_else(source_sheet == "GBIF" & coalesce(n_occ_herb, 0) > 0, "high", reliability),
    gbif_weak_evidence = source_sheet %in% c("GBIF") &
      (coalesce(flag_low_occ, FALSE) | coalesce(flag_stale, FALSE) | coalesce(flag_obs_only, FALSE))
  )

# ---- EASIN ----

easin_species_ids <- tryCatch({
  read_excel(checklist_xlsx, sheet = "EASIN", col_types = "text") %>%
    filter(!is.na(wcvp_final_id)) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    distinct(wcvp_final_id) %>%
    pull(wcvp_final_id)
}, error = function(e) {
  message("Sheet 'EASIN' not found or not reconciled: ", conditionMessage(e), "; empty list.")
  character(0)
})

message(length(easin_species_ids), " distinct taxa in the EASIN list.")

# ---- Non-vascular filter ----

non_vascular_from_occurrences <- all_source_rows %>%
  filter(!is.na(taxo_phylum), taxo_phylum != "Tracheophyta") %>%
  distinct(wcvp_final_id, phylum_source = taxo_phylum)

non_vascular_from_backbone <- tibble(wcvp_final_id = character(0), phylum_source = character(0))
if (file.exists(gbif_backbone_csv)) {
  non_vascular_from_backbone <- read_csv(gbif_backbone_csv, show_col_types = FALSE) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    filter(!is.na(phylum), phylum != "Tracheophyta") %>%
    distinct(wcvp_final_id, phylum_source = phylum)
  message("GBIF backbone loaded: ", nrow(non_vascular_from_backbone), " non-Tracheophyta taxa found.")
} else {
  message("Backbone file not found in '", gbif_backbone_csv, "'; filter based on GBIF occurrences only.")
}

non_vascular_ids <- bind_rows(non_vascular_from_occurrences, non_vascular_from_backbone) %>%
  distinct(wcvp_final_id, .keep_all = TRUE)

message("\n", nrow(non_vascular_ids), " taxa identified as non-vascular in total; excluded.")
if (nrow(non_vascular_ids) > 0) {
  write_csv(non_vascular_ids, file.path(output_dir, "excluded_non_vascular.csv"))
}

all_source_rows <- all_source_rows %>% anti_join(non_vascular_ids, by = "wcvp_final_id")

# ---- Aggregate by area and taxon ----

per_area_taxon <- all_source_rows %>%
  group_by(area_scale_sheet, wcvp_final_id) %>%
  summarise(
    original_names_by_source = paste(sort(unique(na.omit(paste0(original_taxon_raw, " (", source_sheet, ")")))), collapse = " | "),
    taxo_kingdom = first(na.omit(taxo_kingdom)),
    taxo_phylum  = first(na.omit(taxo_phylum)),
    taxo_class   = first(na.omit(taxo_class)),
    taxo_order   = first(na.omit(taxo_order)),
    taxo_family  = first(na.omit(taxo_family)),
    sources_list = paste(sort(unique(source_sheet[source_sheet != "EASIN"])), collapse = "; "),
    n_sources = n_distinct(source_sheet[source_sheet != "EASIN"]),
    n_records_total = n(),
    in_bbpao = "BBPAO" %in% source_sheet,
    in_urbica_censite = "Urbica_Censite" %in% source_sheet,
    in_urbica_ornamentali = "Urbica_Ornamentali" %in% source_sheet,
    in_rami = "RAMI_alberi_monum" %in% source_sheet,
    in_alberate_torino = "Alberate_Torino" %in% source_sheet,
    in_alberate_alessandria = "Alberate_Alessandria" %in% source_sheet,
    in_gbif = "GBIF" %in% source_sheet,
    n_records_bbpao = sum(source_sheet == "BBPAO"),
    n_records_rami = sum(source_sheet == "RAMI_alberi_monum"),
    n_records_gbif = sum(source_sheet == "GBIF"),
    bbpao_years = paste(sort(unique(na.omit(record_year[source_sheet == "BBPAO"]))), collapse = "; "),
    bbpao_references = paste(sort(unique(na.omit(bbpao_reference))), collapse = "; "),
    urbica_survey_year = first(na.omit(record_year[source_sheet %in% c("Urbica_Censite", "Urbica_Ornamentali")])),
    gbif_year_min = first(na.omit(year_min)),
    gbif_year_max = first(na.omit(year_max)),
    gbif_years_since_last_occ = first(na.omit(years_since_last_occ)),
    gbif_basis_of_record = paste(sort(unique(na.omit(gbif_basis_raw))), collapse = "; "),
    gbif_flag_low_occ = any(coalesce(flag_low_occ, FALSE)),
    gbif_flag_stale = any(coalesce(flag_stale, FALSE)),
    gbif_flag_obs_only = any(coalesce(flag_obs_only, FALSE)),
    gbif_flag_gbif_only = any(coalesce(flag_gbif_only, FALSE)),
    has_high_wild = any(reliability == "high" & row_status == "wild"),
    has_high_cultivated = any(reliability == "high" & row_status == "cultivated"),
    has_ambiguous_bbpao_note = any(source_sheet == "BBPAO" & note_signal == "cultivated_ambiguous", na.rm = TRUE),
    only_variable_reliability = all(reliability[source_sheet != "EASIN"] == "variable"),
    any_weak_gbif_evidence = any(coalesce(gbif_weak_evidence, FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    local_status = case_when(
      has_high_wild ~ "confirmed_wild",
      has_high_cultivated ~ "likely_cultivated",
      only_variable_reliability & any_weak_gbif_evidence ~ "uncertain_weak_gbif",
      only_variable_reliability ~ "confirmed_wild",
      TRUE ~ "uncertain"
    ),
    needs_area_review = has_ambiguous_bbpao_note |
      local_status %in% c("uncertain_weak_gbif", "uncertain") |
      (has_high_wild & has_high_cultivated)
  )

message("\n", nrow(per_area_taxon), " area x taxon combinations in total.")

# ---- Higher taxonomy from the backbone ----

if (file.exists(gbif_backbone_csv)) {
  backbone_taxonomy <- read_csv(gbif_backbone_csv, show_col_types = FALSE) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    distinct(wcvp_final_id, .keep_all = TRUE) %>%
    select(wcvp_final_id, kingdom, phylum, class, order, family, genus)
  
  per_area_taxon <- per_area_taxon %>%
    left_join(backbone_taxonomy, by = "wcvp_final_id") %>%
    mutate(
      taxo_kingdom = coalesce(taxo_kingdom, kingdom),
      taxo_phylum  = coalesce(taxo_phylum, phylum),
      taxo_class   = coalesce(taxo_class, class),
      taxo_order   = coalesce(taxo_order, order),
      taxo_family  = coalesce(taxo_family, family),
      taxo_genus   = genus
    ) %>%
    select(-kingdom, -phylum, -class, -order, -family)
  
  message(sum(!is.na(per_area_taxon$taxo_phylum)), " rows with a phylum, out of ", nrow(per_area_taxon), ".")
}

# ---- Join global status, identity and life form ----

origin_status <- read_csv(origin_status_csv, show_col_types = FALSE) %>%
  mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
  distinct(wcvp_final_id, .keep_all = TRUE) %>%
  rename(global_needs_review = needs_manual_review, global_review_reason = review_reason)

cultivation_status <- read_csv(cultivation_csv, show_col_types = FALSE) %>%
  mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
  distinct(wcvp_final_id, .keep_all = TRUE) %>%
  select(wcvp_final_id, evidence_wild, evidence_cultivated, cultivation_status) %>%
  rename(global_cultivation_status = cultivation_status)

identity_lifeform_lookup <- tryCatch({
  identity_cols <- c("wcvp_final_id", "wcvp_final_name", "wcvp_final_authors", "wcvp_final_rank",
                     "wcvp_final_ipni_id", "wfo_final_id", "wfo_final_name", "wfo_final_authors", "wfo_final_rank")
  
  sheets_for_identity <- c("checklist_finale", source_config$sheet_name)
  
  identity_from_all_sheets <- bind_rows(lapply(sheets_for_identity, function(sh) {
    if (!sh %in% sheets_available) return(tibble())
    d <- read_excel(checklist_xlsx, sheet = sh, col_types = "text")
    if (!"wcvp_final_id" %in% names(d)) return(tibble())
    d %>% filter(!is.na(wcvp_final_id)) %>% select(any_of(identity_cols))
  })) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    group_by(wcvp_final_id) %>%
    summarise(across(everything(), ~ first(na.omit(.x))[1]), .groups = "drop")
  
  lifeform_part <- read_excel(checklist_xlsx, sheet = "checklist_finale", col_types = "text") %>%
    filter(!is.na(wcvp_final_id)) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    distinct(wcvp_final_id, .keep_all = TRUE) %>%
    mutate(lifeform_drevojan_summary = summarize_drevojan(.)) %>%
    select(wcvp_final_id, lifeform_from_wcvp, any_of("lifeform_match_level"), lifeform_drevojan_summary)
  
  full_join(identity_from_all_sheets, lifeform_part, by = "wcvp_final_id")
}, error = function(e) { message("Errore costruendo identity_lifeform_lookup: ", conditionMessage(e)); tibble(wcvp_final_id = character(0)) })


endemism_lookup <- tryCatch(
  read_excel(checklist_xlsx, sheet = "Bartolucci_native", col_types = "text") %>%
    filter(!is.na(wcvp_final_id)) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    distinct(wcvp_final_id, .keep_all = TRUE) %>%
    transmute(wcvp_final_id, endemic_italy = !is.na(E) & E != ""),
  error = function(e) { message("Column 'E' not found in Bartolucci_native: ", conditionMessage(e)); tibble(wcvp_final_id = character(0)) }
)


origin_status <- origin_status %>%
  mutate(wcvp_final_id = coalesce(remap_vec[wcvp_final_id], wcvp_final_id))

cultivation_status <- cultivation_status %>%
  mutate(wcvp_final_id = coalesce(remap_vec[wcvp_final_id], wcvp_final_id))

identity_lifeform_lookup <- identity_lifeform_lookup %>%
  mutate(wcvp_final_id = coalesce(remap_vec[wcvp_final_id], wcvp_final_id))

endemism_lookup <- endemism_lookup %>%
  mutate(wcvp_final_id = coalesce(remap_vec[wcvp_final_id], wcvp_final_id))

per_area_taxon <- per_area_taxon %>%
  left_join(origin_status, by = "wcvp_final_id") %>%
  left_join(cultivation_status, by = "wcvp_final_id") %>%
  left_join(identity_lifeform_lookup, by = "wcvp_final_id") %>%
  left_join(endemism_lookup, by = "wcvp_final_id") %>%
  mutate(
    origin_italy = case_when(
      source_side == "native" ~ "native",
      source_side == "alien" ~ "alien",
      TRUE ~ NA_character_
    ),
    in_easin_alien_list = wcvp_final_id %in% easin_species_ids,
    easin_taxon_confirmed_alien = coalesce(str_starts(origin_piedmont, "alien"), FALSE) |
      coalesce(wcvp_introduced_in_italy, FALSE),
    local_global_conflict = case_when(
      is.na(global_cultivation_status) ~ NA,
      local_status == "confirmed_wild" & global_cultivation_status == "likely_cultivated" ~ TRUE,
      local_status == "likely_cultivated" & global_cultivation_status == "confirmed_wild" ~ TRUE,
      TRUE ~ FALSE
    ),
    local_evidence_despite_mistake = coalesce(global_cultivation_status == "exclude_recorded_by_mistake", FALSE) &
      local_status %in% c("confirmed_wild", "likely_cultivated"),
    needs_area_review = needs_area_review | coalesce(global_needs_review, FALSE) |
      coalesce(local_global_conflict, FALSE) | local_evidence_despite_mistake,
    gbif_flag_any = coalesce(gbif_flag_low_occ, FALSE) | coalesce(gbif_flag_stale, FALSE) |
      coalesce(gbif_flag_obs_only, FALSE) | coalesce(gbif_flag_gbif_only, FALSE),
    gbif_flag_reasons = build_gbif_flag_reasons(gbif_flag_low_occ, gbif_flag_stale, gbif_flag_obs_only, gbif_flag_gbif_only)
  )
message(sum(per_area_taxon$local_global_conflict, na.rm = TRUE), " local/global status disagreements.")

# ---- Record years across all sources ----

per_area_taxon <- per_area_taxon %>%
  rowwise() %>%
  mutate(
    .all_years_vec = list(c(extract_years_from_string(bbpao_years), urbica_survey_year, gbif_year_min, gbif_year_max)),
    first_record_year = suppressWarnings(min(unlist(.all_years_vec), na.rm = TRUE)),
    last_record_year = suppressWarnings(max(unlist(.all_years_vec), na.rm = TRUE)),
    first_record_year = if (is.infinite(first_record_year)) NA_integer_ else first_record_year,
    last_record_year = if (is.infinite(last_record_year)) NA_integer_ else last_record_year
  ) %>%
  ungroup() %>%
  select(-.all_years_vec)

# ---- Duplicate safety check ----

n_before_dedup <- nrow(per_area_taxon)
per_area_taxon <- per_area_taxon %>% distinct(area_scale_sheet, wcvp_final_id, .keep_all = TRUE)
if (nrow(per_area_taxon) < n_before_dedup) {
  message("WARNING: removed ", n_before_dedup - nrow(per_area_taxon), " duplicate rows.")
}

# ---- Column order and 1/0 booleans ----

per_area_taxon <- per_area_taxon %>%
  logical_to_01() %>%
  select(area_scale_sheet,
         wcvp_final_id, any_of(c("wcvp_final_name", "wcvp_final_authors", "wcvp_final_rank", "wcvp_final_ipni_id")),
         any_of(c("wfo_final_id", "wfo_final_name", "wfo_final_authors", "wfo_final_rank")),
         original_names_by_source, final_reconciled_name,
         local_status, global_cultivation_status, local_global_conflict, local_evidence_despite_mistake, needs_area_review,
         in_easin_alien_list, easin_taxon_confirmed_alien, 
         origin_piedmont, origin_italy, alien_substatus_piedmont, status_piedmont, status_italy,
         endemic_italy, cultivated, feral_flag, is_ias, introduction_period,
         wcvp_in_italy, wcvp_introduced_in_italy, wcvp_distribution_conflict,
         distribution_category, regions_present, distribution_disjunct,
         taxo_kingdom, taxo_phylum, taxo_class, taxo_order, taxo_family,
         taxo_genus, lifeform_from_wcvp, lifeform_drevojan_summary,
         sources_list, n_sources, n_records_total,
         in_bbpao, in_urbica_censite, in_urbica_ornamentali,
         in_rami, in_alberate_torino, in_alberate_alessandria, in_gbif, 
         n_records_bbpao, n_records_rami, n_records_gbif, 
         bbpao_years, bbpao_references, urbica_survey_year,
         gbif_year_min, gbif_year_max, gbif_years_since_last_occ, gbif_basis_of_record,
         first_record_year, last_record_year,
         gbif_flag_low_occ, gbif_flag_stale, gbif_flag_obs_only, gbif_flag_gbif_only,
         gbif_flag_any, gbif_flag_reasons,
         evidence_wild, evidence_cultivated,
         global_needs_review, global_review_reason, everything())

internal_cols <- c("has_high_wild", "has_high_cultivated", "has_ambiguous_bbpao_note",
                   "only_variable_reliability", "any_weak_gbif_evidence",
                   "evidence_wild", "evidence_cultivated", "review_row_id", "original_taxon",
                   "uncertain_piedmont", "cryptogenic_piedmont", "lifeform_match_level",
                   "national_status_native", "genus")

per_area_taxon <- per_area_taxon %>%
  select(-any_of(internal_cols)) %>%
  mutate(taxo_kingdom = coalesce(taxo_kingdom, "Plantae"))


# ---- Export workbook ----

message("\n=== Creating the final workbook ===")

wb <- createWorkbook()

# ---- All areas sheet ----

addWorksheet(wb, "all_areas")
writeDataTable(
  wb,
  sheet = "all_areas",
  x = per_area_taxon,
  rowNames = FALSE,
  tableStyle = "TableStyleLight9"
)

# ---- Per-area sheets ----

for (a in ALL_AREAS) {
  for (scale in c("LAU", "FUA")) {
    
    sheet_key <- paste0(a, "_", scale)
    
    if (scale == "FUA" && !(a %in% AREAS_WITH_FUA)) next
    
    d <- per_area_taxon %>%
      filter(area_scale_sheet == sheet_key)
    
    if (nrow(d) == 0) {
      message("No data for ", sheet_key, "; sheet skipped.")
      next
    }
    
    sheet_label <- substr(sheet_key, 1, 31)
    
    addWorksheet(wb, sheet_label)
    
    writeDataTable(
      wb,
      sheet = sheet_label,
      x = d %>% select(-area_scale_sheet),
      rowNames = FALSE,
      tableStyle = "TableStyleLight9"
    )
    
    message(
      sheet_key, ": ", nrow(d), " taxa (",
      sum(d$needs_area_review %in% 1, na.rm = TRUE),
      " to review)"
    )
  }
}

# ---- Presence/absence matrix ----

message("\n=== Creating the presence/absence matrix ===")

all_species <- per_area_taxon %>%
  distinct(
    wcvp_final_id,
    final_reconciled_name,
    wcvp_final_name,
    wfo_final_id,
    wfo_final_name
  ) %>%
  arrange(final_reconciled_name, wcvp_final_id)

message("  Unique taxa: ", nrow(all_species))

areas <- per_area_taxon %>%
  distinct(area_scale_sheet) %>%
  filter(!is.na(area_scale_sheet), area_scale_sheet != "") %>%
  arrange(area_scale_sheet) %>%
  pull(area_scale_sheet)

message("  Areas: ", length(areas))

matrix_data <- all_species

for (area_name in areas) {
  
  present_ids <- per_area_taxon %>%
    filter(area_scale_sheet == area_name) %>%
    distinct(wcvp_final_id) %>%
    pull(wcvp_final_id)
  
  matrix_data[[area_name]] <- as.integer(
    matrix_data$wcvp_final_id %in% present_ids
  )
  
  message(
    "    Added column ", area_name,
    " (", sum(matrix_data[[area_name]], na.rm = TRUE),
    " taxa present)"
  )
}

area_cols <- intersect(areas, names(matrix_data))

matrix_data <- matrix_data %>%
  mutate(
    n_areas_present = rowSums(
      as.data.frame(across(all_of(area_cols))),
      na.rm = TRUE
    ),
    areas_list = apply(
      select(., all_of(area_cols)),
      1,
      function(x) {
        present <- area_cols[!is.na(x) & x == 1]
        
        if (length(present) == 0) {
          NA_character_
        } else {
          paste(present, collapse = "; ")
        }
      }
    )
  )

matrix_data <- matrix_data %>%
  select(
    wcvp_final_id,
    final_reconciled_name,
    any_of(c(
      "wcvp_final_name",
      "wfo_final_id",
      "wfo_final_name"
    )),
    n_areas_present,
    areas_list,
    all_of(area_cols)
  )

addWorksheet(wb, "PRESENCE_ABSENCE_MATRIX")

writeDataTable(
  wb,
  sheet = "PRESENCE_ABSENCE_MATRIX",
  x = matrix_data,
  rowNames = FALSE,
  tableStyle = "TableStyleLight9"
)

freezePane(
  wb,
  sheet = "PRESENCE_ABSENCE_MATRIX",
  firstActiveRow = 2,
  firstActiveCol = min(6, ncol(matrix_data) + 1)
)

if (length(area_cols) > 0 && nrow(matrix_data) > 0) {
  
  green_style <- createStyle(
    fgFill = "#C6EFCE",
    fontColour = "#006100"
  )
  
  for (col_name in area_cols) {
    col_idx <- which(names(matrix_data) == col_name)
    
    conditionalFormatting(
      wb,
      sheet = "PRESENCE_ABSENCE_MATRIX",
      cols = col_idx,
      rows = 2:(nrow(matrix_data) + 1),
      rule = "==1",
      style = green_style
    )
  }
}

message(
  "  Matrix created: ", nrow(matrix_data),
  " taxa x ", length(area_cols), " areas"
)

# ---- Save workbook ----

checklist_path <- file.path(
  output_dir,
  "checklists_by_area.xlsx"
)

saveWorkbook(
  wb,
  checklist_path,
  overwrite = TRUE
)

message("\nWritten: ", checklist_path)

write_csv(per_area_taxon, file.path(output_dir, "checklists_all_areas.csv"))
message("Written: ", file.path(output_dir, "checklists_all_areas.csv"))
