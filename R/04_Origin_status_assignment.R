# =============================================================================
# 04_Origin_status_assignment.R
# Origin and status in Piedmont and Italy (Bartolucci et al. 2024; Galasso et al. 2024) and WCVP distribution in Italy
# Piedmont urban plant biodiversity
#
# Input:  Piemonte_reconciled.xlsx
#         Input/wcvp_names_2606.csv, Input/wcvp_distribution_2606.csv
# Output: output/origin_status/origin_status_full.csv and .xlsx
# =============================================================================

library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)

checklist_xlsx <- "Piemonte_reconciled.xlsx"
wcvp_backbone_csv <- "Input/wcvp_names_2606.csv"
wcvp_distribution_csv <- "Input/wcvp_distribution_2606.csv"
output_dir <- "output/origin_status"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Regional distribution (Bartolucci/Galasso) ----
region_cols <- c("VDA", "PIE", "LOM", "TAA", "VEN", "FVG", "LIG", "EMR", "RSM",
                 "TOS", "MAR", "UMB", "LAZ",
                 "ABR", "MOL", "PUG", "CAM", "BAS", "CAL",
                 "SIC", "SAR")

macro_region_map <- c(
  VDA = "Nord", PIE = "Nord", LOM = "Nord", TAA = "Nord", VEN = "Nord",
  FVG = "Nord", LIG = "Nord", EMR = "Nord",
  RSM = "Centro", TOS = "Centro", MAR = "Centro", UMB = "Centro", LAZ = "Centro",
  ABR = "Sud", MOL = "Sud", PUG = "Sud", CAM = "Sud", BAS = "Sud", CAL = "Sud",
  SIC = "Isole", SAR = "Isole"
)

is_present_code <- function(x) {
  x <- as.character(x)
  xc <- str_remove_all(x, "^\\(|\\)$")
  !is.na(x) & x != "" & !str_starts(xc, "NP")
}

label_distribution <- function(macro_string) {
  translate <- c(Centro = "Central", Isole = "Insular", Nord = "Northern", Sud = "Southern")
  sapply(macro_string, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    parts <- str_split(s, ";")[[1]]
    parts <- parts[parts != ""]
    if (length(parts) == 0) return(NA_character_)
    if (length(parts) == 4) return("Widespread")
    paste(translate[parts], collapse = "-")
  }, USE.NAMES = FALSE)
}

extract_distribution <- function(df) {
  present_mat <- sapply(region_cols, function(r) is_present_code(df[[r]]))
  if (nrow(df) == 1) present_mat <- matrix(present_mat, nrow = 1, dimnames = list(NULL, region_cols))
  regions_present <- apply(present_mat, 1, function(row) paste(region_cols[row], collapse = "; "))
  macro_present <- apply(present_mat, 1, function(row) paste(sort(unique(macro_region_map[region_cols[row]])), collapse = ";"))
  has_nord <- str_detect(macro_present, "\\bNord\\b")
  has_centro <- str_detect(macro_present, "\\bCentro\\b")
  has_sud_isole <- str_detect(macro_present, "\\bSud\\b|\\bIsole\\b")
  tibble(
    regions_present = regions_present,
    distribution_category = label_distribution(macro_present),
    distribution_disjunct = has_nord & has_sud_isole & !has_centro
  )
}

# ---- PIE code parser ----

parse_pie_code <- function(x) {
  x <- str_trim(x)
  is_tentative <- str_starts(x, "\\(") & str_ends(x, "\\)")
  x_clean <- str_remove_all(x, "^\\(|\\)$")
  
  m1 <- str_match(x_clean, "^(P|NP|NC|D|EX|DD)(?:\\s+A(?:\\s+(NAT|CAS|INV))?)?(\\s+C)?$")
  m2 <- str_match(x_clean, "^A(?:\\s+(NAT|CAS|INV))?$")
  
  tibble(
    pie_raw             = x,
    pie_clean           = x_clean,
    pie_is_tentative    = is_tentative,
    pie_base_code       = m1[, 2],
    pie_alien_substatus = coalesce(m1[, 3], m2[, 2]),
    pie_has_alien       = !is.na(m1[, 3]) | !is.na(m2[, 1]) | (!is.na(m1[, 1]) & str_detect(x_clean, "\\bA\\b")),
    pie_cryptogenic_note = !is.na(m1[, 4]),
    pie_unparsed        = is.na(m1[, 1]) & is.na(m2[, 1]) & !is.na(x) & x != ""
  )
}

# ---- Load reconciled sheets ----

native_raw <- read_excel(checklist_xlsx, sheet = "Bartolucci_native", col_types = "text")
esotiche_raw <- read_excel(checklist_xlsx, sheet = "Galasso_esotiche", col_types = "text")

message("Native: ", nrow(native_raw), " rows | Alien: ", nrow(esotiche_raw), " rows")

# ---- Native side ----

resolve_alien_origin <- function(pie_base_code, pie_alien_substatus, pie_raw_clean) {
  case_when(
    pie_alien_substatus == "NAT" ~ "alien_naturalized",
    pie_alien_substatus == "INV" ~ "alien_invasive",
    pie_alien_substatus == "CAS" ~ "alien_casual",
    pie_base_code == "P" & is.na(pie_alien_substatus) & str_detect(pie_raw_clean, "\\bA$") ~ "alien_casual_undefined",
    pie_base_code == "EX" ~ "alien_extinct_or_possibly_extinct",
    pie_base_code == "NC" ~ "alien_no_longer_recorded",
    pie_base_code == "D"  ~ "alien_doubtful",
    pie_base_code == "DD" ~ "alien_data_deficient",
    TRUE ~ "alien_unspecified_substatus"
  )
}

native_status <- native_raw %>%
  filter(!is.na(wcvp_final_id)) %>%
  bind_cols(parse_pie_code(.$PIE)) %>%
  bind_cols(extract_distribution(.)) %>%
  rename(national_status_native_raw = matches("^NATIONAL STATUS")) %>%
  transmute(
    wcvp_final_id,
    source_side = "native",
    original_taxon = Taxon,
    final_reconciled_name = final_name_manual,
    regions_present, distribution_category, distribution_disjunct,
    origin_piedmont = case_when(
      pie_has_alien ~ resolve_alien_origin(pie_base_code, pie_alien_substatus, pie_clean),
      pie_base_code %in% c("P")  ~ "native",
      pie_base_code %in% c("NC") ~ "native_no_longer_recorded",
      pie_base_code %in% c("EX") ~ "native_extinct_or_possibly_extinct",
      pie_base_code %in% c("NP") ~ "recorded_by_mistake",
      pie_base_code %in% c("D")  ~ "native_doubtful",
      TRUE                        ~ NA_character_
    ),
    alien_substatus_piedmont = pie_alien_substatus,
    uncertain_piedmont = pie_is_tentative,
    cryptogenic_piedmont = pie_cryptogenic_note,
    national_status_native = national_status_native_raw
  )

# ---- Alien side (Galasso) ----

esotiche_status <- esotiche_raw %>%
  filter(!is.na(wcvp_final_id)) %>%
  bind_cols(parse_pie_code(.$PIE)) %>%
  bind_cols(extract_distribution(.)) %>%
  transmute(
    wcvp_final_id,
    source_side = "alien",
    original_taxon = Taxon,
    final_reconciled_name = final_name_manual,
    regions_present, distribution_category, distribution_disjunct,
    origin_piedmont = case_when(
      pie_base_code %in% c("NP") ~ "recorded_by_mistake",
      !is.na(pie_base_code) | pie_has_alien ~ resolve_alien_origin(pie_base_code, pie_alien_substatus, pie_clean),
      TRUE ~ NA_character_
    ),
    status_piedmont = pie_alien_substatus,
    uncertain_piedmont = pie_is_tentative,
    cultivated_flag_esotiche = `CLT/FER` %in% c("CLT", "CLT?"),
    feral_flag = `CLT/FER` == "FER",
    status_italy = `ALIEN STATUS`,
    introduction_period = case_when(
      ESO == "A" ~ "archaeophyte",
      ESO == "N" ~ "neophyte",
      TRUE ~ NA_character_
    ),
    is_ias = IAS == "YES"
  )

# ---- Merge both sides by wcvp_final_id ----

overlap_ids <- intersect(native_status$wcvp_final_id, esotiche_status$wcvp_final_id)
if (length(overlap_ids) > 0) {
  message(length(overlap_ids), " wcvp_final_id occur in both the native and the alien file; ",
          "mostly agricultural crops, flagged as 'native_and_cultivated_crop'.")
}

native_dedup <- native_status %>%
  mutate(status_italy = "native", cultivated = FALSE, feral_flag = FALSE, is_ias = FALSE) %>%
  group_by(wcvp_final_id) %>%
  mutate(original_taxon_all = paste(sort(unique(original_taxon)), collapse = " | ")) %>%
  ungroup() %>%
  distinct(wcvp_final_id, .keep_all = TRUE) %>%
  mutate(original_taxon = original_taxon_all) %>%
  select(-original_taxon_all)

esotiche_dedup <- esotiche_status %>%
  mutate(cultivated = cultivated_flag_esotiche) %>%
  group_by(wcvp_final_id) %>%
  mutate(original_taxon_all = paste(sort(unique(original_taxon)), collapse = " | ")) %>%
  ungroup() %>%
  distinct(wcvp_final_id, .keep_all = TRUE) %>%
  mutate(original_taxon = original_taxon_all) %>%
  select(-original_taxon_all)

origin_status_overlap <- native_dedup %>%
  filter(wcvp_final_id %in% overlap_ids) %>%
  mutate(
    origin_piedmont = "native_and_cultivated_crop",
    cultivated = TRUE,
    also_alien_in_piedmont = TRUE
  )

origin_status_by_id <- bind_rows(
  native_dedup %>% filter(!wcvp_final_id %in% overlap_ids),
  esotiche_dedup %>% filter(!wcvp_final_id %in% overlap_ids),
  origin_status_overlap
)

# ---- Review flags ----

origin_status_by_id <- origin_status_by_id %>%
  mutate(
    needs_manual_review = coalesce(uncertain_piedmont, FALSE) |
      coalesce(also_alien_in_piedmont, FALSE) |
      coalesce(cryptogenic_piedmont, FALSE) |
      is.na(origin_piedmont),
    review_reason = case_when(
      is.na(origin_piedmont) ~ "origin_piedmont not determined from the PIE code",
      coalesce(origin_piedmont, "") == "native_and_cultivated_crop" ~ "agricultural crop: same taxon is both native wild and widely cultivated",
      coalesce(cryptogenic_piedmont, FALSE) ~ "doubtful or unknown origin in Piedmont (cryptogenic, code 'C')",
      coalesce(uncertain_piedmont, FALSE) ~ "PIE code in parentheses (tentative record)",
      TRUE ~ NA_character_
    )
  )

message("Taxa with a direct Bartolucci/Galasso match: ", nrow(origin_status_by_id))

# ---- Fallback: inherit origin from the parent species ----

all_ids_in_checklist <- read_excel(checklist_xlsx, sheet = "checklist_finale", col_types = "text") %>%
  distinct(wcvp_final_id) %>% filter(!is.na(wcvp_final_id))

checklist_names <- read_excel(checklist_xlsx, sheet = "checklist_finale", col_types = "text") %>%
  filter(!is.na(wcvp_final_id), !is.na(original_raw), !is.na(final_name_manual)) %>%
  distinct(wcvp_final_id, original_raw, final_name_manual, final_author_manual)

unmatched <- all_ids_in_checklist %>% anti_join(origin_status_by_id, by = "wcvp_final_id")
unmatched_named <- unmatched %>%
  left_join(checklist_names, by = "wcvp_final_id") %>%
  distinct(wcvp_final_id, original_raw, final_name_manual, final_author_manual)

message(nrow(unmatched), " taxa without a direct match; trying the parent-species fallback.")

if (!exists("wcvp_names_tbl")) {
  message("Loading the WCVP backbone from '", wcvp_backbone_csv, "'.")
  if (!file.exists(wcvp_backbone_csv)) stop("WCVP file not found in '", wcvp_backbone_csv, "'.")
  wcvp_names_tbl <- if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(wcvp_backbone_csv, sep = "|", data.table = FALSE, encoding = "UTF-8", quote = "")
  } else {
    readr::read_delim(wcvp_backbone_csv, delim = "|", show_col_types = FALSE,
                      locale = readr::locale(encoding = "UTF-8"),
                      col_types = readr::cols(.default = readr::col_character()), quote = "")
  }
  names(wcvp_names_tbl) <- trimws(names(wcvp_names_tbl))
  id_col_idx <- which(tolower(names(wcvp_names_tbl)) == "plant_name_id")[1]
  if (is.na(id_col_idx)) stop("Column 'plant_name_id' not found in the WCVP file.")
  wcvp_names_tbl$wcvp_id <- suppressWarnings(as.integer(wcvp_names_tbl[[id_col_idx]]))
}

species_fallback <- tibble()
if ("parent_plant_name_id" %in% names(wcvp_names_tbl)) {
  parent_map <- wcvp_names_tbl %>%
    transmute(wcvp_final_id = as.character(wcvp_id),
              parent_species_id = as.character(parent_plant_name_id)) %>%
    filter(!is.na(parent_species_id), parent_species_id != "")
  
  unmatched_with_parent <- unmatched %>%
    left_join(parent_map, by = "wcvp_final_id") %>%
    filter(!is.na(parent_species_id))
  
  species_fallback <- unmatched_with_parent %>%
    left_join(
      origin_status_by_id %>%
        transmute(parent_species_id = wcvp_final_id, source_side, origin_piedmont, status_piedmont, status_italy,
                  cultivated, feral_flag, is_ias, cryptogenic_piedmont, introduction_period,
                  distribution_category, regions_present, distribution_disjunct),
      by = "parent_species_id"
    ) %>%
    filter(!is.na(origin_piedmont)) %>%
    left_join(unmatched_named %>% select(wcvp_final_id, original_taxon = original_raw,
                                         final_reconciled_name = final_name_manual),
              by = "wcvp_final_id") %>%
    mutate(
      also_alien_in_piedmont = FALSE,
      uncertain_piedmont = FALSE,
      needs_manual_review = TRUE,
      review_reason = "origin inherited from the parent species (infraspecific taxon not reported separately in Bartolucci/Galasso)"
    ) %>%
    select(-parent_species_id)
  
  message(nrow(species_fallback), " unmatched taxa resolved via the parent-species fallback.")
} else {
  warning("Column 'parent_plant_name_id' not found: parent-species fallback disabled.")
}

# ---- Taxa without any match ----

still_unmatched <- unmatched_named %>%
  { if (nrow(species_fallback) > 0) anti_join(., species_fallback, by = "wcvp_final_id") else . } %>%
  transmute(
    wcvp_final_id, source_side = "no_match",
    original_taxon = original_raw, final_reconciled_name = final_name_manual,
    regions_present = NA_character_, distribution_category = NA_character_, distribution_disjunct = NA,
    origin_piedmont = NA_character_, alien_substatus_piedmont = NA_character_,
    uncertain_piedmont = FALSE, cryptogenic_piedmont = FALSE,
    also_alien_in_piedmont = FALSE,
    needs_manual_review = TRUE,
    review_reason = "no match in Bartolucci/Galasso, either direct or via parent species"
  )

message(nrow(still_unmatched), " taxa remain without any match; ",
        "still included in the final table (origin_piedmont = NA).")

origin_status_by_id <- bind_rows(origin_status_by_id, species_fallback, still_unmatched)

stopifnot(n_distinct(origin_status_by_id$wcvp_final_id) == nrow(all_ids_in_checklist))
message("Check: ", n_distinct(origin_status_by_id$wcvp_final_id), " unique ids = ",
        nrow(all_ids_in_checklist), " total ids in the checklist: full coverage confirmed.")

# ---- WCVP distribution for Italy ----

if (file.exists(wcvp_distribution_csv)) {
  distribution <- read.table(wcvp_distribution_csv, sep = "|", header = TRUE, quote = "", fill = TRUE, encoding = "UTF-8")
  italy_distribution <- distribution %>%
    select(plant_name_id, area, introduced) %>%
    filter(area == "Italy") %>%
    transmute(wcvp_final_id = as.character(plant_name_id),
              wcvp_in_italy = TRUE,
              wcvp_introduced_in_italy = suppressWarnings(as.integer(introduced)) == 1)
  
  origin_status_by_id <- origin_status_by_id %>%
    left_join(italy_distribution, by = "wcvp_final_id") %>%
    mutate(
      wcvp_in_italy = coalesce(wcvp_in_italy, FALSE),
      wcvp_distribution_conflict = case_when(
        !wcvp_in_italy ~ NA,
        str_starts(origin_piedmont, "native") & wcvp_introduced_in_italy ~ TRUE,
        str_starts(origin_piedmont, "alien") & !wcvp_introduced_in_italy ~ TRUE,
        is.na(origin_piedmont) ~ NA,
        TRUE ~ FALSE
      ),
      needs_manual_review = needs_manual_review | coalesce(wcvp_distribution_conflict, FALSE),
      review_reason = if_else(
        coalesce(wcvp_distribution_conflict, FALSE) & is.na(review_reason),
        "WCVP distribution for Italy does not confirm the assigned origin_piedmont",
        review_reason
      )
    )
  
  message(sum(origin_status_by_id$wcvp_in_italy), " taxa (out of ", nrow(origin_status_by_id),
          ") have a WCVP distribution record for Italy.")
  message(sum(origin_status_by_id$wcvp_distribution_conflict, na.rm = TRUE),
          " conflict with origin_piedmont (see 'wcvp_distribution_conflict').")
} else {
  warning("WCVP distribution file not found in '", wcvp_distribution_csv, "': wcvp_* columns set to NA.")
  origin_status_by_id$wcvp_in_italy <- NA
  origin_status_by_id$wcvp_introduced_in_italy <- NA
  origin_status_by_id$wcvp_distribution_conflict <- NA
}

origin_status_by_id <- origin_status_by_id %>%
  mutate(review_row_id = row_number()) %>%
  select(review_row_id, wcvp_final_id, original_taxon, final_reconciled_name,
         origin_piedmont, alien_substatus_piedmont, status_piedmont, status_italy,
         cultivated, feral_flag, is_ias, introduction_period,
         wcvp_in_italy, wcvp_introduced_in_italy, wcvp_distribution_conflict,
         distribution_category, regions_present, distribution_disjunct,
         needs_manual_review, review_reason, everything())

# ---- Export ----

write.csv(origin_status_by_id, file.path(output_dir, "origin_status_full.csv"), row.names = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "origin_status_full")
writeDataTable(wb, "origin_status_full", origin_status_by_id)
addWorksheet(wb, "review_needed")
writeDataTable(wb, "review_needed", origin_status_by_id %>% filter(needs_manual_review))
saveWorkbook(wb, file.path(output_dir, "origin_status_full.xlsx"), overwrite = TRUE)

message("\n=== DONE ===")
message("Written: ", file.path(output_dir, "origin_status_full.xlsx"))
message("Total rows (= unique taxa in the checklist): ", nrow(origin_status_by_id))
message("Rows needing review: ", sum(origin_status_by_id$needs_manual_review, na.rm = TRUE))
message("Ready as input for the cultivation flagging.")

