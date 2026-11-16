# =============================================================================
# 02_Reconciliation_Piemonte.R
# Name parsing and WCVP/WFO reconciliation of all source datasets
# Piedmont urban plant biodiversity
#
# Input:  Input/ (WCVP and WFO backbones, IPNI-WFO crosswalk, Bartolucci, Galasso, RAMI,
#         Flora Urbica, BBPAO, tree inventories, EASIN extract, Drevojan, GBIF cleaned records)
#         Part B: Input/processed/Piedmont_name_reconciliation_reviewed.xlsx
# Output: Part A: Piedmont_name_reconciliation_to_review.xlsx (unique names, for manual review)
#         Part B: Piemonte_reconciled.xlsx (one sheet per source plus checklist_finale)
# =============================================================================

library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(openxlsx)
library(rWCVP)
library(WorldFlora)
library(purrr)

# ---- Part A: parsing, matching and export for manual review ----

# ---- Backbone files ----
wcvp_backbone_csv <- "Input/wcvp_names_2606.csv"
wfo_backbone_csv  <- "Input/_DwC_backbone_R_2606/classification.csv"
ipni_to_wfo_csv   <- "Input/ipni_to_wfo.csv"
use_wfo <- TRUE

output_intermediate_xlsx <- "Piedmont_name_reconciliation_to_review.xlsx"

TEST_MODE <- FALSE
TEST_N_ROWS <- 50

# ---- Source datasets ----
sheet_config <- tibble::tribble(
  ~file_path,                                     ~sheet_name,             ~schema,
  "Input/Flora_Bartolucci_native.xlsx",           "Native",                "taxon",
  "Input/Flora_Galasso_esotiche.xlsx",         "flora_esotiche",        "taxon",
  "Input/Alberi_monumentali_Piemonte.xlsx",       "LAU_FUA_only",          "rami",
  "Input/UrbicaTorino1993Tables1e2.xlsx",         "Tab.1.Censite",         "urbica1",
  "Input/UrbicaTorino1993Tables1e2.xlsx",         "Tab.2.Ornam",           "urbica2",
  "Input/Records_BBPAO.xlsx",                     "Entries",               "cited",
  "Input/occ_IT_Plantae_all_clean.csv",           NA_character_,           "nameonly_easin",
  "Input/Alberi_coord_260313_filtered.csv",       NA_character_,           "nameonly_alb_to",
  "Input/alberi_Alessandria_260313_filtered.csv", NA_character_,           "nameonly_alb_al",
  "Input/Life_form_Drevojan_2023.xlsx",           "FloraVegEU-2023-01-03", "nameonly_drevojan",
  "Input/gbif_clean_occ_herb.csv",                NA_character_,           "gbif"
)

col_map <- list(
  nameonly_easin    = list(name = "SpeciesName"),
  nameonly_alb_to   = list(name = "descr_specie"),
  nameonly_alb_al   = list(name = "Essenza"),
  nameonly_drevojan = list(name = "FloraVeg.Taxon"),
  rami    = list(binom = "binomial", aut = "author", infra = "infraspecific"),
  urbica1 = list(binom = "Species", aut = "Author"),
  urbica2 = list(binom = "Specie", aut = "Autori"),
  cited   = list(binom = "taxon_as_cited", aut = "authors_as_cited", infra = "infra_as_cited"),
  gbif    = list(genus = "genus", sp = "specificEpithet", infra = "infraspecificEpithet",
                  rank = "taxonRank", full = "scientificName")
)

# ---- Shared helpers ----

normalize_hybrid_sign <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\s+", " ") %>% str_trim()
  x <- str_replace_all(x, "(\\b[\\p{Lu}][\\p{Ll}-]+\\b)\\s+[xX]\\s+(\\b[\\p{Ll}-]+\\b)", "\\1 × \\2")
  x <- str_replace_all(x, "\\s*×\\s*", " × ")
  x <- str_replace_all(x, "\\s+", " ") %>% str_trim()
  x
}

is_formula_hybrid <- function(x) {
  x <- normalize_hybrid_sign(x)
  str_detect(x, ".+\\s×\\s.+") &
    str_detect(x, "\\b[A-Z][a-zA-Z-]+\\b.*\\s×\\s\\b[A-Z][a-zA-Z-]+\\b")
}

is_cultivar_ssp <- function(ssp) {
  !is.na(ssp) & str_detect(str_trim(ssp), "^['\u2018\u2019]")
}

parse_ssp_rank_epithet <- function(ssp) {
  rank_pat <- "^(subsp\\.?|ssp\\.?|var\\.?|f\\.?|forma|subvar\\.?|nothosubsp\\.?|nothovar\\.?)\\s+([\\p{Ll}][\\p{L}-]+)"
  m <- str_match(str_trim(ssp), rank_pat)
  rank_raw <- m[, 2]; epithet <- m[, 3]
  rank <- dplyr::case_when(
    rank_raw %in% c("ssp", "ssp.", "subsp", "subsp.") ~ "subsp.",
    rank_raw %in% c("nothosubsp", "nothosubsp.") ~ "nothosubsp.",
    rank_raw %in% c("f", "f.", "forma") ~ "f.",
    rank_raw %in% c("var", "var.") ~ "var.",
    rank_raw %in% c("nothovar", "nothovar.") ~ "nothovar.",
    rank_raw %in% c("subvar", "subvar.") ~ "subvar.",
    TRUE ~ rank_raw
  )
  fallback_epithet <- ifelse(is.na(epithet) & !is.na(ssp) & nzchar(str_trim(ssp)), str_trim(ssp), NA_character_)
  list(rank = rank, epithet = epithet, fallback_epithet = fallback_epithet)
}

# ---- Parser for single-column taxon strings (Bartolucci/Galasso) ----
parse_taxon_string <- function(taxon) {
  pat <- paste0(
    "^([\u00d7xX]\\s*)?([A-Za-z\u00c0-\u00ff-]+)\\s+",
    "([\u00d7xX]\\s*)?([a-z\u00e0-\u00ff.-]+)\\s+",
    "(.*?)",
    "(?:(?<=\\s)(subsp\\.|nothosubsp\\.|var\\.|nothovar\\.|subvar\\.|forma|f\\.)(?=\\s)\\s*",
    "([\u00d7xX]?[a-z\u00e0-\u00ff-]+)\\s*(.*))?$"
  )
  m <- str_match(str_trim(taxon), pat)
  species_is_infrageneric_rank <- m[, 5] %in% c("sect.", "subsect.", "ser.", "subser.", "subg.", "subgen.")
  list(
    genus       = m[, 3],
    species     = m[, 5],
    author1     = na_if(str_trim(m[, 6]), ""),
    infra_rank  = m[, 7],
    infra_epithet = m[, 8],
    author2     = na_if(str_trim(m[, 9]), ""),
    parse_failed = is.na(m[, 1]) | species_is_infrageneric_rank
  )
}

placeholder_values <- c("VUOTO", "SC", "")

# ---- Readers by schema ----

read_file_any <- function(file_path, sheet_name) {
  ext <- tolower(tools::file_ext(file_path))
  df <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(file_path, sheet = sheet_name) %>% as.data.frame()
  } else {
    readr::read_csv(file_path, show_col_types = FALSE) %>% as.data.frame()
  }
  names(df) <- trimws(names(df))
  df
}

compose_common <- function(row_id, source_file, source_sheet, row_in_sheet,
                            is_placeholder, gen_sp_mismatch, is_cultivar, cultivar_epithet,
                            parsed_name, parsed_author, is_not_species_level = FALSE) {
  original_raw <- ifelse(!is.na(parsed_author) & nzchar(parsed_author) & !is.na(parsed_name),
                          paste(parsed_name, parsed_author), parsed_name)
  data.frame(
    row_id = row_id, source_file = source_file, source_sheet = source_sheet,
    row_in_sheet = row_in_sheet,
    is_placeholder = is_placeholder, gen_sp_mismatch = gen_sp_mismatch,
    is_cultivar = is_cultivar, cultivar_epithet = cultivar_epithet,
    is_not_species_level = is_not_species_level,
    parsed_name = normalize_hybrid_sign(parsed_name), parsed_author = na_if(parsed_author, ""),
    original_raw = normalize_hybrid_sign(original_raw),
    is_formula_hybrid = is_formula_hybrid(original_raw),
    stringsAsFactors = FALSE
  )
}

read_and_compose_sheet <- function(file_path, sheet_name, schema) {
  df <- read_file_any(file_path, sheet_name)
  if (TEST_MODE) df <- head(df, TEST_N_ROWS)
  n <- nrow(df)
  row_id <- paste(basename(file_path), ifelse(is.na(sheet_name), "NA", sheet_name), seq_len(n), sep = "::")

  if (schema == "taxon") {
    cm <- col_map[["nameonly_easin"]]
    taxon_col <- df[["Taxon"]]
    parsed <- parse_taxon_string(taxon_col)
    is_placeholder <- is.na(taxon_col) | !nzchar(str_trim(as.character(taxon_col)))
    is_not_species_level <- !is_placeholder & parsed$parse_failed
    ssp_parsed_rank <- parsed$infra_rank
    ssp_parsed_epithet <- parsed$infra_epithet
    has_infra <- !is.na(ssp_parsed_rank) & !is.na(ssp_parsed_epithet)
    name_for_match <- ifelse(is_not_species_level, NA_character_,
                              ifelse(has_infra,
                                     paste(parsed$genus, parsed$species, ssp_parsed_rank, ssp_parsed_epithet),
                                     paste(parsed$genus, parsed$species)))
    author_to_use <- ifelse(has_infra, coalesce(parsed$author2, parsed$author1), parsed$author1)
    out <- compose_common(row_id, basename(file_path), sheet_name, seq_len(n),
                           is_placeholder, FALSE, FALSE, NA_character_,
                           name_for_match, author_to_use, is_not_species_level)
    out$original_raw <- ifelse(is_not_species_level, str_trim(as.character(taxon_col)), out$original_raw)

  } else if (schema %in% c("rami", "urbica1", "urbica2", "cited")) {
    cm <- col_map[[schema]]
    binom <- str_trim(as.character(df[[cm$binom]]))
    aut   <- str_trim(as.character(df[[cm$aut]]))
    infra <- if (!is.null(cm$infra) && cm$infra %in% names(df)) str_trim(as.character(df[[cm$infra]])) else NA_character_
    is_placeholder <- is.na(binom) | !nzchar(binom) | (binom %in% placeholder_values)
    ssp_parsed <- parse_ssp_rank_epithet(infra)
    has_infra <- !is.na(ssp_parsed$rank) & !is.na(ssp_parsed$epithet)
    has_infra_fallback <- is.na(ssp_parsed$rank) & !is.na(ssp_parsed$fallback_epithet)
    epithet_use <- coalesce(ssp_parsed$epithet, ssp_parsed$fallback_epithet)
    aut_rank_pat <- "^(.*?)\\s+(subsp\\.|nothosubsp\\.|var\\.|nothovar\\.|subvar\\.|forma|f\\.)\\s+([×xX]?[a-zà-ÿ-]+)\\s*(.*)$"
    aut_m <- str_match(aut, aut_rank_pat)
    aut_has_embedded_infra <- !is.na(aut_m[, 1]) & !has_infra & !has_infra_fallback
    embedded_author1 <- na_if(str_trim(aut_m[, 2]), "")
    embedded_rank    <- aut_m[, 3]
    embedded_epithet <- aut_m[, 4]
    embedded_author2 <- na_if(str_trim(aut_m[, 5]), "")
    name_for_match <- dplyr::case_when(
      is_placeholder ~ NA_character_,
      has_infra ~ paste(binom, ssp_parsed$rank, epithet_use),
      has_infra_fallback ~ paste(binom, epithet_use),
      aut_has_embedded_infra ~ paste(binom, embedded_rank, embedded_epithet),
      TRUE ~ binom
    )
    aut_final <- dplyr::if_else(aut_has_embedded_infra,
                                 coalesce(embedded_author2, embedded_author1),
                                 aut)
    out <- compose_common(row_id, basename(file_path), sheet_name, seq_len(n),
                           is_placeholder, FALSE, FALSE, NA_character_,
                           name_for_match, aut_final, FALSE)

  } else if (str_starts(schema, "nameonly")) {
    cm <- col_map[[schema]]
    nm <- str_trim(as.character(df[[cm$name]]))
    is_placeholder <- is.na(nm) | !nzchar(nm) | (nm %in% placeholder_values)
    out <- compose_common(row_id, basename(file_path), sheet_name, seq_len(n),
                           is_placeholder, FALSE, FALSE, NA_character_,
                           nm, NA_character_, FALSE)

  } else if (schema == "gbif") {
    cm <- col_map[["gbif"]]
    genus <- str_trim(as.character(df[[cm$genus]]))
    sp    <- str_trim(as.character(df[[cm$sp]]))
    infra <- str_trim(as.character(df[[cm$infra]]))
    rank  <- str_trim(as.character(df[[cm$rank]]))
    full  <- str_trim(as.character(df[[cm$full]]))
    binom <- str_trim(paste(genus, sp))
    is_placeholder <- is.na(genus) | !nzchar(genus)
    has_infra <- !is.na(infra) & nzchar(infra)
    rank_map <- c(SUBSPECIES = "subsp.", VARIETY = "var.", FORM = "f.", FORMA = "f.",
                  SUBVARIETY = "subvar.")
    rank_abbrev <- ifelse(toupper(rank) %in% names(rank_map),
                           rank_map[toupper(rank)], tolower(rank))
    name_for_match <- ifelse(is_placeholder, NA_character_,
                              ifelse(has_infra, paste(binom, rank_abbrev, infra), binom))
    aut_from_full <- str_trim(str_remove(full, fixed(binom, ignore_case = FALSE)))
    binom_found_in_full <- str_detect(full, fixed(binom, ignore_case = FALSE))
    aut_from_full <- ifelse(!binom_found_in_full, NA_character_, aut_from_full)
    rank_epithet_pat <- "^(subsp\\.|nothosubsp\\.|var\\.|nothovar\\.|subvar\\.|forma|f\\.)\\s+[a-zà-ÿ×x-]+\\s*"
    aut_from_full <- str_trim(str_remove(aut_from_full, rank_epithet_pat))
    aut_from_full <- str_trim(str_remove(aut_from_full, "^,"))
    aut_from_full <- ifelse(!is.na(aut_from_full) & nzchar(aut_from_full), aut_from_full, NA_character_)
    out <- compose_common(row_id, basename(file_path), sheet_name, seq_len(n),
                           is_placeholder, FALSE, FALSE, NA_character_,
                           name_for_match, na_if(aut_from_full, ""), FALSE)
  } else {
    stop("Unknown schema: ", schema)
  }
  dplyr::bind_cols(df, out)
}

message("Reading and composing ", nrow(sheet_config), " datasets...")
all_sheets <- purrr::pmap_dfr(sheet_config, function(file_path, sheet_name, schema) {
  message(" - ", file_path, " / ", ifelse(is.na(sheet_name), "(csv)", sheet_name), " (schema: ", schema, ")")
  read_and_compose_sheet(file_path, sheet_name, schema)
})

message("Total rows: ", nrow(all_sheets),
        " | placeholders: ", sum(all_sheets$is_placeholder, na.rm = TRUE),
        " | not species level: ", sum(all_sheets$is_not_species_level, na.rm = TRUE))

to_match <- all_sheets %>% filter(!is_placeholder, !is_not_species_level)

# ---- Global deduplication ----
uniq <- to_match %>%
  filter(!is.na(parsed_name), parsed_name != "") %>%
  distinct(parsed_name, parsed_author, .keep_all = FALSE) %>%
  mutate(match_id = dplyr::row_number()) %>%
  select(match_id, parsed_name, parsed_author)

message("Unique names to match: ", nrow(uniq))

# ---- WCVP matching ----

load_wcvp_backbone <- function(wcvp_csv, delim = "|") {
  if (!file.exists(wcvp_csv)) stop("WCVP backbone not found: ", wcvp_csv)
  message("Loading WCVP backbone: ", wcvp_csv)
  w <- if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(wcvp_csv, sep = delim, data.table = FALSE, encoding = "UTF-8", quote = "")
  } else {
    readr::read_delim(wcvp_csv, delim = delim, show_col_types = FALSE,
                      locale = readr::locale(encoding = "UTF-8"),
                      col_types = readr::cols(.default = readr::col_character()), quote = "") %>% as.data.frame()
  }
  names(w) <- trimws(names(w))
  nml <- tolower(names(w)); idx <- function(col) which(nml == col)[1]
  w$wcvp_id          <- suppressWarnings(as.integer(w[[idx("plant_name_id")]]))
  w$wcvp_name        <- as.character(w[[idx("taxon_name")]])
  w$wcvp_authors     <- dplyr::na_if(w[[idx("taxon_authors")]], "")
  w$wcvp_rank        <- w[[idx("taxon_rank")]]
  w$wcvp_status      <- w[[idx("taxon_status")]]
  w$wcvp_accepted_id <- suppressWarnings(as.integer(w[[idx("accepted_plant_name_id")]]))
  w$wcvp_name[w$wcvp_name == ""] <- NA_character_
  ipni_idx <- idx("ipni_id")
  w$wcvp_ipni_id <- if (!is.na(ipni_idx)) as.character(w[[ipni_idx]]) else NA_character_
  if (is.na(ipni_idx)) {
    warning("Column 'ipni_id' not found in the WCVP backbone.")
  }
  w
}

add_wcvp_accepted <- function(wcvp_matches, wcvp_names_tbl) {
  ensure_col <- function(df, col, fill = NA_character_) { if (!col %in% names(df)) df[[col]] <- fill; df }
  wcvp_matches <- wcvp_matches %>%
    ensure_col("wcvp_id", NA_integer_) %>% ensure_col("wcvp_name") %>%
    ensure_col("wcvp_authors") %>% ensure_col("wcvp_rank") %>%
    ensure_col("wcvp_status") %>% ensure_col("wcvp_accepted_id", NA_integer_)
  accepted_lookup <- wcvp_names_tbl %>%
    select(wcvp_id, accepted_wcvp_name = wcvp_name, accepted_wcvp_authors = wcvp_authors,
           accepted_wcvp_rank = wcvp_rank, accepted_wcvp_status = wcvp_status,
           accepted_wcvp_ipni_id = wcvp_ipni_id)
  wcvp_matches %>%
    left_join(accepted_lookup, by = c("wcvp_accepted_id" = "wcvp_id")) %>%
    mutate(
      wcvp_final_name = case_when(
        is.na(wcvp_id) ~ NA_character_,
        str_to_lower(wcvp_status) == "accepted" ~ wcvp_name,
        str_to_lower(wcvp_status) == "synonym" & !is.na(accepted_wcvp_name) ~ accepted_wcvp_name,
        str_to_lower(wcvp_status) == "unplaced" ~ wcvp_name,
        !is.na(accepted_wcvp_name) ~ accepted_wcvp_name,
        TRUE ~ wcvp_name
      ),
      wcvp_final_basis = case_when(
        is.na(wcvp_id) ~ "no_wcvp_match",
        str_to_lower(wcvp_status) == "accepted" ~ "accepted",
        str_to_lower(wcvp_status) == "synonym" ~ "synonym_to_accepted",
        str_to_lower(wcvp_status) == "unplaced" ~ "unplaced_keep",
        TRUE ~ "other_status"
      ),
      wcvp_matched_ipni_id = wcvp_ipni_id,
      ipni_fallback_to_matched = str_to_lower(wcvp_status) == "synonym" & is.na(accepted_wcvp_ipni_id),
      wcvp_final_accepted_ipni_id = case_when(
        is.na(wcvp_id) ~ NA_character_,
        str_to_lower(wcvp_status) == "accepted" ~ wcvp_ipni_id,
        str_to_lower(wcvp_status) == "synonym" & !is.na(accepted_wcvp_ipni_id) ~ accepted_wcvp_ipni_id,
        TRUE ~ wcvp_ipni_id
      ),
      wcvp_final_id = case_when(
        is.na(wcvp_id) ~ NA_integer_,
        str_to_lower(wcvp_status) == "accepted" ~ wcvp_id,
        !is.na(wcvp_accepted_id) ~ wcvp_accepted_id,
        TRUE ~ wcvp_id
      ),
      wcvp_final_authors = case_when(
        is.na(wcvp_id) ~ NA_character_,
        str_to_lower(wcvp_status) == "accepted" ~ wcvp_authors,
        !is.na(accepted_wcvp_authors) ~ accepted_wcvp_authors,
        TRUE ~ wcvp_authors
      ),
      wcvp_final_rank = case_when(
        is.na(wcvp_id) ~ NA_character_,
        str_to_lower(wcvp_status) == "accepted" ~ wcvp_rank,
        !is.na(accepted_wcvp_rank) ~ accepted_wcvp_rank,
        TRUE ~ wcvp_rank
      )
    )
}

wcvp_names_tbl <- load_wcvp_backbone(wcvp_backbone_csv)
wcvp_matches <- rWCVP::wcvp_match_names(
  names_df = uniq, wcvp_names = wcvp_names_tbl,
  name_col = "parsed_name", author_col = "parsed_author",
  id_col = "match_id", fuzzy = TRUE, progress_bar = TRUE
)
wcvp_candidates <- add_wcvp_accepted(wcvp_matches, wcvp_names_tbl)

wcvp_best <- wcvp_candidates %>%
  mutate(
    wcvp_status_safe = if ("wcvp_status" %in% names(.)) wcvp_status else NA_character_,
    status_rank = case_when(
      str_to_lower(wcvp_status_safe) == "accepted" ~ 1L,
      str_to_lower(wcvp_status_safe) == "synonym" ~ 2L,
      str_to_lower(wcvp_status_safe) == "unplaced" ~ 3L,
      is.na(wcvp_status_safe) ~ 99L, TRUE ~ 50L
    ),
    match_rank = case_when(
      "match_type" %in% names(.) & str_to_lower(match_type) == "exact" ~ 1L,
      "match_type" %in% names(.) & str_to_lower(match_type) == "fuzzy" ~ 2L, TRUE ~ 10L
    ),
    dist_rank = if ("fuzzy_dist" %in% names(.)) suppressWarnings(as.numeric(fuzzy_dist)) else NA_real_
  ) %>%
  arrange(match_id, status_rank, match_rank, dist_rank) %>%
  group_by(match_id) %>% slice(1) %>% ungroup() %>%
  select(match_id,
         wcvp_matched_id = wcvp_id, wcvp_matched_ipni_id, wcvp_matched_name = wcvp_name,
         wcvp_matched_authors = wcvp_authors, wcvp_matched_rank = wcvp_rank,
         wcvp_final_id, wcvp_final_ipni_id = wcvp_final_accepted_ipni_id,
         wcvp_final_name, wcvp_final_authors, wcvp_final_rank, wcvp_final_basis,
         ipni_fallback_to_matched)

# ---- WFO fuzzy matching ----

load_wfo_backbone <- function(backbone_csv) {
  if (!file.exists(backbone_csv)) stop("WFO backbone not found: ", backbone_csv)
  message("Loading WFO backbone: ", backbone_csv)
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(backbone_csv, data.table = FALSE, encoding = "UTF-8")
  } else {
    readr::read_csv(backbone_csv, show_col_types = FALSE)
  }
}

wfo_best <- NULL
wfo_check <- NULL

if (use_wfo) {
  WFO_backbone <- load_wfo_backbone(wfo_backbone_csv)

  wfo_in <- uniq %>% filter(!is.na(parsed_name), parsed_name != "") %>% select(match_id, parsed_name)
  message("WFO fuzzy matching on ", nrow(wfo_in), " unique names")

  wfo_candidates <- WorldFlora::WFO.match(
    spec.data = wfo_in, spec.name = "parsed_name", WFO.data = WFO_backbone,
    Fuzzy = 2, Fuzzy.min = FALSE
  )

  wfo_matched_raw <- wfo_candidates %>%
    mutate(
      submitted = tolower(trimws(.data$parsed_name)),
      sci = tolower(trimws(.data$scientificName)),
      status = tolower(trimws(.data$taxonomicStatus)),
      fuzzy_d = suppressWarnings(as.numeric(.data$Fuzzy.dist)),
      rank1 = ifelse(sci == submitted, 1L, 99L),
      rank3 = ifelse(isTRUE(.data$Matched), 1L, 99L),
      rank4 = ifelse(is.na(fuzzy_d), 9999, fuzzy_d)
    ) %>%
    arrange(.data$match_id, rank1, rank3, rank4, .data$Subseq) %>%
    group_by(.data$match_id) %>% slice(1) %>% ungroup() %>%
    transmute(
      match_id,
      wfo_matched_id = as.character(.data$taxonID),
      wfo_matched_name = .data$scientificName,
      wfo_matched_authors = dplyr::na_if(as.character(.data$scientificNameAuthorship), ""),
      wfo_matched_rank = as.character(.data$taxonRank),
      wfo_matched_status = status,
      wfo_accepted_id_of_matched = as.character(.data$acceptedNameUsageID)
    )

  wfo_accepted_lookup_id <- WFO_backbone %>%
    transmute(
      wfo_id = as.character(taxonID),
      accepted_wfo_name = as.character(scientificName),
      accepted_wfo_authors = dplyr::na_if(as.character(scientificNameAuthorship), ""),
      accepted_wfo_rank = as.character(taxonRank)
    )

  wfo_best <- wfo_matched_raw %>%
    left_join(wfo_accepted_lookup_id, by = c("wfo_accepted_id_of_matched" = "wfo_id")) %>%
    mutate(
      wfo_final_id = case_when(
        wfo_matched_status == "synonym" & !is.na(wfo_accepted_id_of_matched) ~ wfo_accepted_id_of_matched,
        TRUE ~ wfo_matched_id
      ),
      wfo_final_name = case_when(
        wfo_matched_status == "synonym" & !is.na(accepted_wfo_name) ~ accepted_wfo_name,
        TRUE ~ wfo_matched_name
      ),
      wfo_final_authors = case_when(
        wfo_matched_status == "synonym" & !is.na(accepted_wfo_authors) ~ accepted_wfo_authors,
        TRUE ~ wfo_matched_authors
      ),
      wfo_final_rank = case_when(
        wfo_matched_status == "synonym" & !is.na(accepted_wfo_rank) ~ accepted_wfo_rank,
        TRUE ~ wfo_matched_rank
      ),
      wfo_final_basis = case_when(
        wfo_matched_status == "accepted" ~ "accepted",
        wfo_matched_status == "synonym" & !is.na(accepted_wfo_name) ~ "synonym_to_accepted",
        TRUE ~ "kept_unresolved"
      )
    ) %>%
    select(match_id, wfo_matched_id, wfo_matched_name, wfo_matched_authors, wfo_matched_rank,
           wfo_final_id, wfo_final_name, wfo_final_authors, wfo_final_rank, wfo_final_basis)

  if (!file.exists(ipni_to_wfo_csv)) {
    warning("IPNI->WFO crosswalk not found in ", ipni_to_wfo_csv, "; check skipped.")
  } else {
    message("Loading IPNI->WFO crosswalk (check only): ", ipni_to_wfo_csv)
    crosswalk <- if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::fread(ipni_to_wfo_csv, data.table = FALSE, encoding = "UTF-8")
    } else {
      readr::read_csv(ipni_to_wfo_csv, show_col_types = FALSE)
    }
    crosswalk <- crosswalk %>%
      mutate(ipni_id_short = str_remove(ipni_id, "^urn:lsid:ipni\\.org:names:")) %>%
      group_by(ipni_id_short) %>%
      mutate(crosswalk_ambiguous = n() > 1) %>%
      slice(1) %>% ungroup()

    wfo_lookup <- WFO_backbone %>%
      transmute(
        wfo_id = as.character(taxonID),
        wfo_sciname = as.character(scientificName),
        wfo_status = as.character(taxonomicStatus),
        wfo_accepted_id = as.character(acceptedNameUsageID),
        wfo_authorship = dplyr::na_if(as.character(scientificNameAuthorship), ""),
        wfo_rank = as.character(taxonRank)
      )
    wfo_accepted_lookup <- wfo_lookup %>%
      select(wfo_id, accepted_wfo_sciname = wfo_sciname,
             accepted_wfo_authorship = wfo_authorship, accepted_wfo_rank = wfo_rank)

    wfo_check <- wcvp_best %>%
      filter(!is.na(wcvp_final_ipni_id)) %>%
      select(match_id, wcvp_final_ipni_id, ipni_fallback_to_matched) %>%
      left_join(crosswalk %>% select(ipni_id_short, wfo_id, crosswalk_ambiguous),
                by = c("wcvp_final_ipni_id" = "ipni_id_short")) %>%
      left_join(wfo_lookup, by = "wfo_id") %>%
      left_join(wfo_accepted_lookup, by = c("wfo_accepted_id" = "wfo_id")) %>%
      mutate(
        wfo_check_name = case_when(
          is.na(wfo_id) ~ NA_character_,
          str_to_lower(wfo_status) == "accepted" ~ wfo_sciname,
          !is.na(accepted_wfo_sciname) ~ accepted_wfo_sciname,
          TRUE ~ wfo_sciname
        ),
        wfo_check_authors = case_when(
          is.na(wfo_id) ~ NA_character_,
          str_to_lower(wfo_status) == "accepted" ~ wfo_authorship,
          !is.na(accepted_wfo_authorship) ~ accepted_wfo_authorship,
          TRUE ~ wfo_authorship
        ),
        wfo_check_rank = case_when(
          is.na(wfo_id) ~ NA_character_,
          str_to_lower(wfo_status) == "accepted" ~ wfo_rank,
          !is.na(accepted_wfo_rank) ~ accepted_wfo_rank,
          TRUE ~ wfo_rank
        )
      ) %>%
      select(match_id, wfo_check_id = wfo_id, wfo_check_name, wfo_check_authors, wfo_check_rank, crosswalk_ambiguous)
  }
}

# ---- Export for manual review ----

review <- uniq %>%
  left_join(wcvp_best, by = "match_id") %>%
  { if (!is.null(wfo_best))  left_join(., wfo_best,  by = "match_id") else . } %>%
  { if (!is.null(wfo_check)) left_join(., wfo_check, by = "match_id") else . } %>%
  mutate(
    ipni_check_status = case_when(
      is.na(wfo_final_name) | is.na(wfo_check_name) ~ "non_confrontabile",
      trimws(tolower(wfo_final_name)) == trimws(tolower(wfo_check_name)) ~ "concorde",
      TRUE ~ "discordante_da_verificare"
    ),
    proposed_name = case_when(
      !is.na(wcvp_final_name) ~ wcvp_final_name,
      is.na(wcvp_final_name) & !is.na(wfo_final_name) ~ wfo_final_name,
      TRUE ~ parsed_name
    ),
    proposed_author = case_when(
      !is.na(wcvp_final_name) ~ wcvp_final_authors,
      is.na(wcvp_final_name) & !is.na(wfo_final_name) ~ wfo_final_authors,
      TRUE ~ parsed_author
    ),
    proposed_source = case_when(
      !is.na(wcvp_final_name) ~ paste0("WCVP:", wcvp_final_basis),
      is.na(wcvp_final_name) & !is.na(wfo_final_name) ~ paste0("WFO:", wfo_final_basis),
      TRUE ~ "unresolved"
    ),
    final_name_manual   = proposed_name,
    final_author_manual = proposed_author,
    reviewer_notes       = NA_character_
  ) %>%
  select(match_id, parsed_name, parsed_author,
         wcvp_matched_id, wcvp_matched_ipni_id, wcvp_matched_name, wcvp_matched_authors, wcvp_matched_rank,
         wcvp_final_id, wcvp_final_ipni_id, wcvp_final_name, wcvp_final_authors, wcvp_final_rank,
         wcvp_final_basis, ipni_fallback_to_matched,
         wfo_matched_id, wfo_matched_name, wfo_matched_authors, wfo_matched_rank,
         wfo_final_id, wfo_final_name, wfo_final_authors, wfo_final_rank, wfo_final_basis,
         wfo_check_id, wfo_check_name, wfo_check_authors, wfo_check_rank,
         ipni_check_status, proposed_name, proposed_source,
         final_name_manual, final_author_manual, reviewer_notes)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "review_uniq")
openxlsx::writeDataTable(wb, "review_uniq", review)
openxlsx::saveWorkbook(wb, output_intermediate_xlsx, overwrite = TRUE)
message("Intermediate file written: ", output_intermediate_xlsx, " | rows: ", nrow(review))


library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(rWCVP)

manual_reviewed_xlsx <- "Input/processed/Piedmont_name_reconciliation_reviewed.xlsx"
output_final_xlsx <- "Piemonte_reconciled.xlsx"


# ---- Part B: reload the manually reviewed file ----
manual <- readxl::read_excel(manual_reviewed_xlsx, sheet = "review_uniq") %>%
  select(-any_of("match_id")) %>%
  distinct()

message("Unique rows in the reviewed file: ", nrow(manual))
n_blank_final <- sum(is.na(manual$final_name_manual) | manual$final_name_manual == "")
if (n_blank_final > 0) {
  message(n_blank_final, " rows have an empty final_name_manual (intentionally left without",
          " a proposal, e.g. aggregates); they will have no reconciled name in the final file.")
}


if (!exists("uniq")) stop("uniq not found in the session: run Part A (up to deduplication) first.")

key_uniq <- uniq %>% mutate(.k = paste(parsed_name, parsed_author, sep = "||"))
key_manual <- manual %>% mutate(.k = paste(parsed_name, parsed_author, sep = "||"))
new_names <- key_uniq %>% filter(!.k %in% key_manual$.k) %>% select(-.k)

if (nrow(new_names) > 0) {
  message(nrow(new_names), " unique names in the current run are not in the reviewed file",
          " (new or changed since the last run); exported separately for targeted review.")
  wb_new <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_new, "names_to_review")
  openxlsx::writeDataTable(wb_new, "names_to_review", new_names)
  openxlsx::saveWorkbook(wb_new, "Piedmont_new_names_to_review.xlsx", overwrite = TRUE)
}

# ---- Life form from WCVP ----
if (!exists("wcvp_names_tbl")) {
  stop("wcvp_names_tbl not found in the session: load it with load_wcvp_backbone(wcvp_backbone_csv) first.")
}

manual <- manual %>%
  mutate(
    .has_name = !is.na(final_name_manual) & final_name_manual != "",
    .unchanged = .has_name & !is.na(proposed_name) & trimws(final_name_manual) == trimws(proposed_name)
  )

message(sum(manual$.unchanged & !is.na(manual$wcvp_final_id)), " rows use the existing wcvp_final_id")
message(sum(manual$.has_name & !manual$.unchanged), " manually corrected rows were not re-matched and will have no life form")
message(sum(manual$.has_name & manual$.unchanged & is.na(manual$wcvp_final_id)), " unchanged rows without wcvp_final_id (WFO match only) will have no life form")

lifeform_lookup <- wcvp_names_tbl %>% select(wcvp_id, lifeform_description)

manual <- manual %>%
  left_join(lifeform_lookup, by = c("wcvp_final_id" = "wcvp_id")) %>%
  rename(lifeform_from_wcvp = lifeform_description)
manual$lifeform_from_wcvp[!manual$.unchanged] <- NA_character_

manual <- manual %>% select(-.has_name, -.unchanged)

# ---- Life form from Drevojan ----
drevojan_lf_cols <- c("Phanerophyte", "Tree", "Shrub", "Woody liana", "Chamaephyte",
                      "Semi-shrub", "Dwarf shrub", "Hemicryptophyte", "Geophyte",
                      "Hydrophyte", "Therophyte", "Epiphyte", "Herbaceous liana")

if (!exists("all_sheets")) stop("all_sheets not found in the session: needed to extract the Drevojan rows.")

species_key <- function(x) {
  trimws(str_remove(x, "\\s+(subsp\\.|nothosubsp\\.|var\\.|nothovar\\.|subvar\\.|forma|f\\.)\\s+.*$"))
}

build_lifeform_lookup <- function(df, key_col) {
  groups <- split(df, df[[key_col]])
  out <- lapply(names(groups), function(k) {
    sub <- groups[[k]]
    vals <- sapply(drevojan_lf_cols, function(cl) {
      v <- na.omit(sub[[cl]])
      if (length(v) == 0) NA_integer_ else max(v)
    })
    conflict <- any(sapply(drevojan_lf_cols, function(cl) {
      v <- na.omit(sub[[cl]])
      length(unique(v)) > 1
    }))
    c(list(key = k, lifeform_conflict = conflict), as.list(vals))
  })
  bind_rows(out)
}

drevojan_reconciled <- all_sheets %>%
  filter(source_file == "Life_form_Drevojan_2023.xlsx") %>%
  select(parsed_name, parsed_author, all_of(drevojan_lf_cols)) %>%
  mutate(across(all_of(drevojan_lf_cols), ~ suppressWarnings(as.integer(.x)))) %>%
  left_join(manual %>% select(parsed_name, parsed_author, final_name_manual),
            by = c("parsed_name", "parsed_author")) %>%
  filter(!is.na(final_name_manual)) %>%
  mutate(.species_key = species_key(final_name_manual))

lookup_exact <- build_lifeform_lookup(drevojan_reconciled, "final_name_manual") %>%
  rename(final_name_manual = key)
lookup_species <- build_lifeform_lookup(drevojan_reconciled, ".species_key") %>%
  rename(.species_key = key)

message(nrow(lookup_exact), " unique names in the exact Drevojan lookup (", sum(lookup_exact$lifeform_conflict),
        " with conflicting source rows; OR rule applied, see column lifeform_conflict)")

manual <- manual %>% mutate(.species_key = species_key(final_name_manual))

manual_exact <- manual %>%
  left_join(lookup_exact, by = "final_name_manual", suffix = c("", "_drevojan"))
manual_exact$.matched_exact <- rowSums(!is.na(manual_exact[drevojan_lf_cols])) > 0 | !is.na(manual_exact$lifeform_conflict)

needs_fallback <- manual_exact %>% filter(!.matched_exact | is.na(.matched_exact)) %>%
  select(parsed_name, parsed_author, .species_key)
fallback_matched <- needs_fallback %>%
  left_join(lookup_species, by = ".species_key") %>%
  select(parsed_name, parsed_author, all_of(drevojan_lf_cols), lifeform_conflict) %>%
  rename_with(~ paste0(.x, "_fb"), c(all_of(drevojan_lf_cols), "lifeform_conflict"))

manual <- manual_exact %>%
  left_join(fallback_matched, by = c("parsed_name", "parsed_author")) %>%
  mutate(
    lifeform_match_level = case_when(
      .matched_exact ~ "exact",
      rowSums(!is.na(across(all_of(paste0(drevojan_lf_cols, "_fb"))))) > 0 ~ "species_fallback",
      TRUE ~ "none"
    )
  )
for (cl in drevojan_lf_cols) {
  manual[[cl]] <- coalesce(manual[[cl]], manual[[paste0(cl, "_fb")]])
}
manual$lifeform_conflict <- coalesce(manual$lifeform_conflict, manual$lifeform_conflict_fb)
manual <- manual %>% select(-ends_with("_fb"), -.species_key, -.matched_exact)
names(manual)[names(manual) %in% drevojan_lf_cols] <- paste0("drevojan_", drevojan_lf_cols)

message("Drevojan life form coverage: ", sum(manual$lifeform_match_level == "exact", na.rm = TRUE), " exact + ",
        sum(manual$lifeform_match_level == "species_fallback", na.rm = TRUE), " species-level fallback / ",
        sum(!is.na(manual$final_name_manual)), " with a final name")

# ---- Attach to all original rows ----
if (!exists("all_sheets")) {
  stop("all_sheets not found in the session: run Part A (reading and composition) first.")
}

all_sheets_final <- all_sheets %>%
  left_join(manual, by = c("parsed_name", "parsed_author"))

n_missing <- all_sheets_final %>%
  filter(!is_placeholder, !is_not_species_level, is.na(final_name_manual)) %>% nrow()
message("Rows to match left without a reconciled name: ", n_missing,
        " (expected: the rows of the ", nrow(new_names), " new names not yet reviewed)")

# ---- Write final workbook ----
sheet_name_map <- tibble::tribble(
  ~source_file,                                     ~source_sheet,           ~output_sheet_name,
  "Flora_Bartolucci_native.xlsx",                    "Native",                "Bartolucci_native",
  "Flora_Galasso_esotiche.xlsx",                  "flora_esotiche",        "Galasso_esotiche",
  "Alberi_monumentali_Piemonte.xlsx",                "LAU_FUA_only",          "RAMI_alberi_monum",
  "UrbicaTorino1993Tables1e2.xlsx",                  "Tab.1.Censite",         "Urbica_Censite",
  "UrbicaTorino1993Tables1e2.xlsx",                  "Tab.2.Ornam",           "Urbica_Ornamentali",
  "Records_BBPAO.xlsx",                              "Entries",               "BBPAO",
  "occ_IT_Plantae_all_clean.csv",                    NA_character_,           "EASIN",
  "Alberi_coord_260313_filtered.csv",                NA_character_,           "Alberate_Torino",
  "alberi_Alessandria_260313_filtered.csv",           NA_character_,           "Alberate_Alessandria",
  "Life_form_Drevojan_2023.xlsx",                    "FloraVegEU-2023-01-03", "Drevojan_lifeform",
  "gbif_clean_occ_herb.csv",                         NA_character_,           "GBIF"
)

all_sheets_final <- all_sheets_final %>%
  left_join(sheet_name_map, by = c("source_file", "source_sheet"))

n_no_map <- sum(is.na(all_sheets_final$output_sheet_name))
if (n_no_map > 0) {
  warning(n_no_map, " rows have no match in sheet_name_map",
          " (unknown source_file/source_sheet); check sheet_config against sheet_name_map.")
}

wb <- openxlsx::createWorkbook()

dedupe_colnames_ci <- function(df) {
  nm <- names(df)
  seen <- character(0)
  for (i in seq_along(nm)) {
    key <- tolower(nm[i])
    if (key %in% seen) {
      k <- 2
      while (tolower(paste0(nm[i], "_dup", k)) %in% seen) k <- k + 1
      nm[i] <- paste0(nm[i], "_dup", k)
      key <- tolower(nm[i])
    }
    seen <- c(seen, key)
  }
  names(df) <- nm
  df
}

riconciliazione_cols <- setdiff(names(manual), c("parsed_name", "parsed_author"))
checklist_riassunto <- all_sheets_final %>%
  select(row_id, source_file, source_sheet, output_sheet_name,
         is_placeholder, is_not_species_level, original_raw,
         parsed_name, parsed_author, all_of(riconciliazione_cols))

openxlsx::addWorksheet(wb, "checklist_finale")
openxlsx::writeDataTable(wb, "checklist_finale", dedupe_colnames_ci(checklist_riassunto %>% select(-output_sheet_name)))

for (sn in unique(na.omit(all_sheets_final$output_sheet_name))) {
  d <- all_sheets_final %>% filter(output_sheet_name == sn) %>% select(-output_sheet_name)
  d <- Filter(function(x) !all(is.na(x)), d)
  d <- dedupe_colnames_ci(d)
  openxlsx::addWorksheet(wb, sn)
  openxlsx::writeDataTable(wb, sn, d)
  message("Sheet '", sn, "': ", nrow(d), " rows, ", ncol(d), " columns")
}

openxlsx::saveWorkbook(wb, output_final_xlsx, overwrite = TRUE)
message("Written: ", output_final_xlsx, " | total rows: ", nrow(all_sheets_final),
        " | sheets: 1 combined + ", length(unique(na.omit(all_sheets_final$output_sheet_name))), " individual")
