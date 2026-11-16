# =============================================================================
# 01_GBIF.R
# GBIF plant occurrence download and cleaning for the study areas
# Piedmont urban plant biodiversity
#
# Input:  GBIF credentials (GBIF_USER, GBIF_PWD, GBIF_EMAIL in ~/.Renviron)
# Output: output/GBIF/summaries/gbif_clean_occ_herb.csv and per-area species tables
#         output/GBIF/summaries/gbif_download_log_with_doi.csv (download DOIs)
#         output/GBIF/summaries/gbif_cleaning_counts.csv (records removed at each cleaning step)
# =============================================================================

# ---- 0) Packages ----
library(sf)
library(dplyr)
library(giscoR)
library(rgbif)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(ggplot2)
library(CoordinateCleaner)
library(lubridate)

# ---- 1) User settings ----

all_caps <- c(
  "Torino", "Alessandria", "Asti", "Biella",
  "Cuneo", "Novara", "Vercelli", "Verbania"
)

fua_caps <- c("Torino", "Novara", "Alessandria", "Asti")

gbif_user  <- Sys.getenv("GBIF_USER")
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

if (gbif_user == "" || gbif_pwd == "" || gbif_email == "") {
  stop("GBIF credentials not found. Add GBIF_USER, GBIF_PWD, GBIF_EMAIL to ~/.Renviron.")
}

save_citations <- FALSE

kingdom_key         <- 6
require_coordinates <- TRUE
exclude_geo_issues  <- TRUE

base_dir      <- file.path("output", "GBIF")
downloads_dir <- file.path(base_dir, "downloads_raw")
occ_dir       <- file.path(base_dir, "occurrences_by_area")
citations_dir <- file.path(base_dir, "citations_by_area")
summary_dir   <- file.path(base_dir, "summaries")

for (d in c(base_dir, downloads_dir, occ_dir, citations_dir, summary_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- 2) Helpers ----

safe_name <- function(x) {
  x |>
    gsub("[^A-Za-z0-9_\\-]+", "_", x = _) |>
    gsub("_+", "_", x = _) |>
    gsub("^_|_$", "", x = _)
}

wkt_for_gbif <- function(poly_sf, target = 3000, tol_start = 0.02, tol_max = 0.3) {
  tol <- tol_start
  repeat {
    poly_s <- st_simplify(poly_sf, dTolerance = tol, preserveTopology = TRUE)
    wkt <- st_as_text(st_geometry(poly_s))
    if (nchar(wkt) <= target || tol >= tol_max) return(wkt)
    tol <- tol * 1.5
  }
}

preferred_species_name <- function(df) {
  nm <- names(df)
  get_col <- function(x) {
    if (x %in% nm) as.character(df[[x]]) else rep(NA_character_, nrow(df))
  }
  out <- dplyr::coalesce(
    get_col("acceptedScientificName"),
    get_col("acceptedNameUsage"),
    get_col("species"),
    get_col("scientificName")
  )
  out[out == ""] <- NA_character_
  out
}

# ---- 3) Build study areas ----

message("Downloading FUA boundaries...")
options(timeout = 600)

fua_it <- gisco_get_urban_audit(
  year    = "2024",
  epsg    = "4326",
  level   = "FUA",
  country = "IT",
  cache   = TRUE,
  ext     = "gpkg"
)

fuas_sel <- fua_it %>%
  mutate(
    NAME = str_remove(URAU_NAME, "^FUA\\s+"),
    NAME = str_squish(NAME)
  ) %>%
  filter(NAME %in% fua_caps) %>%
  transmute(
    NAME,
    LEVEL    = "FUA",
    geometry = st_make_valid(geometry)
  )

message("Downloading LAU boundaries...")
lau_gpkg <- "https://gisco-services.ec.europa.eu/distribution/v2/lau/gpkg/LAU_RG_01M_2024_4326.gpkg"

lau_all <- st_read(lau_gpkg, layer = "LAU_RG_01M_2024_4326.gpkg", quiet = TRUE)

laus_sel <- lau_all %>%
  filter(CNTR_CODE == "IT", LAU_NAME %in% all_caps) %>%
  rename(geometry = geom) %>%
  transmute(NAME = LAU_NAME, geometry = st_make_valid(geometry))

caps_with_fua <- unique(fuas_sel$NAME)

fua_core <- purrr::map_dfr(caps_with_fua, function(city) {
  pf <- fuas_sel %>% filter(NAME == city)
  pl <- laus_sel %>% filter(NAME == city)
  tibble::tibble(NAME = city, geometry = sf::st_intersection(pf$geometry, pl$geometry))
}) %>%
  sf::st_as_sf(crs = 4326) %>%
  st_make_valid()

areas <- bind_rows(
  fuas_sel %>% mutate(LEVEL = "FUA"),
  fua_core %>% mutate(LEVEL = "CORE_CITY"),
  laus_sel %>% filter(!NAME %in% caps_with_fua) %>% mutate(LEVEL = "LAU_ONLY")
) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate(AREA_ID = paste(NAME, LEVEL, sep = "_")) %>%
  select(NAME, LEVEL, AREA_ID, geometry)

st_write(areas, file.path(base_dir, "areas.gpkg"),
         layer = "piemonte_areas", driver = "GPKG",
         delete_layer = TRUE, quiet = TRUE)
write_sf(areas, file.path(base_dir, "areas.geojson"), delete_dsn = TRUE, quiet = TRUE)

message("Areas created:")
print(areas %>% st_drop_geometry())

# ---- 4) GBIF download helpers ----

build_preds <- function(wkt, kingdom_key = 6,
                        require_coordinates = TRUE,
                        exclude_geo_issues  = TRUE) {
  preds <- list(pred("geometry", wkt))
  if (require_coordinates) preds <- append(preds, list(pred("hasCoordinate", TRUE)))
  if (exclude_geo_issues)  preds <- append(preds, list(pred("hasGeospatialIssue", FALSE)))
  if (!is.null(kingdom_key)) preds <- append(preds, list(pred("kingdomKey", kingdom_key)))
  preds
}

download_one_area <- function(one_area_sf,
                              kingdom_key = 6,
                              downloads_dir, occ_dir, citations_dir,
                              save_citations      = FALSE,
                              require_coordinates = TRUE,
                              exclude_geo_issues  = TRUE,
                              status_ping         = 15) {
  area_id <- one_area_sf$AREA_ID[1]
  area_nm <- one_area_sf$NAME[1]
  level   <- one_area_sf$LEVEL[1]
  wkt     <- wkt_for_gbif(one_area_sf)

  message(sprintf("Submitting GBIF download for %s [%s] ...", area_nm, level))

  preds <- build_preds(wkt, kingdom_key, require_coordinates, exclude_geo_issues)
  key   <- occ_download(do.call(pred_and, preds),
                        user  = gbif_user, pwd = gbif_pwd, email = gbif_email)
  key_chr <- as.character(key)

  message(sprintf("Waiting for %s ... key=%s", area_id, key_chr))
  meta <- occ_download_wait(key_chr, status_ping = status_ping, quiet = FALSE)

  message(sprintf("Downloading archive for %s ...", area_id))
  zip_path <- occ_download_get(key_chr, path = downloads_dir, overwrite = TRUE)

  message(sprintf("Importing occurrences for %s ...", area_id))
  dat <- occ_download_import(zip_path)

  out_csv <- file.path(occ_dir, paste0("occurrence_", safe_name(area_id), ".csv"))
  write_csv(dat, out_csv)

  if (isTRUE(save_citations)) {
    cit <- gbif_citation(key_chr)
    writeLines(as.character(cit),
               file.path(citations_dir, paste0("citation_", safe_name(area_id), ".txt")))
  }

  tibble(
    NAME              = area_nm,
    LEVEL             = level,
    AREA_ID           = area_id,
    gbif_download_key = key_chr,
    wkt_chars         = nchar(wkt),
    n_occurrences     = nrow(dat),
    archive_path      = as.character(zip_path),
    occurrence_csv    = out_csv,
    status            = if (!is.null(meta$status)) as.character(meta$status) else NA_character_
  )
}

# ---- 5) Download occurrences ----

areas_list <- areas %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  split(.$AREA_ID)

download_log <- purrr::map_dfr(
  areas_list,
  ~ download_one_area(
    one_area_sf         = .x,
    kingdom_key         = kingdom_key,
    downloads_dir       = downloads_dir,
    occ_dir             = occ_dir,
    citations_dir       = citations_dir,
    save_citations      = save_citations,
    require_coordinates = require_coordinates,
    exclude_geo_issues  = exclude_geo_issues,
    status_ping         = 15
  )
)

write_csv(download_log, file.path(summary_dir, "gbif_download_log.csv"))
message("All downloads completed.")
print(download_log)

# ---- 6a) Retrieve DOIs and save log ----

as_download_tbl <- function(dl) {
  if (is.data.frame(dl)) return(dl)
  if (!is.null(dl$results))   return(dplyr::bind_rows(dl$results))
  if (!is.null(dl$downloads)) return(dplyr::bind_rows(dl$downloads))
  tibble::tibble()
}

dl_tbl <- as_download_tbl(occ_download_list(user = gbif_user, limit = 200))

if (!all(c("key", "doi") %in% names(dl_tbl))) {
  stop("occ_download_list() did not return the columns 'key' and 'doi'; check names(dl_tbl).")
}

# Join on the download key, not on the number of records
final_download_log_with_doi <- download_log %>%
  left_join(dl_tbl, by = c("gbif_download_key" = "key")) %>%
  select(-c(wkt_chars, archive_path))

if (any(is.na(final_download_log_with_doi$doi))) {
  warning("DOI missing for: ",
          paste(final_download_log_with_doi$AREA_ID[is.na(final_download_log_with_doi$doi)], collapse = ", "))
}
if ("totalRecords" %in% names(final_download_log_with_doi) &&
    any(final_download_log_with_doi$n_occurrences != final_download_log_with_doi$totalRecords, na.rm = TRUE)) {
  warning("n_occurrences differs from GBIF totalRecords for at least one download.")
}

write_csv(final_download_log_with_doi,
          file.path(summary_dir, "gbif_download_log_with_doi.csv"))

# ---- 6b) Load and combine raw occurrences ----

occ_files <- list.files(occ_dir, pattern = "^occurrence_.*\\.csv$", full.names = TRUE)
if (length(occ_files) == 0) stop("No occurrence CSVs found in ", occ_dir)

read_occurrence_with_meta <- function(f) {
  df <- suppressMessages(read_csv(f, col_types = cols(.default = col_character()),
                                  show_col_types = FALSE))
  area_id <- str_remove(str_remove(basename(f), "^occurrence_"), "\\.csv$")
  area_id <- gsub("_+", "_", area_id)
  df %>% mutate(FILE_AREA_ID = area_id, .before = 1)
}

all_occ <- bind_rows(lapply(occ_files, read_occurrence_with_meta))

area_lookup <- areas %>%
  st_drop_geometry() %>%
  mutate(FILE_AREA_ID = safe_name(AREA_ID)) %>%
  select(FILE_AREA_ID, NAME, LEVEL, AREA_ID)

all_occ <- all_occ %>%
  left_join(area_lookup, by = "FILE_AREA_ID") %>%
  mutate(species_name = preferred_species_name(.))

write_csv(all_occ, file.path(summary_dir, "gbif_all_occ.csv"))

# ---- 6c) Spatial QC plots ----

occ_pts <- all_occ %>%
  mutate(
    decimalLatitude  = suppressWarnings(as.numeric(decimalLatitude)),
    decimalLongitude = suppressWarnings(as.numeric(decimalLongitude))
  ) %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude),
         between(decimalLatitude,  -90,  90),
         between(decimalLongitude, -180, 180)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)

ggplot() +
  geom_sf(data = areas,   fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = occ_pts, color = "red", alpha = 0.4, size = 0.4) +
  theme_minimal() +
  labs(title = "Raw GBIF plant occurrences over Piedmont study areas")

# ---- 6d) Quality filtering ----

# Numeric columns were read as character (6b): convert before any numeric comparison,
# otherwise filters such as `coordinateUncertaintyInMeters < 1000` compare strings
num_cols <- intersect(c("decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
                        "coordinatePrecision", "year"), names(all_occ))
message("Column classes before conversion: ",
        paste(num_cols, vapply(all_occ[num_cols], function(x) class(x)[1], character(1)),
              sep = "=", collapse = ", "))

df <- all_occ %>%
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude))

df1 <- df %>%
  as_tibble() %>%
  filter(!basisOfRecord %in% c("FOSSIL_SPECIMEN", "MATERIAL_SAMPLE", "LITERATURE"))

message(nrow(df) - nrow(df1), " records removed (basisOfRecord filter); ",
        nrow(df1), " remaining.")

df2 <- df1 %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
  cc_val() %>%
  cc_zero() %>%
  cc_equ() %>%
  cc_sea() %>%
  cc_cap(buffer = 2000) %>%
  cc_cen(buffer = 2000) %>%
  distinct(AREA_ID, gbifID, .keep_all = TRUE)

message(nrow(df1) - nrow(df2), " records removed (coordinate cleaning); ",
        nrow(df2), " remaining.")

df3 <- df2 %>%
  filter(is.na(coordinateUncertaintyInMeters) |
           coordinateUncertaintyInMeters < 1000,
         is.na(coordinatePrecision) |
           coordinatePrecision < 0.001)

message(nrow(df2) - nrow(df3), " records removed (uncertainty filter); ",
        nrow(df3), " remaining.")

df4 <- df3 %>% filter(year >= 1900)

message(nrow(df3) - nrow(df4), " records removed (year < 1900); ",
        nrow(df4), " remaining.")

occ_clean <- df4 %>%
  filter(
    !is.na(scientificName), scientificName != "",
    taxonRank %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")
  )

message(nrow(df4) - nrow(occ_clean), " records removed (rank filter); ",
        nrow(occ_clean), " remaining.")

write_csv(occ_clean, file.path(summary_dir, "gbif_clean_occ_herb.csv"))

# Record counts at each cleaning step (read by 07_analyses.R for the Sankey diagram)
cleaning_counts <- tibble(
  step = c("downloaded", "removed_basis_of_record", "removed_coordinatecleaner",
           "removed_uncertainty_precision", "removed_year", "removed_rank", "final"),
  n = c(nrow(df), nrow(df) - nrow(df1), nrow(df1) - nrow(df2), nrow(df2) - nrow(df3),
        nrow(df3) - nrow(df4), nrow(df4) - nrow(occ_clean), nrow(occ_clean))
)
write_csv(cleaning_counts, file.path(summary_dir, "gbif_cleaning_counts.csv"))

# ---- 6e) Species lists ----

tax_cols <- c(
  "species_final", "species_name", "acceptedScientificName", "acceptedNameUsage",
  "species", "scientificName", "taxonRank", "taxonomicStatus",
  "kingdom", "phylum", "class", "order", "family", "genus",
  "specificEpithet", "infraspecificEpithet",
  "taxonKey", "acceptedTaxonKey", "speciesKey", "genusKey", "familyKey",
  "scientificNameID", "acceptedNameUsageID", "taxonID",
  "datasetKey", "datasetName", "license"
)

species_by_area <- occ_clean %>%
  filter(!is.na(AREA_ID), !is.na(species_name)) %>%
  arrange(AREA_ID, species_name,
          desc(!is.na(speciesKey)), desc(!is.na(acceptedScientificName))) %>%
  distinct(AREA_ID, species_name, .keep_all = TRUE) %>%
  select(any_of(c("NAME", "LEVEL", "AREA_ID", tax_cols))) %>%
  arrange(NAME, LEVEL, species_name)

write_csv(species_by_area,
          file.path(summary_dir, "integrated_species_by_area_herb.csv"))

species_counts_by_area <- occ_clean %>%
  filter(!is.na(AREA_ID), !is.na(species_name)) %>%
  count(NAME, LEVEL, AREA_ID, species_name, name = "n_occurrences") %>%
  arrange(NAME, LEVEL, desc(n_occurrences), species_name)

write_csv(species_counts_by_area,
          file.path(summary_dir, "integrated_species_counts_by_area_herb.csv"))

species_presence_wide <- species_by_area %>%
  mutate(present = 1L) %>%
  select(AREA_ID, species_name, present) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = AREA_ID, values_from = present, values_fill = 0) %>%
  arrange(species_name)

write_csv(species_presence_wide,
          file.path(summary_dir, "integrated_species_presence_matrix_herb.csv"))

area_summary <- species_counts_by_area %>%
  group_by(NAME, LEVEL, AREA_ID) %>%
  summarise(
    n_species     = n_distinct(species_name),
    n_occurrences = sum(n_occurrences),
    .groups = "drop"
  ) %>%
  arrange(NAME, LEVEL)

write_csv(area_summary, file.path(summary_dir, "area_species_summary_herb.csv"))

message("All GBIF outputs written to: ", summary_dir)
print(area_summary)
