# =============================================================================
# 07_analyses.R
# Analysis of urban plant biodiversity across Piedmont FUAs and LAUs
#
# Run AFTER: 06_checklist_build.R
#
# Input:  output/checklists/checklists_by_area.xlsx
#         (checklist workbook with one sheet per area; no taxa are excluded)
# =============================================================================

# ============================================================================
# COMPLETE ANALYSIS OF URBAN PLANT BIODIVERSITY IN PIEDMONT
# ============================================================================
#
# Script organized in three sections:
# 1) TAXONOMIC STATISTICS (families, genera, life forms)
# 2) PIEDMONT FLORA IN URBAN ENVIRONMENTS (native vs exotic)
# 3) DATA COMPLETENESS (by area, with normalized density)
# plus figures and the GBIF records Sankey diagram
#
# ============================================================================

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(sf)
library(giscoR)
library(scales)
library(plotly)
library(ggrepel)

# ============================================================================
# 1. LOAD DATA FROM CHECKLISTS
# ============================================================================

cat("DATA LOADING\n")

file_path <- "output/checklists/checklists_by_area.xlsx"

# Sheets that are not area checklists
non_area_sheets <- c("Description", "all_areas", "PRESENCE_ABSENCE_MATRIX")

sheet_names <- excel_sheets(file_path)
area_sheets <- sheet_names[!sheet_names %in% non_area_sheets]

cat("Loading data from", length(area_sheets), "urban areas...\n\n")

# Load all data
processed_sheets <- list()
for (sheet in area_sheets) {
  processed_sheets[[sheet]] <- read_excel(file_path, sheet = sheet,
                                          guess_max = 10000)
}

# Combine all data
all_data <- bind_rows(processed_sheets, .id = "area")

# Identify FUA vs LAU
all_data <- all_data %>%
  mutate(
    area_type = ifelse(grepl("_FUA$", area), "FUA", "LAU"),
    area_clean = str_replace(area, "_FUA|_LAU", "")
  )

# Column names used in the analyses
# Records = BBPAO + RAMI + GBIF records; street-tree inventories and EASIN
# records are included in n_records_total but are not occurrence records
all_data <- all_data %>%
  mutate(
    final_accepted_name = wcvp_final_name,
    family              = taxo_family,
    genus               = taxo_genus,
    rank                = wcvp_final_rank,
    lifeform            = lifeform_drevojan_summary,
    n_records_all       = rowSums(across(c(n_records_bbpao, n_records_rami,
                                           n_records_gbif)), na.rm = TRUE),
    # Native in Piedmont: any native category of origin_piedmont
    native_pie          = grepl("^native", origin_piedmont),
    flora_type = case_when(
      native_pie                              ~ "Native Piedmont",
      origin_italy == "native" & !native_pie  ~ "Native Italy",
      origin_italy == "alien"                 ~ "Exotic",
      TRUE                                    ~ "Insufficient data"
    )
  )

stopifnot("final_accepted_name has missing values" = !anyNA(all_data$final_accepted_name))

cat("✓ Data loaded\n")
cat("  -", n_distinct(all_data$area), "urban areas\n")
cat("  -", n_distinct(all_data$final_accepted_name), "unique species\n")
cat("  -", nrow(all_data), "total records\n")
cat("  -", sum(is.na(all_data$family)), "rows without family\n\n")

# ============================================================================
# 2. DOWNLOAD EUROSTAT POLYGONS AND CALCULATE AREAS
# ============================================================================

cat("Downloading Eurostat polygons\n")

# FUA boundaries
cat("Downloading FUA boundaries...\n")
fua_it <- gisco_get_urban_audit(
  year    = "2024",  # same release as 01_GBIF.R and the manuscript
  epsg    = "4326",
  level   = "FUA",
  country = "IT"
)

# Extract provincial capitals with FUA
fua_caps <- c("Torino", "Novara", "Alessandria", "Asti")

fuas_sel <- fua_it %>%
  mutate(
    NAME = str_remove(URAU_NAME, "^FUA\\s+"),
    NAME = str_squish(NAME)
  ) %>%
  filter(NAME %in% fua_caps) %>%
  transmute(
    NAME,
    LEVEL = "FUA",
    geometry = st_make_valid(geometry)
  )

# LAU boundaries
cat("Downloading LAU boundaries...\n")
lau_gpkg <- "https://gisco-services.ec.europa.eu/distribution/v2/lau/gpkg/LAU_RG_01M_2024_4326.gpkg"

lau_all <- st_read(
  lau_gpkg,
  layer = "LAU_RG_01M_2024_4326.gpkg",
  quiet = TRUE
)

# All provincial capitals
all_caps <- c(
  "Torino", "Alessandria", "Asti", "Biella",
  "Cuneo", "Novara", "Vercelli", "Verbania"
)

laus_sel <- lau_all %>%
  filter(CNTR_CODE == "IT", LAU_NAME %in% all_caps) %>%
  rename(geometry = geom) %>%
  transmute(
    NAME = LAU_NAME,
    geometry = st_make_valid(geometry)
  )

# Combine FUA and LAU
areas_sf <- bind_rows(
  fuas_sel %>% mutate(LEVEL = "FUA"),
  laus_sel %>% mutate(LEVEL = "LAU")  # Keep ALL LAU, not filtering out FUA cities
) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate(
    AREA_ID = paste(NAME, LEVEL, sep = "_"),
    area_km2 = as.numeric(st_area(geometry)) / 1e6
  ) %>%
  select(NAME, LEVEL, AREA_ID, area_km2, geometry)

# Map and join
area_mapping <- data.frame(
  area = area_sheets,
  area_clean = str_replace(area_sheets, "_FUA|_LAU", ""),
  area_type = ifelse(grepl("_FUA$", area_sheets), "FUA", "LAU")
)

area_info <- areas_sf %>%
  st_drop_geometry() %>%
  rename(area_clean = NAME) %>%
  left_join(area_mapping, by = c("area_clean", "LEVEL" = "area_type"))

all_data <- all_data %>%
  left_join(
    area_info %>% select(area, AREA_ID, area_km2),
    by = "area"
  )

cat("✓ Polygons loaded and areas calculated\n\n")

# ============================================================================
# SECTION 1: TAXONOMIC STATISTICS
# ============================================================================

cat("SECTION 1: TAXONOMIC STATISTICS\n")

# 1.1 Top 20 families
cat("Top 20 families by number of species:\n\n")
family_stats <- all_data %>%
  filter(!is.na(family)) %>%
  group_by(family) %>%
  summarise(
    n_species = n_distinct(final_accepted_name),
    n_records = n(),
    n_areas = n_distinct(area),
    pct_species = round(n_distinct(final_accepted_name) / n_distinct(all_data$final_accepted_name) * 100, 1)
  ) %>%
  arrange(desc(n_species)) %>%
  head(20)

print(family_stats)

# 1.2 Top 20 genera
cat("\n\nTop 20 genera by number of species:\n\n")
genus_stats <- all_data %>%
  filter(!is.na(genus)) %>%
  group_by(genus) %>%
  summarise(
    n_species = n_distinct(final_accepted_name),
    n_records = n(),
    family = first(family)
  ) %>%
  arrange(desc(n_species)) %>%
  head(20)

print(genus_stats)

# 1.3 Composition by life form (Raunkiaer classes, Drevojan et al. 2023)
# A taxon can belong to more than one class, so percentages do not sum to 100
cat("\n\nComposition by life form:\n\n")
raunkiaer_classes <- c("Phanerophyte", "Chamaephyte", "Hemicryptophyte",
                       "Geophyte", "Therophyte", "Hydrophyte", "Epiphyte")

taxa_lifeform <- all_data %>%
  distinct(final_accepted_name, lifeform) %>%
  filter(!is.na(lifeform))

n_taxa_lifeform <- nrow(taxa_lifeform)

lifeform_composition <- taxa_lifeform %>%
  separate_rows(lifeform, sep = ";\\s*") %>%
  filter(lifeform %in% raunkiaer_classes) %>%
  group_by(lifeform) %>%
  summarise(n_species = n_distinct(final_accepted_name), .groups = "drop") %>%
  mutate(pct_species = round(n_species / n_taxa_lifeform * 100, 1)) %>%
  arrange(desc(n_species))

cat("Taxa with life form:", n_taxa_lifeform, "of",
    n_distinct(all_data$final_accepted_name), "\n")
print(lifeform_composition)

# 1.4 Overall taxonomic summary
cat("\n\nOverall taxonomic summary:\n\n")
taxonomic_summary <- data.frame(
  category = c("Families", "Genera", "Taxa"),
  number = c(
    n_distinct(all_data$family, na.rm = TRUE),
    n_distinct(all_data$genus, na.rm = TRUE),
    n_distinct(all_data$final_accepted_name)
  )
)

print(taxonomic_summary)

# 1.5 Summary by rank
cat("\n\nSummary by taxonomic rank:\n\n")
rank_summary <- all_data %>%
  group_by(rank) %>%
  summarise(
    n_taxa        = n_distinct(final_accepted_name),
    n_records     = n(),
    pct_taxa      = round(n_distinct(final_accepted_name) / n_distinct(all_data$final_accepted_name) * 100, 1)
  ) %>%
  arrange(desc(n_taxa))

print(rank_summary)

# ============================================================================
# SECTION 2: PIEDMONT FLORA IN URBAN ENVIRONMENTS
# ============================================================================

cat("SECTION 2: PIEDMONT FLORA IN URBAN ENVIRONMENTS\n")
cat("NATIVE vs EXOTIC\n")

# 2.1 Classification native vs exotic at global level
cat("Global composition: Native vs Exotic\n\n")

flora_composition_global <- all_data %>%
  group_by(flora_type) %>%
  summarise(
    n_species = n_distinct(final_accepted_name),
    n_records = n(),
    pct_species = round(n_distinct(final_accepted_name) / n_distinct(all_data$final_accepted_name) * 100, 1),
    pct_records = round(n() / nrow(all_data) * 100, 1)
  ) %>%
  arrange(desc(n_species))

print(flora_composition_global)

# 2.2 Top families native vs exotic
cat("\n\nTop 10 native families in Piedmont:\n\n")
native_families <- all_data %>%
  filter(native_pie) %>%
  group_by(family) %>%
  summarise(
    n_species = n_distinct(final_accepted_name),
    n_records = n()
  ) %>%
  arrange(desc(n_species)) %>%
  head(10)

print(native_families)

cat("\n\nTop 10 exotic families:\n\n")
exotic_families <- all_data %>%
  filter(origin_italy == "alien") %>%
  group_by(family) %>%
  summarise(
    n_species = n_distinct(final_accepted_name),
    n_records = n()
  ) %>%
  arrange(desc(n_species)) %>%
  head(10)

print(exotic_families)

# 2.3 Status and distribution
cat("\n\nStatus of exotic species (INV/NAT/CAS):\n\n")
n_alien_taxa <- all_data %>%
  filter(origin_italy == "alien") %>%
  pull(final_accepted_name) %>%
  n_distinct()

alien_status <- all_data %>%
  filter(origin_italy == "alien") %>%
  group_by(status_italy) %>%
  summarise(
    n_species = n_distinct(final_accepted_name),
    n_records = n(),
    pct = round(n_distinct(final_accepted_name) / n_alien_taxa * 100, 1)
  )

print(alien_status)

# 2.4 Species richness by area (native vs exotic)
cat("\n\nSpecies richness by area:\n\n")
diversity_by_area <- all_data %>%
  group_by(area, area_type, area_km2) %>%
  summarise(
    n_species_total = n_distinct(final_accepted_name),
    n_species_native = n_distinct(final_accepted_name[flora_type == "Native Piedmont"]),
    n_species_exotic = n_distinct(final_accepted_name[flora_type == "Exotic"]),
    n_species_uncertain = n_distinct(final_accepted_name[flora_type == "Insufficient data"]),
    n_records = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_native = round(n_species_native / n_species_total * 100, 1),
    pct_exotic = round(n_species_exotic / n_species_total * 100, 1),
    species_per_km2 = round(n_species_total / area_km2, 3),
    native_per_km2 = round(n_species_native / area_km2, 3),
    exotic_per_km2 = round(n_species_exotic / area_km2, 3)
  ) %>%
  arrange(desc(n_species_total))

cat("Details by area:\n")
print(diversity_by_area %>%
        select(area, area_type, n_species_total, n_species_native, n_species_exotic,
               pct_native, pct_exotic, species_per_km2))

# 2.5 FUA vs LAU comparison (native vs exotic)
cat("\n\nFUA vs LAU comparison (Native vs exotic flora):\n\n")
fua_lau_flora_comparison <- all_data %>%
  group_by(area_type) %>%
  summarise(
    n_areas = n_distinct(area),
    n_species_native = n_distinct(final_accepted_name[flora_type == "Native Piedmont"]),
    n_species_exotic = n_distinct(final_accepted_name[flora_type == "Exotic"]),
    pct_native = round(n_distinct(final_accepted_name[flora_type == "Native Piedmont"]) /
                       n_distinct(final_accepted_name) * 100, 1),
    pct_exotic = round(n_distinct(final_accepted_name[flora_type == "Exotic"]) /
                       n_distinct(final_accepted_name) * 100, 1),
    .groups = "drop"
  )

print(fua_lau_flora_comparison)

# ============================================================================
# SECTION 3: DATA COMPLETENESS
# ============================================================================

cat("SECTION 3: DATA COMPLETENESS\n")

# 3.1 Completeness by area (normalized metrics)
cat("Sampling completeness by area:\n\n")
completeness_by_area <- all_data %>%
  group_by(area, area_type) %>%
  summarise(
    area_km2 = first(area_km2),
    n_species_observed = n_distinct(final_accepted_name),
    n_records = sum(n_records_all, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    records_per_species = round(n_records / n_species_observed, 2),
    records_per_km2 = round(n_records / area_km2, 2),
    species_per_km2 = round(n_species_observed / area_km2, 3)
  ) %>%
  arrange(desc(n_species_observed))

print(head(completeness_by_area, 15) %>%
        select(area, area_type, area_km2, n_species_observed, records_per_species,
               records_per_km2, species_per_km2))

# 3.2 Temporal coverage
cat("\n\nTemporal coverage of data:\n\n")
temporal_coverage <- data.frame(
  year_min = min(all_data$first_record_year, na.rm = TRUE),
  year_max = max(all_data$last_record_year, na.rm = TRUE),
  n_rows_with_dates = sum(!is.na(all_data$first_record_year)),
  n_rows_without_dates = sum(is.na(all_data$first_record_year))
)
print(temporal_coverage)

# 3.3 FUA vs LAU comparison (completeness)
cat("\n\nFUA vs LAU comparison (Completeness):\n\n")
fua_lau_completeness <- all_data %>%
  group_by(area_type) %>%
  summarise(
    n_areas = n_distinct(area),
    area_km2_mean = round(mean(area_km2, na.rm = TRUE), 1),
    n_species_mean = round(n_distinct(final_accepted_name) / n_distinct(area), 1),
    species_density_mean = round(n_distinct(final_accepted_name) / mean(area_km2, na.rm = TRUE), 3),
    n_records = sum(n_records_all, na.rm = TRUE),
    records_density_mean = round(sum(n_records_all, na.rm = TRUE) / mean(area_km2, na.rm = TRUE), 2),
    records_per_species = round(sum(n_records_all, na.rm = TRUE) / n_distinct(final_accepted_name), 2),
    .groups = "drop"
  )

print(fua_lau_completeness)

# ============================================================================
# FIGURE CREATION
# ============================================================================

cat("FIGURE CREATION\n")

if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

# Figure 3: Top 20 families
g1 <- family_stats %>%
  ggplot(aes(x = reorder(family, n_species), y = n_species)) +
  geom_col(fill = "#3498db") +
  geom_text(aes(label = n_species), hjust = -0.2, size = 5, color = "grey30") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(title = "Top 20 families by number of taxa",
       x = "Family",
       y = "Number of taxa") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 13),
        axis.title = element_text(size = 15),
        panel.grid.major.y = element_blank())

ggsave("output/figures/03_top_families.png", g1, width = 10, height = 7, dpi = 300)

# Figure 4: Life forms (Raunkiaer classes, Drevojan et al. 2023)
g2 <- lifeform_composition %>%
  mutate(lifeform = factor(lifeform, levels = lifeform[order(n_species)])) %>%
  ggplot(aes(x = lifeform, y = n_species, fill = lifeform)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(n_species, " (", pct_species, "%)")),
            hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of taxa",
    title = "Life-form composition (Raunkiaer classes)",
    subtitle = paste0("Percentages refer to the ", n_taxa_lifeform,
                      " taxa with a life form; a taxon can belong to more than one class")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave("output/figures/04_lifeform.png", g2, width = 10, height = 8, dpi = 300)

# Figure 5: Scatter records/km² vs taxa/km² (completeness assessment)
g3 <- completeness_by_area %>%
  ggplot(aes(x = records_per_km2, y = species_per_km2, color = area_type)) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, color = "gray50") +
  geom_point(aes(size = n_species_observed), alpha = 0.6) +
  geom_text_repel(
    aes(label = area),
    size = 5, color = "grey20",
    box.padding = 0.6, point.padding = 0.8,
    min.segment.length = 0, segment.color = "grey50",
    max.overlaps = Inf, seed = 1
  ) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Completeness assessment: sampling intensity vs density",
       x = "Records per km²", y = "Taxa per km²",
       color = "Area type", size = "Taxa") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")

ggsave("output/figures/05_completeness_assessment.png", g3, width = 11, height = 6, dpi = 300)

# ============================================================================
# GBIF RECORDS SANKEY DIAGRAM (Figure 2)
# ============================================================================

# Record counts per cleaning step: written by 01_GBIF.R; the values below are the
# ones used in the paper and serve as fallback if the file is not available
gbif_counts <- c(downloaded = 80278, removed_basis_of_record = 218, removed_coordinatecleaner = 2110,
                 removed_uncertainty_precision = 42357, removed_year = 536, removed_rank = 75)
counts_csv <- "output/GBIF/summaries/gbif_cleaning_counts.csv"
if (file.exists(counts_csv)) {
  cc <- read.csv(counts_csv, stringsAsFactors = FALSE)
  gbif_counts <- setNames(cc$n, cc$step)[names(gbif_counts)]
}

fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
n0 <- gbif_counts[["downloaded"]]
r1 <- n0 - gbif_counts[["removed_basis_of_record"]]
r2 <- r1 - gbif_counts[["removed_coordinatecleaner"]]
r3 <- r2 - gbif_counts[["removed_uncertainty_precision"]]
r4 <- r3 - gbif_counts[["removed_year"]]
r5 <- r4 - gbif_counts[["removed_rank"]]

# Nodes
node_labels <- c(
  paste0("GBIF download:<br>", fmt(n0), " records"),                       # 0
  paste0("Retained:<br>", fmt(r1)),                                        # 1
  paste0("Retained:<br>", fmt(r2)),                                        # 2
  paste0("Retained:<br>", fmt(r3)),                                        # 3
  paste0("Retained:<br>", fmt(r4)),                                        # 4
  paste0("Final dataset:<br>", fmt(r5)),                                   # 5
  paste0("Removed:<br>fossils/<br>literature (", fmt(gbif_counts[["removed_basis_of_record"]]), ")"),          # 6
  paste0("Removed:<br>CoordinateCleaner<br>(", fmt(gbif_counts[["removed_coordinatecleaner"]]), ")"),          # 7
  paste0("Removed:<br>coordinate<br>uncertainty<br>(", fmt(gbif_counts[["removed_uncertainty_precision"]]), ")"), # 8
  paste0("Removed:<br>temporal filter<br>(", fmt(gbif_counts[["removed_year"]]), ")"),                         # 9
  paste0("Removed:<br>taxonomic rank<br>(", fmt(gbif_counts[["removed_rank"]]), ")")                           # 10
)

node_x <- c(0.001, 0.20, 0.40, 0.60, 0.78, 0.999,
            0.20, 0.40, 0.60, 0.78, 0.999)
node_y <- c(0.45, 0.45, 0.45, 0.20, 0.20, 0.20,
            0.995, 0.92, 0.80, 0.86, 0.97)

node_colours <- c(
  rep("#4C72B0", 6),
  "#DD8452",
  "#C44E52",
  "#8172B2",
  "#937860",
  "#64B5CD"
)

links <- data.frame(
  source = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4),
  target = c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10),
  value  = c(r1, gbif_counts[["removed_basis_of_record"]],
             r2, gbif_counts[["removed_coordinatecleaner"]],
             r3, gbif_counts[["removed_uncertainty_precision"]],
             r4, gbif_counts[["removed_year"]],
             r5, gbif_counts[["removed_rank"]])
)

link_colours <- c(
  "rgba(76,114,176,0.5)",   # -> retained
  "rgba(221,132,82,0.5)",   # -> fossils/literature
  "rgba(76,114,176,0.5)",
  "rgba(196,78,82,0.5)",    # -> CoordinateCleaner
  "rgba(76,114,176,0.5)",
  "rgba(129,114,178,0.5)",  # -> coordinate uncertainty
  "rgba(76,114,176,0.5)",
  "rgba(147,120,96,0.5)",   # -> temporal filter
  "rgba(76,114,176,0.5)",
  "rgba(100,181,205,0.5)"   # -> taxonomic rank
)

sankey <- plot_ly(
  type = "sankey",
  arrangement = "fixed",
  node = list(
    label = node_labels,
    x = node_x,
    y = node_y,
    color = node_colours,
    pad = 20,
    thickness = 20,
    line = list(color = "white", width = 0.5)
  ),
  link = list(
    source = links$source,
    target = links$target,
    value  = links$value,
    color  = link_colours
  )
) %>%
  layout(font = list(size = 27), height = 900, width = 1800)

# Export: interactive HTML always; static PNG only if the kaleido backend is available
htmlwidgets::saveWidget(sankey, file.path(getwd(), "output/figures/02_gbif_sankey.html"),
                        selfcontained = TRUE)
tryCatch(
  plotly::save_image(sankey, "output/figures/02_gbif_sankey.png", width = 1800, height = 900, scale = 2),
  error = function(e) message("Static export skipped (needs the kaleido backend): ", conditionMessage(e))
)

sankey
