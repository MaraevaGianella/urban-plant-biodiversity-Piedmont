# =============================================================================
# 05_cultivation_flagging.R
# Global cultivation status per taxon from weighted evidence (wild vs cultivated)
# Piedmont urban plant biodiversity
#
# Input:  output/origin_status/origin_status_full.csv
#         output/GBIF/summaries/flagging_by_accepted_name.csv
#         Piemonte_reconciled.xlsx (endemism, sheet Bartolucci_native)
# Output: output/cultivation/cultivation_status_by_id.csv
#         output/cultivation/cultivation_conflicting_evidence_review.csv
# =============================================================================

library(dplyr)
library(stringr)
library(readr)
library(readxl)

origin_status_csv <- "output/origin_status/origin_status_full.csv"
gbif_flagging_csv <- "output/GBIF/summaries/flagging_by_accepted_name.csv"
checklist_xlsx <- "Piemonte_reconciled.xlsx"
output_dir <- "output/cultivation"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load sources ----

origin_status <- read_csv(origin_status_csv, show_col_types = FALSE) %>%
  mutate(wcvp_final_id = as.character(wcvp_final_id))
gbif_flags <- read_csv(gbif_flagging_csv, show_col_types = FALSE) %>%
  rename(wcvp_final_id = wcvp_accepted_id) %>%
  mutate(wcvp_final_id = as.character(wcvp_final_id))

endemism_lookup <- tryCatch(
  read_excel(checklist_xlsx, sheet = "Bartolucci_native", col_types = "text") %>%
    filter(!is.na(wcvp_final_id)) %>%
    mutate(wcvp_final_id = as.character(wcvp_final_id)) %>%
    distinct(wcvp_final_id, .keep_all = TRUE) %>%
    transmute(wcvp_final_id, endemic_italy = !is.na(E) & E != ""),
  error = function(e) { message("Column 'E' not found in Bartolucci_native: ", conditionMessage(e)); tibble(wcvp_final_id = character(0)) }
)

message("origin_status: ", nrow(origin_status), " taxa | gbif_flags: ", nrow(gbif_flags), " taxa | ",
        "endemism_lookup: ", nrow(endemism_lookup), " taxa")

# ---- Merge by wcvp_final_id ----

combined <- full_join(origin_status, gbif_flags, by = "wcvp_final_id") %>%
  left_join(endemism_lookup, by = "wcvp_final_id")

message(nrow(combined), " taxa after the merge (", sum(is.na(combined$origin_piedmont)),
        " without origin_piedmont, ", sum(is.na(combined$n_occ_total)), " without GBIF evidence)")

# ---- Evidence weights ----

W_NATIVE_PIEDMONT      <- 3
W_NATIVE_HISTORICAL    <- 1
W_NATIVE_DOUBTFUL      <- 1
W_NATURALIZED_PIEDMONT <- 2
W_NATURALIZED_ITALIA   <- 1
W_HERBARIUM_VOUCHER    <- 1
W_ENDEMIC_ITALY        <- 1

W_CULTIVATED_FLAG      <- 3
W_ALSO_PLANTED         <- 2
W_FERAL                <- 1
W_WOODY_CASUAL         <- 1

MARGIN <- 2

# ---- Wild and cultivated evidence ----

is_woody <- function(lifeform_drevojan) {
  !is.na(lifeform_drevojan) & str_detect(lifeform_drevojan, "Tree|Shrub|Woody liana")
}

flagged <- combined %>%
  mutate(
    evidence_wild =
      W_NATIVE_PIEDMONT      * (coalesce(origin_piedmont, "") == "native") +
      W_NATIVE_HISTORICAL    * (coalesce(origin_piedmont, "") == "native_no_longer_recorded") +
      W_NATIVE_DOUBTFUL      * (coalesce(origin_piedmont, "") == "native_doubtful") +
      W_NATURALIZED_PIEDMONT * (coalesce(status_piedmont, "") %in% c("NAT", "INV")) +
      W_NATURALIZED_ITALIA   * (coalesce(status_italy, "") %in% c("NAT", "INV")) +
      W_HERBARIUM_VOUCHER    * (coalesce(n_occ_herb, 0) > 0) +
      W_ENDEMIC_ITALY        * coalesce(endemic_italy, FALSE),
    
    evidence_cultivated =
      W_CULTIVATED_FLAG * coalesce(cultivated, FALSE) +
      W_ALSO_PLANTED    * coalesce(flag_also_planted, FALSE) +
      W_FERAL           * coalesce(feral_flag, FALSE) +
      W_WOODY_CASUAL    * (coalesce(status_italy, "") == "CAS" & is_woody(lifeform_drevojan)),
    
    exclude_recorded_by_mistake = coalesce(origin_piedmont, "") == "recorded_by_mistake",
    
    cultivation_status = case_when(
      exclude_recorded_by_mistake ~ "exclude_recorded_by_mistake",
      evidence_wild == 0 & evidence_cultivated == 0 ~ "uncertain_no_evidence",
      evidence_wild >= evidence_cultivated + MARGIN ~ "confirmed_wild",
      evidence_cultivated >= evidence_wild + MARGIN ~ "likely_cultivated",
      evidence_wild == 0 | evidence_cultivated == 0 ~ "uncertain_weak_evidence",
      TRUE ~ "uncertain_conflicting_evidence"
    )
  )

# ---- Summary ----

message("\n=== Distribuzione cultivation_status ===")
print(flagged %>% count(cultivation_status, sort = TRUE))

conflicting <- flagged %>%
  filter(cultivation_status == "uncertain_conflicting_evidence") %>%
  select(wcvp_final_id, final_reconciled_name, origin_piedmont, status_piedmont, status_italy,
         wcvp_in_italy, wcvp_introduced_in_italy, cultivated, flag_also_planted, feral_flag,
         n_occ_herb, lifeform_drevojan, evidence_wild, evidence_cultivated) %>%
  arrange(desc(pmax(evidence_wild, evidence_cultivated)))

message(nrow(conflicting), " taxa with comparable evidence on both sides (review priority).")

# ---- Export ----

write_csv(flagged, file.path(output_dir, "cultivation_status_by_id.csv"))
write_csv(conflicting, file.path(output_dir, "cultivation_conflicting_evidence_review.csv"))

message("\nWritten: ", file.path(output_dir, "cultivation_status_by_id.csv"))
message("Written: ", file.path(output_dir, "cultivation_conflicting_evidence_review.csv"),
        " (", nrow(conflicting), " rows)")
