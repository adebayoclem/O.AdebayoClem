# Import Libraries
pacman::p_load(
  pacman,
  tidyverse,
  readxl,
  writexl,
  DBI,
  odbc,
  knitr,
  data.table,
  kableExtra,
  janitor,
  report,
  naniar,
  arrow,
  RecordLinkage,
  fastLink
)

# Run Data Preparation
rm(list=ls())
source("01_linking/data_preparation.R")

# Match on Primary Identifier
id_matches <- left_join(
    dataset_a_clean,
    dataset_b_clean,
    by = join_by("P_record_id" == "L_record_id"),
    na_matches = "never"
  ) %>%
  mutate(match = ifelse(is.na(L_date), FALSE, TRUE))

id_matches %>%
  group_by(match) %>%
  tally

# Match on Secondary Identifiers
# Deterministic linkage rules:
# - Match if primary identifier matches
# - Match if birth date and location match and primary identifier doesn't disagree

id_matches_true <- id_matches %>%
  filter(match == TRUE)

id_matches_false <- id_matches %>%
  filter(match == FALSE) %>%
  select(starts_with("P"))

secondary_matches <- left_join(
    id_matches_false,
    dataset_b_clean,
    by = c("P_birth_date" = "L_birth_date", "P_location" = "L_location"),
    na_matches = "never"
  ) %>%
  mutate(match1 = ifelse(is.na(L_date), FALSE, TRUE)) %>%
  mutate(match2 = if_else((is.na(L_date)) |
                            ((!is.na(P_record_id) |
                                !is.na(L_record_id)) &
                               (P_record_id != L_record_id)
                            ), FALSE, TRUE, TRUE))

secondary_matches %>%
  group_by(match1, match2) %>%
  tally

# Probabilistic Matching Using FastLink
# Clean workspace
rm(list = setdiff(ls(), c("dataset_b_clean_orig", "dataset_a_clean_orig")))

# Exclude exact matches
dataset_a_no_match <- anti_join(dataset_a_clean_orig, dataset_b_clean_orig, by = "record_id")
dataset_b_no_match <- anti_join(dataset_b_clean_orig, dataset_a_clean_orig, by = "record_id")

# Create blocks
block_out <- blockData(dataset_a_no_match, dataset_b_no_match, varnames = "category")

# Subset data into blocks
dataset_a_blocks <- list()
for (i in 1:3) {
  dataset_a_blocks[[i]] <- dataset_a_no_match[block_out[[paste0("block.", i)]]$dfA.inds, ]
}

list2env(setNames(dataset_a_blocks, paste0("dataset_a_block", 1:3)), envir = .GlobalEnv)

dataset_b_blocks <- list()
for (i in 1:3) {
  dataset_b_blocks[[i]] <- dataset_b_no_match[block_out[[paste0("block.", i)]]$dfB.inds, ]
}

list2env(setNames(dataset_b_blocks, paste0("dataset_b_block", 1:3)), envir = .GlobalEnv)

# Run FastLink
gc() # free up memory

fl_out <- list()

for (i in 1:3) {
  fl_out[[i]] <- fastLink(
    get(paste0("dataset_a_block", i)),
    get(paste0("dataset_b_block", i)),
    varnames = c(
      "date",
      "birth_date_int",
      "location",
      "site_code",
      "record_id",
      "date_int"
    ),
    stringdist.match = c("location", "record_id"),
    numeric.match = c("date_int", "birth_date_int"),
    partial.match = c("location", "record_id"),
    threshold.match = .85,
    dedupe.matches = FALSE
  )
}

list2env(setNames(fl_out, paste0("fl_out", 1:3)), envir = .GlobalEnv)

# Combine Matches
matches <- list()
for (i in 1:3) {
  matches[[i]] <- get(paste0("fl_out", i))[[1]]
}

matches_combined <- do.call("rbind", matches)

# Summarize Results
agg.out <- aggregateEM(
    em.list = list(
      fl_out1,
      fl_out2,
      fl_out3
    )
  )

summary(agg.out)

# Get matched records
dataset_a_matches <- dataset_a_no_match[matches_combined$inds.a, ]
dataset_b_matches <- dataset_b_no_match[matches_combined$inds.b, ]