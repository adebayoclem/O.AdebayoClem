# IMPORT LIBRARIES
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

# RUN CLEANING
rm(list=ls())
source("01_linking/cleaning_for_linkage.R")

# MATCH ON NHS NUMBER
nhs_num_match <-
  left_join(
    ECDS_clean,
    SGSS_clean,
    by = join_by("P_nhs_number" == "L_nhs_number"),
    na_matches = "never"
  ) %>%
  mutate(match = ifelse(is.na(L_date), FALSE, TRUE))

nhs_num_match %>%
  group_by(match) %>%
  tally

# MATCH ON DATE OF BIRTH OR POSTCODE
# deterministic linkage rules:
# match if NHS number matches
# match if date of birth and postcode match and NHS number doesn't disagree

nhs_num_match_true <- nhs_num_match %>%
  filter(match == TRUE)

nhs_num_match_false <- nhs_num_match %>%
  filter(match == FALSE) %>%
  select(starts_with("P"))

dob_pcode_match <-
  left_join(
    nhs_num_match_false,
    SGSS_clean,
    by = c("P_birth_date" = "L_birth_date", "P_postcode" = "L_postcode"),
    na_matches = "never"
  ) %>%
  mutate(match1 = ifelse(is.na(L_date), FALSE, TRUE)) %>%
  mutate(match2 = if_else((is.na(L_date)) |
                            ((!is.na(P_nhs_number) |
                                !is.na(L_nhs_number)) &
                               (P_nhs_number != L_nhs_number)
                            ), FALSE, TRUE, TRUE))

dob_pcode_match %>%
  group_by(match1, match2) %>%
  tally


# TRY PROBABILISTIC MATCHING WITH FASTLINK
# clean up space
rm(list = setdiff(ls(), c("SGSS_clean_nop", "ECDS_clean_nop")))

# exclude matches on NHS number
ECDS_no_nhs <-
  anti_join(ECDS_clean_nop, SGSS_clean_nop, by = "nhs_number")
SGSS_no_nhs <-
  anti_join(SGSS_clean_nop, ECDS_clean_nop, by = "nhs_number")

# blocking
block_out <-
  blockData(ECDS_no_nhs, SGSS_no_nhs, varnames = "sex")

# subset data into blocks
ECDS_blocks <- list()
for (i in 1:3) {
  ECDS_blocks[[i]] <-
    ECDS_no_nhs[block_out[[paste0("block.", i)]]$dfA.inds, ]
}

list2env(setNames(ECDS_blocks, paste0("ECDS_block", 1:3)), envir = .GlobalEnv)

SGSS_blocks <- list()
for (i in 1:3) {
  SGSS_blocks[[i]] <-
    SGSS_no_nhs[block_out[[paste0("block.", i)]]$dfB.inds, ]
}

list2env(setNames(SGSS_blocks, paste0("SGSS_block", 1:3)), envir = .GlobalEnv)

## Run fastLink on each
gc() # free up memory

# try one
# fl_out1 <- fastLink(
#     ECDS_block1,
#     SGSS_block1,
#     varnames = c(
#       "date",
#       "birth_date_int",
#       "postcode",
#       "provider_code",
#       "nhs_number",
#       "date_int"
#     ),
#     stringdist.match = c("postcode", "nhs_number"),
#     numeric.match = c("date_int", "birth_date_int"),
#     partial.match = c("postcode", "nhs_number"),
#     threshold.match = .85,
#     dedupe.matches = FALSE)

# as loop
fl_out <- list()

for (i in 1:3) {
  fl_out[[i]] <- fastLink(
    get(paste0("ECDS_block", i)),
    get(paste0("SGSS_block", i)),
    varnames = c(
      "date",
      "birth_date_int",
      "postcode",
      "provider_code",
      "nhs_number",
      "date_int"
    ),
    stringdist.match = c("postcode", "nhs_number"),
    numeric.match = c("date_int", "birth_date_int"),
    partial.match = c("postcode", "nhs_number"),
    threshold.match = .85,
    dedupe.matches = FALSE
  )
}

list2env(setNames(fl_out, paste0("fl_out", 1:3)), envir = .GlobalEnv)

# fetch matches
matches <- list()
for (i in 1:3) {
  matches[[i]] <- get(paste0("fl_out", i))[[1]]
}

matches_combined <- do.call("rbind", matches)

# # fetch EM
# em <- list()
# for (i in 1:12) {
#   em[[i]] <- get(paste0("fl_out", i))[[2]]
# }
# 
# em_combined <- do.call("rbind", em)
# 
# # fetch patterns
# patterns <- list()
# for (i in 1:12) {
#   patterns[[i]] <- get(paste0("fl_out", i))[[3]]
# }
# 
# patterns_combined <- do.call("rbind", patterns)

# summarise
agg.out <-
  aggregateEM(
    em.list = list(
      fl_out1,
      fl_out2,
      fl_out3
    )
  )

summary(agg.out)

# get ECDS ids for matches
ECDS_matches <- ECDS_no_nhs[matches_combined$inds.a, ]
SGSS_matches <- SGSS_no_nhs[matches_combined$inds.b, ]


