# run source data queries
# Sentinel diagnoses linked
source("sentinel_diagnoses_linking.R")

# SGSS HCV diagnoses linked
source("Data_prep_SGSS.r")

# remove all but data needed
rm(list= ls()[!(ls() %in% c('attendancesbloodtests','attendances_sentinel', 
                            'HBV_included_diagnoses_sentinel', 'HCV_included_diagnoses_sentinel',
                            'HCV_DIAGNOSES_SENTINEL', 'HBV_DIAGNOSES_SENTINEL',
                            'HCV_SGSS_included_diagnoses', 'HCV_SGSS_included_diagnoses2'))])

# CHARING CROSS
# filter to get data for relevant site (and time period if wanted)
# 1. Sentinel
charingcrosssentinel <- HCV_included_diagnoses_sentinel%>%
  filter(SITE == "RYJ02")

# 2. SGSS - use wider definition (included_diagnoses2)
charingcrosssgss <- HCV_SGSS_included_diagnoses2%>%
  filter(SITE == "RYJ02")


# anti-join to find records in one database but not the other
SGSS_only_HCV_cc <- anti_join(charingcrosssgss, charingcrosssentinel, by = c("NHS_NUMBER"))

Sentinel_only_HCV_cc <- anti_join(charingcrosssentinel, charingcrosssgss, by = c("NHS_NUMBER"))

# there were none in sentinel that didn't match to an SGSS record on NHS Number only so tried matching on both NHS Number and Specimen Date
Sentinel_only_HCV_cc <- anti_join(charingcrosssentinel, charingcrosssgss, by = c("NHS_NUMBER", "SPECDATE" = "SPECIMEN_DATE"))

# CHELSEA & WESTMINSTER
# filter to get data for relevant site (and time period if wanted)
# 1. Sentinel
chelwestsentinel <- HCV_included_diagnoses_sentinel%>%
  filter(SITE == "RQM06" | SITE == "RQM25" | SITE == "RQM01")
# 2. SGSS - use wider definition (included_diagnoses2)
chelwestsgss <- HCV_SGSS_included_diagnoses2%>%
  filter(SITE == "RQM06" | SITE == "RQM25" | SITE == "RQM01")


# anti-join to find records in one database but not the other
SGSS_only_HCV_cw <- anti_join(chelwestsgss, chelwestsentinel, by = c("NHS_NUMBER"))

Sentinel_only_HCV_cw <- anti_join(chelwestsentinel, chelwestsgss, by = c("NHS_NUMBER"))

# there were none in sentinel that didn't match to an SGSS record on NHS Number only so tried matching on both NHS Number and Specimen Date
Sentinel_only_HCV_cw <- anti_join(chelwestsentinel, chelwestsgss, by = c("NHS_NUMBER", "SPECDATE" = "SPECIMEN_DATE"))

# ST THOMAS
# filter to get data for relevant site (and time period if wanted)
# 1. Sentinel
stthomassentinel <- HCV_included_diagnoses_sentinel%>%
  filter(SITE == "RJ122")
# 2. SGSS - use wider definition (included_diagnoses2)
stthomassgss <- HCV_SGSS_included_diagnoses2%>%
  filter(SITE == "RJ122")


# anti-join to find records in one database but not the other
SGSS_only_HCV_st <- anti_join(stthomassgss, stthomassentinel, by = c("NHS_NUMBER"))

Sentinel_only_HCV_st <- anti_join(stthomassentinel, stthomassgss, by = c("NHS_NUMBER"))

# there were none in sentinel that didn't match to an SGSS record on NHS Number only so tried matching on both NHS Number and Specimen Date
Sentinel_only_HCV_st <- anti_join(stthomassentinel, stthomassgss, by = c("NHS_NUMBER", "SPECDATE" = "SPECIMEN_DATE"))


# ST MARYS
# filter to get data for relevant site (and time period if wanted)
# 1. Sentinel
stmarysentinel <- HCV_included_diagnoses_sentinel%>%
  filter(SITE == "RYJ01")
# 2. SGSS - use wider definition (included_diagnoses2)
stmarysgss <- HCV_SGSS_included_diagnoses2%>%
  filter(SITE == "RYJ01")

# anti-join to find records in one database but not the other
SGSS_only_HCV_sm <- anti_join(stmarysgss, stmarysentinel, by = c("NHS_NUMBER"))

Sentinel_only_HCV_sm <- anti_join(stmarysentinel, stmarysgss, by = c("NHS_NUMBER"))

# there were none in sentinel that didn't match to an SGSS record on NHS Number only so tried matching on both NHS Number and Specimen Date
Sentinel_only_HCV_sm <- anti_join(stmarysentinel, stmarysgss, by = c("NHS_NUMBER", "SPECDATE" = "SPECIMEN_DATE"))

# WEST MIDDLESEX
# filter to get data for relevant site (and time period if wanted)
# 1. Sentinel
westmidsentinel <- HCV_included_diagnoses_sentinel%>%
  filter(SITE == "RQM91")
# 2. SGSS - use wider definition (included_diagnoses2)
westmidsgss <- HCV_SGSS_included_diagnoses2%>%
  filter(SITE == "RQM91")

# anti-join to find records in one database but not the other
SGSS_only_HCV_wm <- anti_join(westmidsgss, westmidsentinel, by = c("NHS_NUMBER"))

Sentinel_only_HCV_wm <- anti_join(westmidsentinel, westmidsgss, by = c("NHS_NUMBER"))

# there were none in sentinel that didn't match to an SGSS record on NHS Number only so tried matching on both NHS Number and Specimen Date
Sentinel_only_HCV_wm <- anti_join(westmidsentinel, westmidsgss, by = c("NHS_NUMBER", "SPECDATE" = "SPECIMEN_DATE"))