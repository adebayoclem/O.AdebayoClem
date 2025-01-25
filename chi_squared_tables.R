install.packages("gtsummary")
library(gtsummary)

## This scripts produces a table to be used for chi squared tests and charts for the 5 sites

#Run HCV_Sentinel_Linking.R, HBV_Sentinel_Linking.R, HCV_diagnosis_linking.R 
# run 5 sites versions - these load 5 sites parquet and link to that (comment out other lines)
source ("01_linking/HCV_sentinel_linking.R")
source ("01_linking/HBV_sentinel_linking.R")
source("01_linking/HIV_sentinel_linking.R")
source ("01_linking/HCV_diagnosis_linking.R")
source ("01_linking/HBV_diagnosis_linking.R")

#restrict data to chosen 5 sites
attendees_5_sites <- attendees%>%filter(SITE %in%five_included_sites)
attendances_5_sites <- attendances%>%filter(SITE%in%five_included_sites)
attendancesbloodtests_5_sites <- attendancesbloodtests%>%filter(SITE%in%five_included_sites)
attendeesbloodtests_5_sites <- ECDSBloodsfirstdate%>%filter(SITE%in%five_included_sites)
attendees_tested_HCV_5 <- included_tests_HCV5 %>% filter(SITE %in%five_included_sites)
attendees_tested_HBV_5 <- included_tests_HBV5 %>% filter(SITE %in%five_included_sites)
attendees_tested_HIV_5 <- included_tests_HIV5 %>% filter(SITE %in%five_included_sites)

HCV_diags_5 <-  HCV_included_diagnoses_sentinel %>% 
  filter(Hospital == "Whipps Cross University Hospital" | Hospital=="St Thomas' Hospital" | Hospital=="The Royal London Hospital" | Hospital=="Newham General Hospital" | Hospital=="Homerton University Hospital")

HBV_diags_5 <- HBV_included_diagnoses_sentinel %>% filter(SITE %in% five_included_sites)

##unlinked (all sites) 

#calculations with the following variables are unlinked to ECDS
HCV_DIAG_SENTINEL
HBV_DIAGNOSES_SENTINEL


#Get NHS numbers for attendees with a HCV diagnosis
unlinked_HCV_diagnoses_NHSn_chi <- HCV_DIAG_SENTINEL %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_diag_unlinked ='Yes')


#Get NHS numbers for HCV positive patients linked to care (within 28 days)

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(max_tx_date = HCVRNAdate + 28, 
         include = case_when(HCVRNAdate < postRxdate &
                               (postRxdate < max_tx_date)~ "INCLUDE",
                             FALSE ~ "EXCLUDE"))

HCV_linkedcare_NHSn_chi <- HCV_DIAG_SENTINEL %>% 
  filter(include == "INCLUDE") %>%
  select(NHS_NUMBER) %>% 
  mutate(HCV_linked = "Yes")

#Get NHS numbers for HCV positive patients who were previously but not currently in care who attend care

HCV_prelost <- HCV_DIAG_SENTINEL %>% 
  filter((preRxoutcome == "Lost to follow-up (Did not attend 12/24 week post-treatment follow-up appointments)"| preRxoutcome == "Did not commence treatment") & !is.na(postRxdate)) 

HCV_reengage_NHSn_chi <- HCV_prelost %>% 
  select(NHS_NUMBER) %>% 
  mutate(HCV_reengage = "Yes")

#Get NHS numbers for HCV positive patients who who are lost to follow up from HCV services / not engaged in care 

HCVlost <- HCV_DIAG_SENTINEL %>% 
  filter(preRxoutcome == "Lost to follow-up (Did not attend 12/24 week post-treatment follow-up appointments)"| preRxoutcome == "Did not commence treatment")

HCV_lost_NHSn_chi <- HCVlost %>% 
  select(NHS_NUMBER) %>% 
  mutate(HCV_lost = "Yes")


df_list = list(HCV_DIAG_SENTINEL, unlinked_HCV_diagnoses_NHSn_chi,HCV_linkedcare_NHSn_chi,HCV_reengage_NHSn_chi, HCV_lost_NHSn_chi)

linkagetocare_joined <- df_list %>% reduce(left_join, by='NHS_NUMBER', na_matches = "never")

#replace na values with 'No'
linkagetocare_joined_clean <- linkagetocare_joined %>% 
  mutate(HCV_diags_unlinked = replace_na(HCV_diag_unlinked, "No"),
         HCV_linkedcare = replace_na(HCV_linked, "No"),
         HCV_reengagedcare = replace_na(HCV_reengage, "No"),
         HCV_lostfollowup = replace_na(HCV_lost, "No"))


#for HCV re-engagement in care - indicator 11b
hcv_reengagetbl <- linkagetocare_joined_clean %>% 
  filter(HCV_lost == "Yes") %>% 
  select(agegroup, sex, CombinedEthnicity, IMD, HCV_reengagedcare) %>%
  tbl_summary(by = "HCV_reengagedcare", percent = "row", type = list(IMD ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HCV linked to care 28 days - indicator 12c
hcv_linkedcaretbl <- linkagetocare_joined_clean %>% 
  filter(HCV_diag_unlinked == "Yes") %>% 
  select(agegroup, sex, CombinedEthnicity, IMD, HCV_linkedcare) %>%
  tbl_summary(by = "HCV_linkedcare", percent = "row", type = list(IMD ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

##attendances 

##Get NHS numbers for attendances overall
attendances_NHSn <- attendances %>%  select(NHS_NUMBER)%>%
  mutate(attendance ='Yes')

#Get NHS numbers for attendances with a blood test
attendance_blood_tests_NHSn <- attendancesbloodtests %>%  select(NHS_NUMBER)%>%
  mutate(blood_test ='Yes')

df_list = list(attendances,attendances_NHSn,attendance_blood_tests_NHSn)

attendances_joined <- df_list %>% reduce(left_join, by='NHS_NUMBER', na_matches = "never")

#replace na values with 'No'
attendances_joined_clean <- attendances_joined %>% 
  mutate(attendances = replace_na(attendance, "No"),
         blood_tests = replace_na(blood_test, "No"))

##attendees

#Get NHS numbers for attendees overall
attendees_NHSn <- attendees %>%  select(NHS_NUMBER)%>%
  mutate(attendee ='Yes')

## starting from attendees table, get NHS numbers for features we want to add

#Get NHS numbers for attendees with a blood test
blood_tests_NHSn <- attendeesbloodtests %>%  select(NHS_NUMBER)%>%
  mutate(blood_test ='Yes')


#Get NHS numbers for attendees with a HCV test
HCV_tests_NHSn_chi <- included_tests_HCV4 %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_test ='Yes')

#Get NHS numbers for attendees with a HBV test
HBV_tests_NHSn_chi <- included_tests_HBV4 %>% 
  select(NHS_NUMBER) %>%
  mutate(HBV_test ='Yes')

#Get NHS numbers for attendees with a HIV test
HIV_tests_NHSn_chi <- included_tests_HIV4 %>% 
  select(NHS_NUMBER) %>%
  mutate(HIV_test ='Yes')

#Get NHS numbers for attendees with a positive HCV test
HCV_pos_NHSn_chi <- HCV_included_diagnoses_sentinel %>% 
  select(NHS_NUMBER) %>% 
  mutate(HCV_pos ='Yes')

#Get NHS numbers for attendees with a confirmatory HCV test
HCV_conf_NHSn_chi <- HCV_included_diagnoses_sentinel %>% 
  filter(!is.na (HCVRNA)) %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_conf ='Yes')

#Get NHS numbers for attendees with a reflex HCV test
HCV_reflex_NHSn_chi <- HCV_included_diagnoses_sentinel %>% 
  filter(RNAtiming == "Reflex") %>% 
  select(NHS_NUMBER) %>% 
  mutate(HCV_reflex_test ='Yes')

#Get NHS numbers for attendees with a positive confirmatory HCV test
HCV_conf_pos_NHSn_chi <- HCV_included_diagnoses_sentinel %>% 
  filter(HCVRNA == "positive") %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_confpos ='Yes')

#Get NHS numbers for attendees with a positive HBV test
HBV_pos_NHSn_chi <- HBV_included_diagnoses_sentinel %>%
  filter(HBsAg == "positive") %>% 
  select(NHS_NUMBER) %>%
  mutate(HBV_pos ='Yes')


#Get NHS numbers for attendees with a confirmatory HBV test
#HBV_conf_NHSn_chi <- HBV_included_5 %>% 
#  select(NHS_NUMBER) %>%
#  filter(___ == "Positive") %>% 
#  mutate(HBV_conf ='Yes')

#Get NHS numbers for attendees with a HCV and blood test
HCV_blood_NHSn_chi <- included_tests_HCV5 %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_blood_test ='Yes')

#Get NHS numbers for attendees with a HBV and blood test
HBV_blood_NHSn_chi <- included_tests_HBV5 %>% 
  select(NHS_NUMBER) %>%
  mutate(HBV_blood_test ='Yes')

#Get NHS numbers for attendees with a HCV diagnosis
HCV_diagnoses_NHSn_chi <- HCV_included_diagnoses_sentinel %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_diag ='Yes')

#Get NHS numbers for attendees with a NEW HCV diagnosis
HCV_newdiagnoses <- HCV_included_diagnoses_sentinel %>% 
  filter(rna_positive_new == "New")

HCV_newdiagnoses_NHSn_chi <- HCV_newdiagnoses %>% 
  select(NHS_NUMBER) %>%
  mutate(HCV_diag_new ='Yes')

#Get NHS numbers for attendees with a HBV diagnosis
HBV_diagnoses_NHSn_chi <- HBV_included_diagnoses_sentinel %>% 
  select(NHS_NUMBER) %>%
  mutate(HBV_diag ='Yes')

#Get NHS numbers for attendees with a NEW HBV diagnosis
HBV_newdiagnoses <- HBV_included_diagnoses_sentinel %>% 
  filter(Newdiag == "Yes")

HBV_newdiagnoses_NHSn_chi <- HBV_newdiagnoses %>% 
  select(NHS_NUMBER) %>%
  mutate(HBV_diag_new='Yes')

#Get NHS numbers for HCV positive patients linked to care (within 28 days)

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(max_tx_date = HCVRNAdate + 28, 
         include = case_when(HCVRNAdate < postRxdate &
                               (postRxdate < max_tx_date)~ "INCLUDE",
                             FALSE ~ "EXCLUDE"))%>%
  rename("NHS_NUMBER" = "NHSNumber")

HCV_linkedcare_NHSn_chi <- HCV_DIAG_SENTINEL %>% 
  filter(include == "INCLUDE") %>%
  select(NHS_NUMBER) %>% 
  mutate(HCV_linked = "Yes")

#Get NHS numbers for HCV positive patients who were previously but not currently in care who attend care

HCV_prelost <- HCV_DIAG_SENTINEL %>% 
  filter((preRxoutcome == "Lost to follow-up (Did not attend 12/24 week post-treatment follow-up appointments)"| preRxoutcome == "Did not commence treatment") & !is.na(postRxdate)) 

HCV_reengage_NHSn_chi <- HCV_prelost %>% 
  select(NHS_NUMBER) %>% 
  mutate(HCV_reengage = "Yes")

#Get NHS numbers for HCV positive patients who who are lost to follow up from HCV services / not engaged in care 

HCVlost <- HCV_DIAG_SENTINEL %>% 
  filter(preRxoutcome == "Lost to follow-up (Did not attend 12/24 week post-treatment follow-up appointments)"| preRxoutcome == "Did not commence treatment")

HCV_lost_NHSn_chi <- HCVlost %>% 
  select(NHS_NUMBER) %>% 
  mutate(HCV_lost = "Yes")


#ATTENDEES (LIMITED TO 5 SITES)

df_list = list(attendees, blood_tests_NHSn,HCV_tests_NHSn_chi,HCV_pos_NHSn_chi,
               HCV_conf_NHSn_chi,HCV_conf_pos_NHSn_chi,HCV_reflex_NHSn_chi, HCV_blood_NHSn_chi,
               HCV_diagnoses_NHSn_chi, HCV_newdiagnoses_NHSn_chi, HCV_linkedcare_NHSn_chi, 
               HCV_reengage_NHSn_chi, HCV_lost_NHSn_chi, HBV_tests_NHSn_chi,HBV_pos_NHSn_chi,
               HBV_blood_NHSn_chi,HBV_diagnoses_NHSn_chi, HBV_newdiagnoses_NHSn_chi, HIV_tests_NHSn_chi) 

attendees_joined <- df_list %>% reduce(left_join, by='NHS_NUMBER', na_matches = "never")

attendees_joined<- attendees_joined%>%
  distinct(TOKEN_PERSON_ID, .keep_all = TRUE)%>%
  filter(live_HIV =='LIVE')

#replace na values with 'No'
attendees_joined_clean <- attendees_joined %>% 
  mutate(blood_tests = replace_na(blood_test, "No"),
         HCV_tests = replace_na(HCV_test, "No"),
         HBV_tests = replace_na(HBV_test, "No"),
         HIV_tests = replace_na(HIV_test, "No"),
         HCV_positive = replace_na(HCV_pos, "No"),
         HBV_positive = replace_na(HBV_pos, "No"),
         HCV_reflex = replace_na(HCV_reflex_test, "No"),
         HCV_confirmatory = replace_na(HCV_conf, "No"),
         HCV_confirmatorypositive = replace_na(HCV_confpos, "No"),
         HCV_linkedcare = replace_na(HCV_linked, "No"),
         HCV_reengagedcare = replace_na(HCV_reengage, "No"),
         HCV_lostfollowup = replace_na(HCV_lost, "No"),
         #HBV_confirmatory = replace_na(HBV_conf, "No"),
         HCV_blood_tests = replace_na(HCV_blood_test, "No"),
         HBV_blood_tests = replace_na(HBV_blood_test, "No"),
         HCV_diags = replace_na(HCV_diag, "No"),
         HCV_diags_new = replace_na(HCV_diag_new, "No"),
         HBV_diags = replace_na(HBV_diag, "No"),
         HBV_diags_new = replace_na(HBV_diag_new, "No"))

## tidy - remove columns not needed
attendees_joined_clean_2 <- attendees_joined_clean%>%
select (-c(INVESTIGATION_CODE, INVESTIGATION_CODE_Description, InvestigationIdx, 
           CHIEF_COMPLAINT, CHIEF_COMPLAINT_Description, FYEAR.x, month,
           PROVIDER_CODE_DERIVED, SITE, ARRIVAL_MONTH, ARRIVAL_DATE, INVESTIGATION_DATE,
           AGE_AT_ARRIVAL, LSOA_2011, ETHNIC_CATEGORY, ETHNIC_CATEGORY_Description, London_Provider_ICS, Provider_derived,
          blood_test, HCV_test, HBV_test, HIV_test, HCV_pos, HBV_pos, HCV_reflex_test,
           HCV_conf, HCV_confpos, HCV_linked, HCV_reengage, HCV_lost, HCV_blood_test,
           HBV_blood_test, HCV_diag, HCV_diag_new, HBV_diag, HBV_diag_new, age_group2))

check <- attendees_joined_clean_2%>%
  filter(HCV_diags_new =='Yes')

write_parquet(attendees_joined_clean_2, "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendees_joined_5_sites.parquet")
# I think this can go as age group is already in there
# attendees_joined_clean <- attendees_joined_clean %>%
#   mutate(age_group = case_when(AGE_AT_ARRIVAL <= 14 ~ "Under 15",
#                                AGE_AT_ARRIVAL >=15 & AGE_AT_ARRIVAL <= 24  ~ "15-24",
#                                AGE_AT_ARRIVAL >= 25 & AGE_AT_ARRIVAL <= 34 ~ "25-34",
#                                AGE_AT_ARRIVAL >= 35 & AGE_AT_ARRIVAL <= 49 ~ "35-49",
#                                AGE_AT_ARRIVAL >= 50 & AGE_AT_ARRIVAL <= 64 ~ "50-64",
#                                AGE_AT_ARRIVAL >= 65 & AGE_AT_ARRIVAL <= 79 ~ "65-79",
#                                AGE_AT_ARRIVAL > 79 ~ "80+")) %>%
#   mutate(age_group2 = case_when(AGE_AT_ARRIVAL < 45 ~ "16-44",
#                                 AGE_AT_ARRIVAL > 44 & AGE_AT_ARRIVAL < 65 ~ "45-64",
#                                 AGE_AT_ARRIVAL > 64 ~ "65+")) 

#create chi squared table and values for 5 sites

attendees_joined_clean<- attendees_joined_clean%>%
  filter(STATED_GENDER == "1" | STATED_GENDER =="2")

#for blood tests - indicator 1a
bloodtestsattendeestbl <- attendees_joined_clean %>% 
  filter(attendee == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, blood_tests) %>%
  tbl_summary(by = "blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for blood tests - indicator 1b
bloodtestsattendancestbl <- attendances_joined_clean %>% 
  filter(attendance == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, blood_tests) %>%
  tbl_summary(by = "blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HCV tests - indicator 2
HCV_tbl <- attendees_joined_clean %>% 
  filter(live_HCV == 'LIVE')%>%
  filter(blood_test == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_tests) %>%
  tbl_summary(by = "HCV_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HBV tests - indicator 2
HBV_tbl <- attendees_joined_clean %>%
  filter(live_HBV =='LIVE') %>% 
  filter(blood_test == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HBV_tests) %>%
  tbl_summary(by = "HBV_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HCV tests - indicator 3
HCV_coverage_tbl <- attendees_joined_clean %>% 
  filter(live_HCV =='LIVE') %>% 
  filter(attendee == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_tests) %>%
  tbl_summary(by = "HCV_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HBV tests - indicator 3
HBV_coverage_tbl <- attendees_joined_clean %>%
  filter(live_HBV =='LIVE') %>% 
  filter(attendee == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HBV_tests) %>%
  tbl_summary(by = "HBV_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HIV tests - indicator 3
HIV_tbl <- attendees_joined_clean %>% 
  filter(live_HIV =='LIVE') %>% 
  filter(attendee == "Yes") %>%
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HIV_tests) %>%
  tbl_summary(by = "HIV_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for confirmatory HCV tests - indicator 4
hcv_confirmatorytbl <- attendees_joined_clean %>% 
  filter(HCV_pos == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_confirmatory) %>%
  tbl_summary(by = "HCV_confirmatory", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for reflex HCV tests - indicator 5
hcv_reflex_tbl <- attendees_joined_clean %>% 
  filter(HCV_pos == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_reflex) %>%
  tbl_summary(by = "HCV_reflex", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HCV new diagnoses - indicator 6
hcv_newdiag <- attendees_joined_clean %>% 
  filter(HCV_blood_test == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_diags_new) %>%
  tbl_summary(by = "HCV_diags_new", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HBV new diagnoses - indicator 6
hbv_newdiag <- attendees_joined_clean %>% 
  filter(HBV_blood_test == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HBV_diags_new) %>%
  tbl_summary(by = "HBV_diags_new", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for positive confirmatory HCV tests - indicator 7
hcv_posconfirmatorytbl <- attendees_joined_clean %>% 
  filter(HCV_pos == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_confirmatorypositive) %>%
  tbl_summary(by = "HCV_confirmatorypositive", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()


#for HCV re-engagement in care - indicator 11b
hcv_reengagetbl <- attendees_joined_clean %>% 
  filter(HCV_lost == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_reengagedcare) %>%
  tbl_summary(by = "HCV_reengagedcare", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for HCV linked to care 28 days - indicator 12c
hcv_linkedcaretbl <- attendees_joined_clean %>% 
  filter(HCV_diag == "Yes") %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_linkedcare) %>%
  tbl_summary(by = "HCV_linkedcare", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()


# #for attendees
# attendeestbl <- attendees_joined_clean %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, attendees) %>%
#   tbl_summary(by = "attendees", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()

# #for HCV and blood tests
# hcv_bloods_tbl <- attendees_joined_clean %>% 
#   filter(SITE %in% five_included_sites) %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_blood_tests) %>%
#   tbl_summary(by = "HCV_blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()
# 
# #for HCV diagnoses
# hcv_diag <- attendees_joined_clean %>% 
#   filter(SITE %in% five_included_sites) %>% 
#   filter(HCV_blood_test == "Yes") %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_diags) %>%
#   tbl_summary(by = "HCV_diags", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()
# 
# 
# #for positive HCV tests
# hcv_positive <- attendees_joined_clean %>% 
#   filter(SITE %in% five_included_sites) %>% 
#   filter(HCV_blood_test == "Yes") %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, HCV_positive) %>%
#   tbl_summary(by = "HCV_positive", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()
# 
# #for positive HBV tests
# hbv_positive <- attendees_joined_clean %>% 
#   filter(SITE %in% five_included_sites) %>% 
#   filter(HBV_blood_test == "Yes") %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, HBV_positive) %>%
#   tbl_summary(by = "HBV_positive", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()
# 
# 
# #for HBV and blood tests
# hbv_bloods_tbl <- attendees_joined_clean %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, HBV_blood_tests) %>%
#   tbl_summary(by = "HBV_blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()
# 
# #for HBV diagnoses
# hbv_diag <- attendees_joined_clean %>% 
#   filter(SITE %in% five_included_sites) %>% 
#   filter(HBV_blood_test == "Yes") %>% 
#   select(age_group, STATED_GENDER, ethnic_group, IMD_19, HBV_diags) %>%
#   tbl_summary(by = "HBV_diags", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
#   add_overall(last = TRUE) %>% 
#   add_p()





################# are the sections below still needed?
############

#ATTENDEES (SENTINEL SITES)

df_list_sentinel = list(attendees_sentinel, attendees_NHSn, blood_tests_NHSn,HCV_tests_NHSn_chi,HCV_pos_NHSn_chi,HCV_conf_NHSn_chi, HCV_blood_NHSn_chi,HCV_diagnoses_NHSn_chi, HCV_newdiagnoses_NHSn_chi, HBV_tests_NHSn_chi,HBV_pos_NHSn_chi,HBV_blood_NHSn_chi,HBV_diagnoses_NHSn_chi, HBV_newdiagnoses_NHSn_chi) 

attendees_sentinel_joined <- df_list_sentinel %>% reduce(left_join, by='NHS_NUMBER', na_matches = "never")


#replace na values with 'No'
attendees_sentinel_joined_clean <- attendees_sentinel_joined %>% 
  mutate(attendees = replace_na(attendee, "No"),
         blood_tests = replace_na(blood_test, "No"),
         HCV_tests = replace_na(HCV_test, "No"),
         HBV_tests = replace_na(HBV_test, "No"),
         HCV_positive = replace_na(HCV_pos, "No"),
         HBV_positive = replace_na(HBV_pos, "No"),
         HCV_confirmatory = replace_na(HCV_conf, "No"),
         #HBV_confirmatory = replace_na(HBV_conf, "No"),
         HCV_blood_tests = replace_na(HCV_blood_test, "No"),
         HBV_blood_tests = replace_na(HBV_blood_test, "No"),
         HCV_diags = replace_na(HCV_diag, "No"),
         HCV_diags_new = replace_na(HCV_diag_new, "No"),
         HBV_diags = replace_na(HBV_diag, "No"),
         HBV_diags_new = replace_na(HBV_diag_new, "No"))

#create chi squared table and values
#for blood tests
sentinel_bt_tbl <- attendees_sentinel_joined_clean %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, blood_tests) %>%
  tbl_summary(by = "blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()


#ATTENDEES (LONDON SITES)

df_list_ldn = list(attendees_ldn,attendees_NHSn, blood_tests_NHSn) 

attendees_ldn_joined <- df_list_ldn %>% reduce(left_join, by='NHS_NUMBER', na_matches = "never")

#replace na values with 'No'
attendees_ldn_joined_clean <- attendees_ldn_joined %>% 
  mutate(attendees = replace_na(attendee, "No"),
    blood_tests = replace_na(blood_test, "No"))

#create chi squared table and values
#for attendees
ldn_attendees_tbl <- attendees_ldn_joined_clean %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, attendees) %>%
  tbl_summary(by = "attendees", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for blood tests
ldn_bt_tbl <- attendees_ldn_joined_clean %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, blood_tests) %>%
  tbl_summary(by = "blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#ATTENDEES (OUTSIDE LONDON SITES)

df_list_outside = list(attendees_outside, attendees_NHSn, blood_tests_NHSn) 

attendees_outside_joined <- df_list_outside %>% reduce(left_join, by='NHS_NUMBER', na_matches = "never")

#replace na values with 'No'
attendees_outside_joined_clean <- attendees_outside_joined %>% 
  mutate(attendees = replace_na(attendee, "No"),
    blood_tests = replace_na(blood_test, "No"))

#create chi squared table and values
#for attendees
outside_attendees_tbl <- attendees_outside_joined_clean %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, attendees) %>%
  tbl_summary(by = "attendees", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()

#for blood tests
outside_bt_tbl <- attendees_outside_joined_clean %>% 
  select(age_group, STATED_GENDER, ethnic_group, IMD_19, blood_tests) %>%
  tbl_summary(by = "blood_tests", percent = "row", type = list(IMD_19 ~ "categorical")) %>% 
  add_overall(last = TRUE) %>% 
  add_p()





