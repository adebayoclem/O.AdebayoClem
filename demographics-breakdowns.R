### all HCV diagnoses (unlinked)
## anti-HCV positives (also include RNA positives so can be a true denominator of RNA positives)
age <- hcv_pos_joined_2%>%
  filter(antiHCV =='positive' | HCVRNA =='positive')%>%
  group_by(agegroup) %>%
  count() %>%
  select(agegroup, n)

write.csv(age, "anti_hcvpos_age.csv")

ethnicity <- hcv_pos_joined_2%>%
  filter(antiHCV =='positive'| HCVRNA =='positive')%>%
  group_by(CombinedEthnicity) %>%
  count() %>%
  select(CombinedEthnicity, n)

write.csv(ethnicity, "anti_hcvpos_ethnicity.csv")

gender <- hcv_pos_joined_2%>%
  filter(antiHCV =='positive'| HCVRNA =='positive')%>%
  group_by(sex) %>%
  count() %>%
  select(sex, n)

write.csv(gender, "anti_hcvpos_sex.csv")

imd <- hcv_pos_joined_2%>%
  filter(antiHCV =='positive'| HCVRNA =='positive')%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)

write.csv(imd, "anti_hcvpos_imd.csv")

### Had RNA test

age <- hcv_pos_joined_2%>%
  filter(!is.na(HCVRNAdate))%>%
  group_by(agegroup) %>%
  count() %>%
  select(agegroup, n)

check_2 <- rbind(age, attendees_ethnic)

age <- age%>%
  rename('group' = 'age_group')

attendees_ethnic <- attendees_ethnic%>%
  rename('group' = 'ethnic_group')

write.csv(age, "rna_test_age.csv")

ethnicity <- hcv_pos_joined_2%>%
  filter(!is.na(HCVRNAdate))%>%
  group_by(CombinedEthnicity) %>%
  count() %>%
  select(CombinedEthnicity, n)

write.csv(ethnicity, "rna_test_ethnicity.csv")

gender <- hcv_pos_joined_2%>%
  filter(!is.na(HCVRNAdate))%>%
  group_by(sex) %>%
  count() %>%
  select(sex, n)

write.csv(gender, "rna_test_sex.csv")

imd <- hcv_pos_joined_2%>%
  filter(!is.na(HCVRNAdate))%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)

write.csv(imd, "rna_test_imd.csv")

### reflex testing
age <- hcv_pos_joined_2%>%
  filter(RNAtiming =='Reflex')%>%
  group_by(agegroup) %>%
  count() %>%
  select(agegroup, n)

write.csv(age, "reflex_test_age.csv")

ethnicity <- hcv_pos_joined_2%>%
  filter(RNAtiming =='Reflex')%>%
  group_by(CombinedEthnicity) %>%
  count() %>%
  select(CombinedEthnicity, n)

write.csv(ethnicity, "reflex_test_ethnicity.csv")

gender <- hcv_pos_joined_2%>%
  filter(RNAtiming =='Reflex')%>%
  group_by(sex) %>%
  count() %>%
  select(sex, n)

write.csv(gender, "reflex_test_sex.csv")

imd <- hcv_pos_joined_2%>%
  filter(RNAtiming =='Reflex')%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)

write.csv(imd, "reflex_test_imd.csv")

### RNA positives ##########
age <- hcv_pos_joined_2%>%
  filter(HCVRNA =='positive')%>%
  group_by(agegroup) %>%
  count() %>%
  select(agegroup, n)

write.csv(age, "rna_pos_age.csv")

ethnicity <- hcv_pos_joined_2%>%
  filter(HCVRNA =='positive')%>%
  group_by(CombinedEthnicity) %>%
  count() %>%
  select(CombinedEthnicity, n)

write.csv(ethnicity, "rna_pos_ethnicity.csv")

gender <- hcv_pos_joined_2%>%
  filter(HCVRNA =='positive')%>%
  group_by(sex) %>%
  count() %>%
  select(sex, n)

write.csv(gender, "rna_pos_sex.csv")

imd <- hcv_pos_joined_2%>%
  filter(HCVRNA =='positive')%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)

write.csv(imd, "rna_pos_imd.csv")

#### new rna positives
age <- hcv_pos_joined_2%>%
  filter(HCV_diag_new =='Yes')%>%
  group_by(agegroup) %>%
  count() %>%
  select(agegroup, n)

write.csv(age, "rna_new_age.csv")

imd <- HCV_included_diagnoses_sentinel%>%
  filter(HCVRNA =='positive')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)

write.csv(ethnicity, "rna-linked_ethnicity.csv")

gender <- hcv_pos_joined_2%>%
  filter(HCV_diag_new =='Yes')%>%
  group_by(sex) %>%
  count() %>%
  select(sex, n)

write.csv(gender, "rna_new_sex.csv")

imd <- hcv_pos_joined_2%>%
  filter(HCV_diag_new =='Yes')%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)

write.csv(imd, "rna_new_imd.csv")

#### reinfections
age <- hcv_pos_joined_2%>%
  filter(preRxSVR =='Yes')%>%
  group_by(agegroup) %>%
  count() %>%
  select(agegroup, n)

check <- hcv_pos_joined_2%>%
  filter(preRxSVR =='Yes'& HCVRNA =='positive')

check4<- HCV_DIAG_SENTINEL%>%
  filter(HCV_linked28 == 'Yes' & HCVRNA == 'positive')%>%
  filter(live_HCV == 'LIVE')



attendances_sites <- attendances%>%
  filter(!is.na(NHS_NUMBER))%>%
  group_by(SITE)%>%
  count()%>%
  select(SITE, n)

#### attendances breakdowns #########
attendances_age <- attendances%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(attendances_age, "attendances_age_5_sites.csv")

attendancesbt_age <- attendancesbloodtests%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(attendancesbt_age, "attendances_bt_age_5_sites.csv")

attendances_gender <- attendances%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(attendances_gender, "attendances_gender_5_sites.csv")

attendancesbt_gender <- attendancesbloodtests%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(attendancesbt_gender, "attendances_bt_gender_5_sites.csv")

attendances_ethnic <- attendances%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(attendances_ethnic, "attendances_ethnic_5_sites.csv")

attendancesbt_ethnic <- attendancesbloodtests%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(attendancesbt_ethnic, "attendances_bt_ethnic_5_sites.csv")

attendances_IMD <- attendances%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(attendances_IMD, "attendances_IMD_5_sites.csv")

attendancesbt_IMD <- attendancesbloodtests%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(attendancesbt_IMD, "attendances_bt_IMD_5_sites.csv")

#### attendees breakdowns #########
attendees_age <- attendees%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(attendees_age, "hbv_attendees_age_5_sites.csv")

attendeesbt_age <- ECDSBloodsfirstdate%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(attendeesbt_age, "hbv_attendees_bt_age_5_sites.csv")

attendees_gender <- attendees%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(attendees_gender, "hbv-attendees_gender_5_sites.csv")

attendeesbt_gender <- ECDSBloodsfirstdate%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(attendeesbt_gender, "hbv-attendees_bt_gender_5_sites.csv")

attendees_ethnic <- attendees%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(attendees_ethnic, "hbv_attendees_ethnic_5_sites.csv")

attendeesbt_ethnic <- ECDSBloodsfirstdate%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(attendeesbt_ethnic, "hbv-attendees_bt_ethnic_5_sites.csv")

attendees_IMD <- attendees%>%
  filter(live_HCV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(attendees_IMD, "hcv_attendees_IMD_5_sites.csv")

attendeesbt_IMD <- ECDSBloodsfirstdate%>%
  filter(live_HCV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(attendeesbt_IMD, "hcv-attendees_bt_IMD_5_sites.csv")

#### attendees with HIV tests breakdowns #########
####### HIV tests for attendees (with blood tests)
# included_tests_HIV5 is attendees with blood tests who had hiv tests

hiv_tests_age <-included_tests_HIV5 %>%
  filter(live_HIV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(hiv_tests_age, "hiv_tests_attendeesbt_age_5_sites.csv")


HIV_tests_gender <-included_tests_HIV5 %>%
  filter(live_HIV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HIV_tests_gender, "hiv_tests_attendeesbt_gender_5_sites.csv")

hiv_tests_ethnic <- included_tests_HIV5%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(hiv_tests_ethnic, "hiv_tests__attendeesbt_ethnic_5_sites.csv")


hiv_tests_IMD <- included_tests_HIV5%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(hiv_tests_IMD, "hiv_tests_attendeesbt_IMD_5_sites.csv")

# attendeesbt_IMD <- ECDSBloodsfirstdate%>%
#   filter(live_HIV == 'LIVE')%>%
#   group_by(IMD_19) %>%
#   count() %>%
#   select(IMD_19, n)
# write.csv(attendeesbt_IMD, "attendees_bt_IMD_5_sites.csv")
# 
# 
# attendees_hiv_month <- attendees%>%
#   filter(live_HIV == 'LIVE')%>%
#   group_by(month)%>%
#   count()%>%
# select(month, n)
# 
# write.csv(attendees_hiv_month, "attendeesmont_hiv_go_live_dates.csv")

## repeat for HCV
# included_tests_HCV5 is attendees with blood tests who had hcv tests

hcv_tests_age <-included_tests_HCV5 %>%
  filter(live_HCV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(hcv_tests_age, "hcv_tests_attendeesbt_age_5_sites.csv")


HCV_tests_gender <-included_tests_HCV5 %>%
  filter(live_HCV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HCV_tests_gender, "hcv_tests_attendeesbt_gender_5_sites.csv")

hcv_tests_ethnic <- included_tests_HCV5%>%
  filter(live_HCV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(hcv_tests_ethnic, "hcv_tests__attendeesbt_ethnic_5_sites.csv")


hcv_tests_IMD <- included_tests_HCV5%>%
  filter(live_HCV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(hcv_tests_IMD, "hcv_tests_attendeesbt_IMD_5_sites.csv")


## repeat for HBV
# included_tests_HBV5 is attendees with blood tests who had hbv tests

hbv_tests_age <-included_tests_HBV5 %>%
  filter(live_HBV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(hbv_tests_age, "hbv_tests_attendeesbt_age_5_sites.csv")


HBV_tests_gender <-included_tests_HBV5 %>%
  filter(live_HBV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HBV_tests_gender, "hbv_tests_attendeesbt_gender_5_sites.csv")

hbv_tests_ethnic <- included_tests_HBV5%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(hbv_tests_ethnic, "hbv_tests__attendeesbt_ethnic_5_sites.csv")


hbv_tests_IMD <- included_tests_HBV5%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(hbv_tests_IMD, "hbv_tests_attendeesbt_IMD_5_sites.csv")

#### HIV tests all attendees
# included_tests_HIV4 is attendees who had hiv tests regardless of whether they had blood tests

hiv_tests_age <-included_tests_HIV4 %>%
  filter(live_HIV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(hiv_tests_age, "hiv_tests_attendeesall_age_5_sites.csv")


HIV_tests_gender <-included_tests_HIV4 %>%
  filter(live_HIV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HIV_tests_gender, "hiv_tests_attendeesall_gender_5_sites.csv")

hiv_tests_ethnic <- included_tests_HIV4%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(hiv_tests_ethnic, "hiv_tests__attendeesall_ethnic_5_sites.csv")


hiv_tests_IMD <- included_tests_HIV4%>%
  filter(live_HIV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(hiv_tests_IMD, "hiv_tests_attendeesall_IMD_5_sites.csv")

## repeat for HCV
# included_tests_HCV4 is attendees who had hcv tests, regardless of whether they had blood tests

hcv_tests_age <-included_tests_HCV4 %>%
  filter(live_HCV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(hcv_tests_age, "hcv_tests_attendeesall_age_5_sites.csv")


HCV_tests_gender <-included_tests_HCV4 %>%
  filter(live_HCV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HCV_tests_gender, "hcv_tests_attendeesall_gender_5_sites.csv")

hcv_tests_ethnic <- included_tests_HCV4%>%
  filter(live_HCV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(hcv_tests_ethnic, "hcv_tests__attendeesall_ethnic_5_sites.csv")


hcv_tests_IMD <- included_tests_HCV4%>%
  filter(live_HCV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(hcv_tests_IMD, "hcv_tests_attendeesall_IMD_5_sites.csv")

## repeat for HBV
# included_tests_HBV4 is attendees who had hbv tests, regardless of whether they had blood tests

hbv_tests_age <-included_tests_HBV4 %>%
  filter(live_HBV == 'LIVE')%>%
  group_by(age_group) %>%
  count() %>%
  select(age_group, n)
write.csv(hbv_tests_age, "hbv_tests_attendeesall_age_5_sites.csv")


HBV_tests_gender <-included_tests_HBV4 %>%
  filter(live_HBV == 'LIVE')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HBV_tests_gender, "hbv_tests_attendeesall_gender_5_sites.csv")

hbv_tests_ethnic <- included_tests_HBV4%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(hbv_tests_ethnic, "hbv_tests__attendeesall_ethnic_5_sites.csv")


hbv_tests_IMD <- included_tests_HBV4%>%
  filter(live_HBV == 'LIVE')%>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(hbv_tests_IMD, "hbv_tests_attendeesall_IMD_5_sites.csv")






attendees_hbv <- attendeesbloodtests_5_sites%>%
  filter(live_HBV =='LIVE')

attendees_hbv_age <- attendees_hbv%>%
  group_by(age_group)%>%
  count()%>%
  select(age_group, n)

write.csv(attendees_hbv_age, "attendeesbt_hbv_go_live_dates.csv")

HBV_attendees_gender <-attendees_hbv %>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(HBV_attendees_gender, "hbv_attendeesbt_gender_5_sites.csv")

HBV_attendees_ethnic <-attendees_hbv %>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(HBV_attendees_ethnic, "hbv_attendeesbt_ethnic_5_sites.csv")

HBV_attendees_IMD <-attendees_hbv %>%
  group_by(IMD_19) %>%
  count() %>%
  select(IMD_19, n)
write.csv(HBV_attendees_IMD, "hbv_attendeesbt_IMD_5_sites.csv")


gc()

check<- attendees_joined_clean%>%
  filter(HCV_diags_new =="Yes")

tests_sentinel <- read_parquet ("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendees_joined_5_sites.parquet")

check <- HBV_included_diagnoses_sentinel%>%
  filter(HBsAg =='Positive')
     

check <- tests_sentinel%>%
  filter(HBV_diags == 'Yes')
  
### groupings for HBV positives
new_positive_hbv_age <- tests_sentinel%>%
  filter(HBV_diags_new =='Yes')%>%
  group_by(age_group)%>%
  count()%>%
  select(age_group, n)

write.csv(new_positive_hbv_age, "new_positive_hbv_age.csv")

new_HBV_positive_gender <-tests_sentinel%>%
  filter(HBV_diags_new =='Yes')%>%
  group_by(STATED_GENDER) %>%
  count() %>%
  select(STATED_GENDER, n)
write.csv(new_HBV_positive_gender, "new_positive_hbv_gender_5_sites.csv")

new_HBV_positive_ethnic <-tests_sentinel%>%
  filter(HBV_diags_new =='Yes')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)
write.csv(new_HBV_positive_ethnic, "new_positive_hbv_ethnic_5_sites.csv")

new_HBV_positive_IMD <-HBV_included_diagnoses_sentinel%>%
  filter(Newdiag =='Yes')%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)
write.csv(new_HBV_positive_IMD, "new_positive_hbv_IMD_5_sites.csv")

### groupings for HBV all diagnoses (unlinked) ####
all_positive_hbv_age <- HBV_DIAGNOSES_SENTINEL%>%
  group_by(agegroup)%>%
  count()%>%
  select(agegroup, n)

write.csv(all_positive_hbv_age, "all_positive_hbv_age.csv")

all_HBV_positive_gender <-HBV_DIAGNOSES_SENTINEL%>%
  group_by(sex) %>%
  count() %>%
  select(sex, n)
write.csv(all_HBV_positive_gender, "all_positive_hbv_gender.csv")

all_HBV_positive_ethnic <-HBV_DIAGNOSES_SENTINEL%>%
  group_by(CombinedEthnicity) %>%
  count() %>%
  select(CombinedEthnicity, n)
write.csv(all_HBV_positive_ethnic, "all_positive_hbv_ethnic.csv")

all_HBV_positive_IMD <-HBV_DIAGNOSES_SENTINEL%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)
write.csv(all_HBV_positive_IMD, "all_positive_hbv_IMD.csv")

### groupings for HBV new diagnoses (all - unlinked) ####
new_all_positive_hbv_age <- HBV_DIAGNOSES_SENTINEL%>%
  filter(Newdiag == 'Yes')%>%
  group_by(agegroup)%>%
  count()%>%
  select(agegroup, n)

write.csv(new_all_positive_hbv_age, "new_all_positive_hbv_age.csv")

new_all_HBV_positive_gender <-HBV_DIAGNOSES_SENTINEL%>%
  group_by(sex) %>%
  filter(Newdiag == 'Yes')%>%
  count() %>%
  select(sex, n)
write.csv(new_all_HBV_positive_gender, "new_all_positive_hbv_gender.csv")

new_all_HBV_positive_ethnic <-HBV_DIAGNOSES_SENTINEL%>%
  filter(Newdiag == 'Yes')%>%
  group_by(CombinedEthnicity) %>%
  count() %>%
  select(CombinedEthnicity, n)
write.csv(new_all_HBV_positive_ethnic, "new_all_positive_hbv_ethnic.csv")

new_all_HBV_positive_IMD <-HBV_DIAGNOSES_SENTINEL%>%
  filter(Newdiag == 'Yes')%>%
  group_by(IMD) %>%
  count() %>%
  select(IMD, n)
write.csv(new_all_HBV_positive_IMD, "new_all_positive_hbv_IMD.csv")


#### linked HCV positives

anti_HCV_age <-HCV_included_diagnoses_sentinel%>%
  filter(antiHCV =='positive' | HCVRNA =='positive')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)

write.csv(anti_HCV_age, "anti_hcv_pos_ethnic_linked.csv")

anti_HCV_age <-attendees_joined_clean_2%>%
  filter(HCV_diags_new == 'Yes')%>%
  group_by(ethnic_group) %>%
  count() %>%
  select(ethnic_group, n)

write.csv(anti_HCV_age, "rna_new_ethnic_linked.csv")

linked <-attendees_joined_clean_2

check <- HCV_DIAG_JOINED%>%
  filter(HCVRNA == 'positive' & !is.na(postRxdate))


example <- HCV_DIAG_JOINED%>%
  group_by(ethnic_group)%>%
  mutate(ethnic_total = sum(Number.of.Admissions)) %>% 
  ungroup()

for (i in colnames(df)){
  print(mean(df[[i]]))
}


imdcheck <- HCV_included_diagnoses_sentinel%>%
  select(IMD_19, IMD)

write.csv(imdcheck, "imdcheck.csv")
