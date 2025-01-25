if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
if (!require(janitor)) install.packages("janitor")
if (!require(arrow)) install.packages("arrow")
if (!require(clipr)) install.packages("clipr")

####attendances and attendees#####
# load dataset
ECDS_JOIN_PII <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/ECDS_JOIN_PII.parquet")

# create list of included sites
included_sites <-c('RAL26','RXL01','RYJ02','RQM06','RQM01', 'RQM25','RJ611','R1K04','RVR50','RAS01','RQXM1','RF4DG',
                   'RJZ01',	'RAX01',	'R0A02', 'RM326',	'R1HNH',	'RW602', 'R0A66', 'RM318',	'RAPNM',	'R1K01', 'RJZ30', 'RJ253', 'RJ231',	'RF4QH',	'RAL01',	
                   'RXH01', 'E0A3H',	'RJ701',	'RVR05',	'RYJ01',	'RJ122',	'R1H12',	'RKEQ4',	'RRV03',	'RJ224',	'RQM91',	'R1HKH',	'R0A07', 'RM325'
)

# create list of sites in sentinel
sentinel_sites <-c('RYJ02',	'RQM06','RQM01', 'RQM25',	'RJ611',	'RAS01',	'RQXM1',	'RJZ01',	'RAX01',	'R0A02', 'RM326', 'RJZ30',	'R1HNH',	'RW602', 'R0A66', 'RM318',
                   'RJ253',	'RXH01', 'E0A3H',	'RJ701',	'RYJ01',	'RJ122',	'R1H12',	'RQM91',	'R1HKH',	'R0A07', 'RM325'
)

#create list of london sites
london_sites <-c('RAL26','RYJ02','RQM06','RQM01', 'RQM25','RJ611','R1K04','RAS01','RQXM1','RF4DG',
                 'RJZ01',	'RAX01',	'R1HNH',	'RAPNM',	'R1K01', 'RJZ30', 'RJ253', 'RJ231',	'RF4QH',	'RAL01',	'RJ701',	'RYJ01',	'RJ122',	'R1H12',	'RKEQ4',	'RRV03',	'RJ224',	'RQM91',	'R1HKH', 'RVR05'
)

#create list of sites outside of london
outside_sites <-c('RXL01','RVR50',	'R0A02', 'RM326',	'RW602', 'R0A66', 'RM318', 'RXH01', 'E0A3H',	'R0A07', 'RM325'
)

#create list of non-london sites


# filter dataset on included sites
ECDS_JOIN_PII <- ECDS_JOIN_PII %>%
  filter (SITE %in%included_sites)

# format dates
ECDS_JOIN_PII <- ECDS_JOIN_PII %>%
mutate(date = as.Date((ARRIVAL_DATE), format = "%Y-%m-%d"))%>%
  mutate(month = format(date, "%m-%Y"))


#add in age group to ecds_join_pii

ECDS_JOIN_PII <- ECDS_JOIN_PII %>%
  mutate(age_group = case_when(AGE_AT_ARRIVAL <= 24 ~ "15-24",
                               AGE_AT_ARRIVAL >= 25 & AGE_AT_ARRIVAL <= 34 ~ "25-34",
                               AGE_AT_ARRIVAL >= 35 & AGE_AT_ARRIVAL <= 44 ~ "35-44",
                               AGE_AT_ARRIVAL >= 45 & AGE_AT_ARRIVAL <= 54 ~ "45-54",
                               AGE_AT_ARRIVAL >= 55 & AGE_AT_ARRIVAL <= 64 ~ "55-64",
                               AGE_AT_ARRIVAL >= 65 & AGE_AT_ARRIVAL <= 74 ~ "65-74",
                               AGE_AT_ARRIVAL >= 75 & AGE_AT_ARRIVAL <= 84 ~ "75-84",
                               AGE_AT_ARRIVAL >= 85 & AGE_AT_ARRIVAL <= 94 ~ "85-94",
                               AGE_AT_ARRIVAL > 94 ~ "95+")) %>%
  mutate(age_group2 = case_when(AGE_AT_ARRIVAL < 45 ~ "16-44",
                                AGE_AT_ARRIVAL > 44 & AGE_AT_ARRIVAL < 65 ~ "45-64",
                                AGE_AT_ARRIVAL > 64 ~ "65+")) 



# Attendees
#order by ARRIVAL_DATE to pick up first attendance only
ECDS_Ordered <- arrange(ECDS_JOIN_PII, ARRIVAL_DATE)
#dedupe by TOKEN_PERSON_ID
attendees <-  distinct(ECDS_Ordered, TOKEN_PERSON_ID, .keep_all = TRUE)
##1,759,075 distinct TOKEN_PERSON_IDs

#filter for sentinel sites only
attendees_sentinel <- attendees%>%
  filter(SITE%in%sentinel_sites)

#filter for london sites only
attendees_ldn <- attendees %>%
  filter(SITE %in% london_sites)

#filter for outside london sites only
attendees_outside <- attendees %>%
  filter(SITE %in% outside_sites)


write_parquet(attendees, "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendees.parquet" )

#regroup to find total attendees by month
attendees_month <- attendees %>%
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

#double check total attendees figure matches distinct IDs
sum(attendees_month$n)
#1,759,075 distinct attendees

#regroup to find total attendees at sentinel sites by month
attendees_sentinelmonth <- attendees_sentinel %>%
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

write.csv(attendees_sentinelmonth, 'sentinelattendees.csv')


#double check total attendees figure matches distinct IDs
sum(attendees_sentinelmonth$n)
#1,027,328 distinct sentinel attendees

#regroup to find total attendees at London sites by month
attendees_LDNmonth <- attendees_ldn %>%
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

#double check total attendees figure matches distinct IDs
sum(attendees_LDNmonth$n)
#1,484,773 distinct sentinel attendees

#regroup to find total attendees at outside London sites by month
attendees_outsidemonth <- attendees_outside %>%
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

#double check total attendees figure matches distinct IDs
sum(attendees_outsidemonth$n)
#274,302 distinct sentinel attendees



# Attendances
# creates table of attendances - Deduped to 1 PERSON_TOKEN_ID per day - so a person can have multiple attendances
# use this for subsequent attendances analyses
attendances <- ECDS_JOIN_PII %>%
  distinct(TOKEN_PERSON_ID, ARRIVAL_DATE, .keep_all = TRUE)
#2,724,697

#create table of attendances for sentinel sites only
attendances_sentinel <- attendances %>% 
  filter(SITE %in% sentinel_sites)
#1,583,133

#filter for london sites only
attendances_ldn <- attendances %>%
  filter(SITE %in% london_sites)
#2,305,684

#filter for outside london sites only
attendances_outside <- attendances %>%
  filter(SITE %in% outside_sites)
#419,013


#regroup to find attendances per month
attendances_month <- attendances %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

sum(attendances_month$n)
#2,724,697 attendances de-duped by TPID and arrival date

#attendances per month for sentinel sites
attendances_sentinelmonth <- attendances_sentinel %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

sum(attendances_sentinelmonth$n)
#1,583,133 attendances de-duped by TPID and arrival date

write.csv(attendances_sentinelmonth, 'sentinelattendances.csv')

#create table of attendances for London sites only
attendances_LDNmonth <- attendances_ldn %>%
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

#double check total attendances figure matches distinct IDs
sum(attendances_LDNmonth$n)
#2,305,684 distinct London attendances

#create table of attendances for sites outside of London only
attendances_outsidemonth <- attendances_outside %>%
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  select(ARRIVAL_MONTH, n)

#double check total attendees figure matches distinct IDs
sum(attendances_outsidemonth$n)
#419,013 distinct outside of London attendances


# CHECK NO OF REPEAT ATTENDANCES
repeatattendance <- attendances %>%
  group_by(TOKEN_PERSON_ID) %>%
  count()%>%
  select(TOKEN_PERSON_ID, n)

repeatattendance2 <- repeatattendance %>%
  distinct(n) %>%
  group_by(n)%>%
  count()

write.csv(repeatattendance2, 'repeatattendance.csv')


##total attendances by trust

totalattendancesbytrust <- attendances %>%
  group_by(ARRIVAL_MONTH, Provider_derived) %>%
  count() %>%
  select(ARRIVAL_MONTH, Provider_derived, n)

totalattendancesbytrustw <- spread(totalattendancesbytrust, Provider_derived, n)

##attendances by site

attendances_by_site <- attendances %>%
  group_by(ARRIVAL_MONTH, SITE) %>%
  count() %>%
  select(ARRIVAL_MONTH, SITE, n)


attendances_by_site_sentinel <- attendances %>%
  filter (SITE %in%sentinel_sites)%>%
  group_by(ARRIVAL_MONTH, SITE) %>%
  count() %>%
  select(ARRIVAL_MONTH, SITE, n)


attendances_by_site_sentinel<- pivot_wider(attendances_by_site_sentinel, names_from = SITE, values_from = n) %>% write_clip()

write.csv(attendances_by_site_sentinel, "attendances_sentinel.csv")



###demographics for attendees 

##age group

age_attendees_trust <- attendees %>%
  group_by(Provider_derived, age_group) %>%
  count()

#age groups of attendees from all sites
age_attendees_all <- attendees %>%
  group_by(age_group) %>%
  count()

write.csv(age_attendees_all, "agegroups_attendees.csv")

#age groups of attendees from sentinel sites
age_attendees_sentinel <- attendees_sentinel %>%
  group_by(age_group)%>%
  count()

write.csv(age_attendees_sentinel, "agesentinel_attendees.csv")


#age groups of attendees from London sites

age_attendees_london <- attendees_ldn %>%
  group_by(age_group)%>%
  count()

write.csv(age_attendees_london, "ageldn_attendees.csv")

#age groups of attendees from sites outside of London

age_attendees_outside <- attendees_outside %>%
  group_by(age_group)%>%
  count()


write.csv(age_attendees_outside, "ageoutside_attendees.csv")


#ethnic group 

ethnic_group_attendees_trust <- attendees %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

#age groups of attendees from all sites
ethnic_group_attendees_all <- attendees %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(ethnic_group_attendees_all, "ethnicgp_attendees.csv")

#ethnic groups of attendees from Sentinel sites
ethnic_group_attendees_sentinel <- attendees_sentinel %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(ethnic_group_attendees_sentinel, "ethgp_sentinel_attendees.csv")

#ethnic groups of attendees from London sites
ethnic_group_attendees_ldn <- attendees_ldn %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_wide_ldn, "ethgp_ldn_attendees.csv")

#ethnic groups of attendees from sites outside of London 
ethnic_group_attendees_outside <- attendees_outside %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_wide_outside, "ethgp_outside_attendees.csv")


##imd

imd_trust <- attendees %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

#imd for attendees from all sites
imd <- attendees %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd, "imd_attendees.csv")

#imd for attendees from sentinel sites
imd_sentinel <- attendees_sentinel %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_sentinel, "imd_attendees_sentinel.csv")

#imd for attendees from London sites
imd_ldn <- attendees_ldn %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_ldn, "imd_attendees_ldn.csv")

#imd for attendees from sites outside of London
imd_outside <- attendees_outside %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_outside, "imd_attendees_outside.csv")



####blood tests#####

##blood tests in ECDS - keep those who have had blood tests

ECDS_bloods_any <- ECDS_JOIN_PII %>%
  filter(INVESTIGATION_CODE_Description %in% c("Serology", "Urea & Electrolytes (U&Es)",
                                               "Full blood count (FBC)", "Liver function tests (LFTs)"))


#wider definition of blood test
ECDS_bloods_any2 <- ECDS_JOIN_PII %>%
  filter(INVESTIGATION_CODE_Description %in% c("Serology", "Urea & Electrolytes (U&Es)",
                                               "Full blood count (FBC)", "Liver function tests (LFTs)", "Lactate",
                                               "C reactive protein (CRP)", "Clotting studies"))


#####attendees w/ blood tests######

##DEFINITION 1
#check expected no. of attendees w/ blood tests
ECDS_bloods_any %>% distinct(TOKEN_PERSON_ID) %>%
  group_by(TOKEN_PERSON_ID)%>%
  count()
#994,815 attendees w/ blood test

#order by arrival date, then dedupe using token_person_id so we are using the first date of attendance for each attendee
ECDS_BloodsOrdered <- arrange(ECDS_bloods_any, ARRIVAL_DATE)

ECDSBloodsfirstdate <-  distinct(ECDS_BloodsOrdered, TOKEN_PERSON_ID, .keep_all = TRUE)

bloodtestsattendeessentinel <- ECDSBloodsfirstdate %>%
  filter (SITE %in%sentinel_sites)

bloodtestsattendeesldn <- ECDSBloodsfirstdate %>%
  filter (SITE %in%london_sites)

bloodtestsattendeesoutside <- ECDSBloodsfirstdate %>%
  filter (SITE %in%outside_sites)

write_parquet(ECDSBloodsfirstdate, "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendees_blood_tests.parquet")

#attendees w/ blood test by month (all sites)
blood_tests_attendees <- ECDSBloodsfirstdate %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_tests_attendees, "bloods_attendees.csv")

#attendees w/ blood test by month (sentinel sites)
blood_attendees_sentinel <- bloodtestsattendeessentinel %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_attendees_sentinel, "bloodsattendees_sentinel.csv")

#attendees w/ blood test by month (ldn sites)
blood_attendees_ldn <- bloodtestsattendeesldn %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_attendees_ldn, "bloodsattendees_ldn.csv")

#attendees w/ blood test by month (outside sites)
blood_attendees_outside <- bloodtestsattendeesoutside %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_attendees_outside, "bloodsattendees_outside.csv")

#blood tests by trust

blood_tests_trust <- ECDSBloodsfirstdate %>% group_by(Provider_derived) %>% count

blood_tests_month_trust <- ECDSBloodsfirstdate %>% group_by(Provider_derived, ARRIVAL_MONTH) %>% count()

blood_tests_month_trust_wide <- blood_tests_month_trust %>% pivot_wider(names_from = ARRIVAL_MONTH, values_from = n)


##DEFINITION 2

#check expected no. of attendees w/ blood tests
ECDS_bloods_any2 %>% distinct(TOKEN_PERSON_ID) %>%
  group_by(TOKEN_PERSON_ID)%>%
  count()
#1,013,297 attendees w/ blood test def2

#order by arrival date, then dedupe using token_person_id so we are using the first date of attendance for each attendee
ECDS_BloodsOrdered2 <- arrange(ECDS_bloods_any2, ARRIVAL_DATE)

ECDSBloodsfirstdate2 <-  distinct(ECDS_BloodsOrdered2, TOKEN_PERSON_ID, .keep_all = TRUE)

bloodtestsattendeessentinel2 <- ECDSBloodsfirstdate2 %>%
  filter (SITE %in%sentinel_sites)

bloodtestsattendeesldn2 <- ECDSBloodsfirstdate2 %>%
  filter (SITE %in%london_sites)

bloodtestsattendeesoutside2 <- ECDSBloodsfirstdate2 %>%
  filter (SITE %in%outside_sites)


#attendees w/ blood test by month def2 (all sites)
blood_tests_attendees2 <- ECDSBloodsfirstdate2 %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_tests_attendees2, "bloodsattendees2.csv")

#attendees w/ blood test by month (sentinel sites)
blood_attendees_sentinel2 <- bloodtestsattendeessentinel2 %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_attendees_sentinel2, "bloodsattendees_sentinel2.csv")

#attendees w/ blood test by month (ldn sites)
blood_attendees_ldn2 <- bloodtestsattendeesldn2 %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_attendees_ldn, "bloodsattendees_ldn2.csv")

#attendees w/ blood test by month (outside sites)
blood_attendees_outside2 <- bloodtestsattendeesoutside2 %>%
  group_by(ARRIVAL_MONTH)%>%
  summarise(totalbloodtests = n())

write.csv(blood_attendees_outside, "bloodsattendees_outside2.csv")


##look at demographics of both definitions

##DEFINITION 1
##age group 
age_blood_tests_all <- ECDSBloodsfirstdate %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, age_group) %>%
  group_by(age_group) %>%
  count()

write.csv(age_blood_tests_all, "age_bloodtests.csv")

##age group - sentinel sites
age_blood_tests_sentinel <- bloodtestsattendeessentinel %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, age_group) %>%
  group_by(age_group) %>%
  count()

write.csv(age_blood_tests_sentinel, "age_bloodtests_sentinel.csv")

##age group - london sites
age_blood_tests_ldn <- bloodtestsattendeesldn %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, age_group) %>%
  group_by(age_group) %>%
  count()

write.csv(age_blood_tests_ldn, "age_bloodtests_ldn.csv")

##age group - sentinel sites
age_blood_tests_outside <- bloodtestsattendeesoutside %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, age_group) %>%
  group_by(age_group) %>%
  count()

write.csv(age_blood_tests_outside, "age_bloodtests_sentinel.csv")



##ethnic groups

eth_blood_tests_all <- ECDSBloodsfirstdate %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ETHNIC_CATEGORY_Description) %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_blood_tests_all, "ethnicgrp_bloodtests.csv")

##ethnic groups - sentinel

eth_blood_tests_sentinel <- bloodtestsattendeessentinel %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ETHNIC_CATEGORY_Description) %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_blood_tests_sentinel, "ethnicgrp_bloodtests_sentinel.csv")

##ethnic groups - london

eth_blood_tests_ldn <- bloodtestsattendeesldn %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ETHNIC_CATEGORY_Description) %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_blood_tests_ldn, "ethnicgrp_bloodtests_ldn.csv")

##ethnic groups - outside of london

eth_blood_tests_outside <- bloodtestsattendeesoutside %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ETHNIC_CATEGORY_Description) %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_blood_tests_outside, "ethnicgrp_bloodtests_outside.csv")


##IMd

imd_blood_tests_all <- ECDSBloodsfirstdate %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, IMD_19) %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_blood_tests_all, "imdbloodtests.csv")

##imd - sentinel sites
imd_blood_tests_sentinel <- bloodtestsattendeessentinel %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, IMD_19) %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_blood_tests_sentinel, "imdbloodtests_sentinel.csv")

##imd - london sites
imd_blood_tests_ldn <- bloodtestsattendeesldn %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, IMD_19) %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_blood_tests_ldn, "imdbloodtests_ldn.csv")

##imd - outside of london sites
imd_blood_tests_outside <- bloodtestsattendeesoutside %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, IMD_19) %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_blood_tests_outside, "imdbloodtests_outside.csv")


##DEFINITION 2
##age group 
age_blood_tests_all2 <- ECDSBloodsfirstdate2 %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, age_group) %>%
  group_by(age_group) %>%
  count()

write.csv(age_blood_tests_all2, "age_bloodtests2.csv")


##ethnic group

eth_blood_tests_all2 <- ECDSBloodsfirstdate2 %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ETHNIC_CATEGORY_Description) %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_blood_tests_all2, "ethnicgrp_bloodtests2.csv")


##IMd

imd_blood_tests_all2 <- ECDSBloodsfirstdate2 %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, IMD_19) %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_blood_tests_all2, "imdbloodtests2.csv")


#####attendances w/ blood tests######

#DEFINITION 1
#check expected no. of attendances w/ blood tests
expectedattendances <- ECDS_bloods_any %>% distinct(TOKEN_PERSON_ID, ARRIVAL_DATE) %>%
  group_by(ARRIVAL_DATE)%>%
  count()

sum(expectedattendances$n)
#1,420,672 attendances w/ blood test


# create table of attendances with blood tests - use this for subsequent blood test attendance analyses
attendancesbloodtests <- ECDS_bloods_any%>%
  distinct(TOKEN_PERSON_ID, ARRIVAL_DATE, .keep_all = TRUE)

attendancesbloodtests_sentinel <- attendancesbloodtests %>% filter(SITE %in% sentinel_sites)

attendancesbloodtests2 <- ECDS_bloods_any2 %>%
  distinct(TOKEN_PERSON_ID, ARRIVAL_DATE, .keep_all = TRUE)

attendancesbloodtests_sentinel2 <- attendancesbloodtests2 %>% filter(SITE %in% sentinel_sites)


#attendances w/ blood test by month

blood_tests_attendances_site <- attendancesbloodtests %>%
  group_by(SITE, ARRIVAL_MONTH)%>%
count() %>%
  select(ARRIVAL_MONTH, SITE, n)

blood_tests_attendances_sitew <- pivot_wider(blood_tests_attendances_site, names_from = SITE, values_from = n) %>% write_clip()

blood_tests_attendances <- attendancesbloodtests %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(total_btattendancesmonth = sum(n))

# sentinel sites only
blood_tests_attendances_sentinel <- attendancesbloodtests %>%
  filter(SITE%in%sentinel_sites)%>%
  group_by(SITE, ARRIVAL_MONTH)%>%
  count() %>%
  select(ARRIVAL_MONTH, SITE, n)

blood_tests_attendances_sentinel <- pivot_wider(blood_tests_attendances_sentinel, names_from = SITE, values_from = n) %>% write_clip()


#double check numbers match expected no.
sum(blood_tests_attendances$total_btattendancesmonth)
#1,602,258 attendances w/ blood test

#blood tests by trust

blood_tests_trust_attendances <- attendancesbloodtests2 %>% group_by(Provider_derived) %>% count

blood_tests_month_trust_attendances <- attendancesbloodtests2 %>% group_by(Provider_derived, ARRIVAL_MONTH) %>% count()

blood_tests_month_trust_wide_attendances <- blood_tests_month_trust_attendances %>% pivot_wider(names_from = ARRIVAL_MONTH, values_from = n)

##DEFINITION 2

#check expected no. of attendances w/ blood tests
expectedattendances2 <- ECDS_bloods_any2 %>% 
  distinct(TOKEN_PERSON_ID, ARRIVAL_DATE) %>%
  group_by(ARRIVAL_DATE)%>%
  count()

sum(expectedattendances2$n)
#1452282 attendances w/ blood test def2

#attendances w/ blood test by month def2

blood_tests_person_attendances2 <- attendancesbloodtests2 %>%
  distinct(TOKEN_PERSON_ID, ARRIVAL_DATE) %>%
  group_by(ARRIVAL_DATE) %>%
  count() %>%
  select(ARRIVAL_DATE, n)%>%
  mutate(date = as.Date((ARRIVAL_DATE), format = "%Y-%m-%d"))%>%
  mutate(month = format(date, "%m"))

blood_tests_attendances2 <- blood_tests_person_attendances2 %>% 
  group_by(month)%>%
  summarise(total_btattendancesmonth = sum(n))

#double check numbers match expected no.
sum(blood_tests_attendances2$total_btattendancesmonth)
#1,452,282 attendances w/ blood test def2


################ demographics for attendance, attendee and blood tests with HCV tests ###############################


#####demographics for attendees with HCV tests 

#check expected no. of attendees w/ HCV tests
included_tests_HCV4 %>% count()
#58,415 attendees w/ HCV test

HCV_tests_attendees_month <- included_tests_HCV4 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendees = sum(n))

attendees_HCV <- included_tests_HCV4

##age group

age_HCV_attendees_trust <- attendees_HCV%>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HCV_attendees <- attendees_HCV%>%
  group_by(age_group) %>%
  count()

write.csv(age_HCV_attendees, "agegroups_HCVattendees.csv")


##ethnic group 

ethnic_group_HCV_attendees_trust <- attendees_HCV%>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HCV_attendees <- attendees_HCV%>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(ethnic_group_HCV_attendees, "ethnicgroups_HCVattendees.csv")

##imd

imd_HCV_trust <- attendees_HCV%>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HCV <- attendees_HCV%>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_HCV, "imd_HCVattendees.csv")


#####demographics for attendances with HCV tests

HCV_tests_attendances_month <- included_tests_HCV3 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(total_HCVattendancesmonth = sum(n))

attendances_HCV <- included_tests_HCV3

#attendances w/ HCV test by month

HCV_tests_attendances_month <- attendances_HCV %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(total_HCVattendancesmonth = sum(n))

##age group 

age_HCV_tests_attendance <- attendances_HCV %>%
  group_by(age_group) %>%
  count()

write.csv(age_HCV_tests_attendance, "age_HCVattendance.csv")


##ethnic group

eth_HCV_tests_attendance <- attendances_HCV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_HCV_tests_attendance, "ethnicgrp_HCVattendance.csv")


##IMd


imd_HCV_tests_attendance <- attendances_HCV %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_HCV_tests_attendance, "imd_HCVattendance.csv")


#####demographics for attendees who had blood tests and HCV tests

HCV_blood_attendees_month <- included_tests_HCV5 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(total_HCVattendeesmonth = sum(n))

attendees_blood_HCV <- included_tests_HCV5

##age group

age_HCV_blood_attendees_trust <- attendees_blood_HCV %>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HCV_blood_attendees <- attendees_blood_HCV %>%
  group_by(age_group) %>%
  count()

##ethnic group 

ethnic_group_HCV_blood_attendees_trust <- attendees_blood_HCV %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HCV_blood_attendees <- attendees_blood_HCV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

##imd

imd_HCV_blood_trust_attendees <- attendees_blood_HCV %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HCV_blood_attendees <- attendees_blood_HCV %>%
  group_by(IMD_19) %>%
  count()


#####demographics for attendances who had blood tests and HCV tests 

HCV_blood_attendances_month <- included_tests_HCV %>%
  group_by(ARRIVAL_MONTH) %>%
  count()%>%
  summarise(totalattendances = sum(n))

attendances_blood_HCV <- included_tests_HCV

##age group

age_HCV_blood_attendances_trust <- attendances_blood_HCV %>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HCV_blood_attendances <- attendances_blood_HCV %>%
  group_by(age_group) %>%
  count()

##ethnic group 

ethnic_group_HCV_blood_attendances_trust <- attendances_blood_HCV %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HCV_blood_attendances <- attendances_blood_HCV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

##imd

imd_HCV_blood_attendances_trust <- attendances_blood_HCV %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HCV_blood_attendances <- attendances_blood_HCV %>%
  group_by(IMD_19) %>%
  count()


################ demographics for attendance, attendee and blood tests with HBV tests ###############################


#####demographics for attendees with HBV tests 

HBV_tests_attendees_month <- included_tests_HBV4 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendees = sum(n))

#check expected no. of attendees w/ HBV tests
included_tests_HBV4 %>% count()
#36,440 attendees w/ HBV test

attendees_HBV <- included_tests_HBV4

##age group

age_HBV_attendees_trust <- attendees_HBV%>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HBV_attendees <- attendees_HBV%>%
  group_by(age_group) %>%
  count()

write.csv(age_HBV_attendees, "agegroups_HBVattendees.csv")


##ethnic group 

ethnic_group_HBV_attendees_trust <- attendees_HBV%>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HBV_attendees <- attendees_HBV%>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(ethnic_group_HBV_attendees, "ethnicgroups_HBVattendees.csv")

##imd

imd_HBV_trust <- attendees_HBV%>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HBV <- attendees_HBV%>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_HBV, "imd_HBVattendees.csv")


#####demographics for attendances with HBV tests

#attendances w/ HBV test by month

HBV_tests_attendances_month <- included_tests_HBV3 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendances = sum(n))

attendances_HBV <- included_tests_HBV3

##age group 

age_HBV_tests_attendance <- attendances_HBV %>%
  group_by(age_group) %>%
  count()

write.csv(age_HBV_tests_attendance, "age_HBVattendance.csv")


##ethnic group

eth_HBV_tests_attendance <- attendances_HBV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_HBV_tests_attendance, "ethnicgrp_HBVattendance.csv")


##IMd


imd_HBV_tests_attendance <- attendances_HBV %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_HBV_tests_attendance, "imd_HBVattendance.csv")


#####demographics for attendees who had blood tests and HBV tests


HBV_blood_attendees_month <- included_tests_HBV5 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendees = sum(n))

attendees_blood_HBV <- included_tests_HBV5

##age group

age_HBV_blood_attendees_trust <- attendees_blood_HBV %>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HBV_blood_attendees <- attendees_blood_HBV %>%
  group_by(age_group) %>%
  count()

##ethnic group 

ethnic_group_HBV_blood_attendees_trust <- attendees_blood_HBV %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HBV_blood_attendees <- attendees_blood_HBV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

##imd

imd_HBV_blood_trust_attendees <- attendees_blood_HBV %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HBV_blood_attendees <- attendees_blood_HBV %>%
  group_by(IMD_19) %>%
  count()


#####demographics for attendances who had blood tests and HBV tests 

HBV_blood_attendances_month <- included_tests_HBV %>%
  group_by(ARRIVAL_MONTH) %>%
  count()%>%
  summarise(totalattendances = sum(n))

attendances_blood_HBV <- included_tests_HBV

##age group

age_HBV_blood_attendances_trust <- attendances_blood_HBV %>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HBV_blood_attendances <- attendances_blood_HBV %>%
  group_by(age_group) %>%
  count()

##ethnic group 

ethnic_group_HBV_blood_attendances_trust <- attendances_blood_HBV %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HBV_blood_attendances <- attendances_blood_HBV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

##imd

imd_HBV_blood_attendances_trust <- attendances_blood_HBV %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HBV_blood_attendances <- attendances_blood_HBV %>%
  group_by(IMD_19) %>%
  count()


################ demographics for attendance, attendee and blood tests with HIV tests ###############################


#####demographics for attendees with HIV tests 

HIV_tests_attendees_month <- included_tests_HIV4 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendees = sum(n))

#check expected no. of attendees w/ HIV tests
included_tests_HIV4 %>% count()
#95,438 attendees w/ HIV test

attendees_HIV <- included_tests_HIV4

##age group

age_HIV_attendees_trust <- attendees_HIV%>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HIV_attendees <- attendees_HIV%>%
  group_by(age_group) %>%
  count()

write.csv(age_HIV_attendees, "agegroups_HIVattendees.csv")


##ethnic group 

ethnic_group_HIV_attendees_trust <- attendees_HIV%>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HIV_attendees <- attendees_HIV%>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(ethnic_group_HIV_attendees, "ethnicgroups_HIVattendees.csv")

##imd

imd_HIV_trust <- attendees_HIV%>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HIV <- attendees_HIV%>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_HIV, "imd_HIVattendees.csv")


#####demographics for attendances with HIV tests

#attendances w/ HIV test by month

HIV_tests_attendances_month <- included_tests_HIV3 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendances = sum(n))

attendances_HIV <- included_tests_HIV3

##age group 

age_HIV_tests_attendance <- attendances_HIV %>%
  group_by(age_group) %>%
  count()

write.csv(age_HIV_tests_attendance, "age_HIVattendance.csv")


##ethnic group

eth_HIV_tests_attendance <- attendances_HIV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

write.csv(eth_HIV_tests_attendance, "ethnicgrp_HIVattendance.csv")


##IMd


imd_HIV_tests_attendance <- attendances_HIV %>%
  group_by(IMD_19) %>%
  count()

write.csv(imd_HIV_tests_attendance, "imd_HIVattendance.csv")


#####demographics for attendees who had blood tests and HIV tests

HIV_blood_attendees_month <- included_tests_HIV5 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendees = sum(n))

attendees_blood_HIV <- included_tests_HIV5

##age group

age_HIV_blood_attendees_trust <- attendees_blood_HIV %>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HIV_blood_attendees <- attendees_blood_HIV %>%
  group_by(age_group) %>%
  count()

##ethnic group 

ethnic_group_HIV_blood_attendees_trust <- attendees_blood_HIV %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HIV_blood_attendees <- attendees_blood_HIV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

##imd

imd_HIV_blood_trust_attendees <- attendees_blood_HIV %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HIV_blood_attendees <- attendees_blood_HIV %>%
  group_by(IMD_19) %>%
  count()


#####demographics for attendances who had blood tests and HIV tests 

HIV_BLOOD_attendances_month <- included_tests_HIV %>%
  group_by(ARRIVAL_MONTH) %>%
  count()%>%
  summarise(totalattendances = sum(n))

attendances_blood_HIV <- included_tests_HIV

##age group

age_HIV_blood_attendances_trust <- attendances_blood_HIV %>%
  group_by(Provider_derived, age_group) %>%
  count()

age_HIV_blood_attendances <- attendances_blood_HIV %>%
  group_by(age_group) %>%
  count()

##ethnic group 

ethnic_group_HIV_blood_attendances_trust <- attendances_blood_HIV %>%
  group_by(Provider_derived, ETHNIC_CATEGORY_Description) %>%
  count()

ethnic_group_HIV_blood_attendances <- attendances_blood_HIV %>%
  group_by(ETHNIC_CATEGORY_Description) %>%
  count()

##imd

imd_HIV_blood_attendances_trust <- attendances_blood_HIV %>%
  group_by(Provider_derived, IMD_19) %>%
  count()

imd_HIV_blood_attendances <- attendances_blood_HIV %>%
  group_by(IMD_19) %>%
  count()

