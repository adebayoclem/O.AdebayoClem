if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
if (!require(janitor)) install.packages("janitor")
if (!require(arrow)) install.packages("arrow")
if (!require(clipr)) install.packages("clipr")

# load data files needed

source("Attendances_blood_tests_setup.R")

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
  group_by(Provider_derived, ethnic_group) %>%
  count()

#age groups of attendees from all sites
ethnic_group_attendees_all <- attendees %>%
  group_by(ethnic_group) %>%
  count()

write.csv(ethnic_group_attendees_all, "ethnicgp_attendees.csv")

#ethnic groups of attendees from Sentinel sites
ethnic_group_attendees_sentinel <- attendees_sentinel %>%
  group_by(ethnic_group) %>%
  count()

write.csv(ethnic_group_attendees_sentinel, "ethgp_sentinel_attendees.csv")

#ethnic groups of attendees from London sites
ethnic_group_attendees_ldn <- attendees_ldn %>%
  group_by(ethnic_group) %>%
  count()

write.csv(ethnic_group_attendees_ldn, "ethgp_ldn_attendees.csv")

#ethnic groups of attendees from sites outside of London 
ethnic_group_attendees_outside <- attendees_outside %>%
  group_by(ethnic_group) %>%
  count()

write.csv(ethnic_group_attendees_outside, "ethgp_outside_attendees.csv")


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


###### Demographics for attendees with a blood test ###########

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
  distinct(Provider_derived, TOKEN_PERSON_ID, ethnic_group) %>%
  group_by(ethnic_group) %>%
  count()

write.csv(eth_blood_tests_all, "ethnicgrp_bloodtests.csv")

##ethnic groups - sentinel

eth_blood_tests_sentinel <- bloodtestsattendeessentinel %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ethnic_group) %>%
  group_by(ethnic_group) %>%
  count()

write.csv(eth_blood_tests_sentinel, "ethnicgrp_bloodtests_sentinel.csv")

##ethnic groups - london

eth_blood_tests_ldn <- bloodtestsattendeesldn %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ethnic_group) %>%
  group_by(ethnic_group) %>%
  count()

write.csv(eth_blood_tests_ldn, "ethnicgrp_bloodtests_ldn.csv")

##ethnic groups - outside of london

eth_blood_tests_outside <- bloodtestsattendeesoutside %>%
  distinct(Provider_derived, TOKEN_PERSON_ID, ethnic_group) %>%
  group_by(ethnic_group) %>%
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



