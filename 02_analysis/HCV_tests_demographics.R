if (!require(tidyverse)) install.packages("tidyverse")
if (!require(clipr)) install.packages("clipr")

# load data files needed

source("01_linking/HCV_sentinel_linking.R")

#summary
#included_tests_HCV - linked to attendances (with blood tests)
#included_tests_HCV3 - linked to attendances (no blood tests)
#included_tests_HCV4 - linked to attendees (no blood tests)
#included_tests_HCV5 - linked to attendees (with blood tests)


################ demographics for attendance, attendee and blood tests with HCV tests ###############################


#####demographics for attendees with HCV tests 

# rename dataset
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

#rename dataset
attendances_HCV <- included_tests_HCV3

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

#rename dataset

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

#rename dataset

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