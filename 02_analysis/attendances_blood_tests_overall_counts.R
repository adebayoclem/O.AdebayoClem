if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
if (!require(janitor)) install.packages("janitor")
if (!require(arrow)) install.packages("arrow")
if (!require(clipr)) install.packages("clipr")

# load data files needed

source("Attendances_blood_tests_setup.R")

########### Attendees ##################

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


##### Attendances ##############

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


###### Attendees with a blood test ###########

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


###### Attendances with a blood test ###########

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





