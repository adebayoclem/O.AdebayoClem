if (!require(tidyverse)) install.packages("tidyverse")
if (!require(clipr)) install.packages("clipr")

# load data files needed

source("01_linking/HCV_sentinel_linking.R")

#summary
#included_tests_HCV - linked to attendances (with blood tests)
#included_tests_HCV3 - linked to attendances (no blood tests)
#included_tests_HCV4 - linked to attendees (no blood tests)
#included_tests_HCV5 - linked to attendees (with blood tests)

################ overall counts of attendances, attendees and blood tests with HCV tests ###############################

##### attendees with HCV tests 

#check expected no. of attendees w/ HCV tests
included_tests_HCV4 %>% count()
#58,415 attendees w/ HCV test

HCV_tests_attendees_month <- included_tests_HCV4 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(totalattendees = sum(n))

##### attendances with HCV tests

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

##### attendees who had blood tests and HCV tests

HCV_blood_attendees_month <- included_tests_HCV5 %>% 
  group_by(ARRIVAL_MONTH)%>%
  count()%>%
  summarise(total_HCVattendeesmonth = sum(n))

##### attendances who had blood tests and HCV tests 

HCV_blood_attendances_month <- included_tests_HCV %>%
  group_by(ARRIVAL_MONTH) %>%
  count()%>%
  summarise(totalattendances = sum(n))
