#Run Attendances and blood tests.R to load the variables required (listed below)
# Run linking scripts

# which attendees had a blood test
blood_tests_NHSn <- ECDSBloodsfirstdate %>%  select(NHS_NUMBER)%>%
  mutate(blood_test ='Yes')

attendees_joined <-left_join(attendees, blood_tests_NHSn, by= "NHS_NUMBER")

%>%
  distinct(NHS_NUMBER, .keep_all = TRUE)

# which attendees had a HIV test
HIV_tests_NHSn <- included_tests_HIV5 %>%  select(NHS_NUMBER)%>%
  mutate(HIV_test ='Yes')

attendees_joined <-left_join(attendees, HIV_tests_NHSn, by= "NHS_NUMBER")%>%
  distinct(NHS_NUMBER, ARRIVAL_DATE, .keep_all = TRUE)

# which attendees had a HCV test
HCV_tests_NHSn <- included_tests_HCV5 %>%  select(NHS_NUMBER)%>%
  mutate(HCV_test ='Yes')

attendees_joined <-left_join(attendees, HCV_tests_NHSn, by= "NHS_NUMBER")%>%
  distinct(NHS_NUMBER, ARRIVAL_DATE, .keep_all = TRUE)

# which attendees had a HBV test
HBV_tests_NHSn <- included_tests_HBV5 %>%  select(NHS_NUMBER)%>%
  mutate(HBV_test ='Yes')

attendees_joined <-left_join(attendees, HBV_tests_NHSn, by= "NHS_NUMBER")%>%
  distinct(NHS_NUMBER, ARRIVAL_DATE, .keep_all = TRUE)