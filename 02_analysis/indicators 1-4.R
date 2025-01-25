#Run Attendances and blood tests.R to load the variables required (listed below)
    
    ##ATTENDEES
    
    #attendees - all sites
    attendees
    
    #attendees - sentinel sites
    attendees_sentinel
    
    
    ##ATTENDANCES
    
    #attendances - all sites
    attendances
    
    #attendances - sentinel sites
    attendances_sentinel <- attendances %>%
      filter(SITE %in% sentinel_sites)
    
    
    ##BLOODS
    
    #blood tests - all sites
    ECDSBloodsfirstdate
    ECDSBloodsfirstdate2
    attendancesbloodtests
    attendancesbloodtests2
    
    
    #blood tests attendances - sentinel sites
    bloodtestsattendeessentinel
    bloodtestsattendeessentinel2
    attendancesbloodtests_sentinel    
    attendancesbloodtests_sentinel2 
    
    
#############Indicator 1a##############
#proportion of patients attending ED who have had a blood test

#no of attendees with blood test / no of attendees overall

#definition 1

#all sites

attendeesbloods_all <- ECDSBloodsfirstdate %>%
  count()
#994,815

indicator1a_all1 <- attendeesbloods_all/(attendees %>% count())
#0.565 -> 57%

#sentinel sites only

attendeesbloods_sentinel <- bloodtestsattendeessentinel %>%
  count()
#584,824

indicator1a_sentinel1 <- attendeesbloods_sentinel/(attendees_sentinel %>% count())
#0.569 -> 57% 

#definition 2

attendeesbloods_all2 <- ECDSBloodsfirstdate2 %>%
  count()
#1,013,297

indicator1a_all2 <- attendeesbloods_all2/((attendees) %>% count())
#0.576 -> 58%

#sentinel sites only

attendeesbloods_sentinel2 <- bloodtestsattendeessentinel2 %>%
  count()
#596,719

indicator1a_sentinel2 <- attendeesbloods_sentinel2/(attendees_sentinel %>% count())
#0.580 -> 58% 

#############Indicator 1b#############
#proportion of attendances to ED where a blood test has been carried out

#no of attendances with blood test / no of attendances overall

#definition 1

#all sites
attendancesbloods_all <- attendancesbloodtests %>% count()
#1,420,672

indicator1b_all <- attendancesbloods_all/(attendances %>% count())
#0.521 -> 52%

#sentinel sites only 
attendancesbloods_sentinel <- attendancesbloodtests_sentinel %>% count()
#837,063

indicator1b_sentinel <- attendancesbloods_sentinel/(attendances_sentinel %>% count())
#0.528 -> 53%


#definition 2

attendancesbloods_all2 <- attendancesbloodtests2 %>% count()
#1,452,282

indicator1b_all2 <- attendancesbloods_all2/(attendances %>% count())
#0.533 -> 53%

#sentinel sites only 
attendancesbloods_sentinel2 <- attendancesbloodtests_sentinel2 %>% count()
#857,683

indicator1b_sentinel2 <- attendancesbloods_sentinel2/(attendances_sentinel %>% count())
#0.541 -> 54%



#############Indicator 2#############
#Proportion of patients having a blood test in ED who have a HIV/HBV/HCV test - SENTINEL SITES ONLY 

#no of attendees with HIV/HBV/HCV test / no. of attendees with blood test

##IMPORTANT: Need to run HIV_Sentinel_linking.R, HBV_Sentinel_linking.R and HCV_Sentinel_linking.R before running this section

###HIV
#definition 1

attendees_HIV <- included_tests_HIV4

#overall proportion

attendeesbloods_sentinel <- bloodtestsattendeessentinel %>% count()
#584,824

indicator2_hiv <- (attendees_HIV %>% count())/attendeesbloods_sentinel
#0.163 - 16%


#definition 2

attendeesbloods_sentinel2 <- bloodtestsattendeessentinel2 %>% count()
#596,719

#overall proportion
indicator2_hiv2 <- (attendees_HIV %>% count())/attendeesbloods_sentinel2
#0.159 - 16%


###HCV
#definition 1

attendees_HCV <- included_tests_HCV4

#overall proportion

attendeesbloods_sentinel <- bloodtestsattendeessentinel %>% count()
#584,824

indicator2_HCV <- (attendees_HCV %>% count())/attendeesbloods_sentinel
#0.099 - 10%


#definition 2

attendeesbloods_sentinel2 <- bloodtestsattendeessentinel2 %>% count()
#596,719

#overall proportion
indicator2_HCV2 <- (attendees_HCV %>% count())/attendeesbloods_sentinel2
#0.097 - 10%


###HBV
#definition 1

attendees_HBV <- included_tests_HBV4

#overall proportion

attendeesbloods_sentinel <- bloodtestsattendeessentinel %>% count()
#584,824

indicator2_HBV <- (attendees_HBV %>% count())/attendeesbloods_sentinel
#0.062 - 6%


#definition 2

attendeesbloods_sentinel2 <- bloodtestsattendeessentinel2 %>% count()
#596,719

#overall proportion
indicator2_HBV2 <- (attendees_HBV %>% count())/attendeesbloods_sentinel2
#0.061 - 6%


#############Indicator 3#############
#Proportion of patients attending ED who have a HIV/HBV/HCV test

# attendees with HIV/HBV/HCV test / total attendees to ED

###HIV

(attendees_HIV %>% count())/(attendees_sentinel %>% count())

#0.092 -> 9%

###HBV

(attendees_HBV %>% count())/(attendees_sentinel %>% count())

#0.035 -> 4%

###HCV

(attendees_HCV %>% count())/(attendees_sentinel %>% count())
#0.056 -> 6%

#############Indicator 4#############
#Proportion of patients who tested positive for HIV/HCV/HBV in ED who received a confirmatory test 

#no. of attendees with a confirmatory HIV/HCV/HBV test after a positive ED HIV/HCV/HBV test / no. of attendees with a positive HIV/HCV/HBV test

#attendees with a positive HCV test
positive_HCV <- HCV_TESTS_SENTINEL %>%
  filter(antiHCV == "POSITIVE" | HCVRNA == "Positive" )%>%
  distinct(NHS_NUMBER)%>%
  count()
#1382

#attendees with a confirmatory HCV test (RNA)
confirmatory_HCV <- HCV_TESTS_SENTINEL %>% 
  filter(HCVRNA == "Positive")%>%
  distinct(NHS_NUMBER)%>%
  count()
#475

#HCV antibody positive
antibody_HCV <- HCV_TESTS_SENTINEL %>% 
  filter(antiHCV == "POSITIVE")%>%
  distinct(NHS_NUMBER)%>%
  count()
#1106

indicator4_hcv <- (confirmatory_HCV/positive_HCV)
#0.343 -> 34% 

indicator4_hcv <- (antibody_HCV/positive_HCV)
#0.800 <- 80%


#attendees with a positive HBV test
positive_HBV <- HBV_TESTS_SENTINEL %>% 
  filter(HBV == "POSITIVE")%>%
  distinct(NHS_NUMBER)%>%
  count()
#715

#attendees with a confirmatory HBV test
confirmatory_HBV <- HBV_TESTS_SENTINEL %>% 
  filter(HBV_PCR == "POSITIVE")%>%
  distinct(NHS_NUMBER)%>%
  count()
#9

indicator4_hbv <- (confirmatory_HBV/positive_HBV)
#0.012 -> 1%



                             