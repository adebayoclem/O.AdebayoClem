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



#############Indicator 1b#############
#proportion of attendances to ED where a blood test has been carried out

#no of attendances with blood test / no of attendances overall


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



#############Indicator 2#############
#Proportion of patients having a blood test in ED who have a HIV/HBV/HCV test - SENTINEL SITES ONLY 

#no of attendees with HIV/HBV/HCV test / no. of attendees with blood test

##IMPORTANT: Need to run HIV_Sentinel_linking.R, HBV_Sentinel_linking.R and HCV_Sentinel_linking.R before running this section

###HIV

attendees_HIV <- included_tests_HIV4

attendees_HIV <- attendees_HIV %>% filter(SITE %in%five_included_sites)

#overall proportion

attendeesbloods_restricted <- bloodtestsattendeessentinel %>% filter(SITE %in% five_included_sites)

indicator2_hiv <- (attendees_HIV %>% count())/(attendeesbloods_restricted %>% count())


###HCV

attendees_HCV <- included_tests_HCV4

attendees_HCV <- attendees_HCV %>% filter(SITE %in%five_included_sites)

#overall proportion


indicator2_HCV <- (attendees_HCV %>% count())/(attendeesbloods_restricted %>% count())
#0.363 - 36%


###HBV

attendees_HBV <- included_tests_HBV4 %>% filter(SITE %in%five_included_sites)

attendees_HBV <- attendees_HBV %>% filter(SITE %in%five_included_sites)

#overall proportion

indicator2_HBV <- (attendees_HBV %>% count())/(attendeesbloods_restricted %>% count())
#0.352 - 35%



#############Indicator 3#############
#Proportion of patients attending ED who have a HIV/HBV/HCV test

#restrict attendees to the 5 included sites
attendees_sentinel_restricted <- attendees_sentinel %>%
  filter(SITE %in% five_included_sites)
  
# attendees with HIV/HBV/HCV test / total attendees to ED

###HIV

(attendees_HIV %>% count())/(attendees_sentinel_restricted %>% count())

#0.080 -> 8%

###HBV

(attendees_HBV %>% count())/(attendees_sentinel_restricted %>% count())

#0.055 -> 6%

###HCV

(attendees_HCV %>% count())/(attendees_sentinel_restricted %>% count())
#0.057 -> 6%

#############Indicator 4#############
#Proportion of patients who tested positive for HIV/HCV/HBV in ED who received a confirmatory test 

#no. of attendees with a confirmatory HIV/HCV/HBV test after a positive ED HIV/HCV/HBV test / no. of attendees with a positive HIV/HCV/HBV test

#reduce to 5 included sites
HCV_included_5 <-  HCV_included_diagnoses_sentinel %>% 
  filter(Hospital == "Whipps Cross University Hospital" | Hospital=="St Thomas' Hospital" | Hospital=="The Royal London Hospital" | Hospital=="Newham General Hospital" | Hospital=="Homerton University Hospital")

#attendees with a positive HCV test
positive_HCV <- HCV_included_5 %>%
  filter(antiHCV == "positive")%>%
  distinct(NHS_NUMBER)%>%
  count()
#411

#attendees with a confirmatory HCV test (RNA)

confirmatory_HCV <- HCV_included_5 %>% 
  filter(HCVRNA == "positive")%>%
  distinct(NHS_NUMBER)%>%
  count()
#88

indicator4_hcv <- (confirmatory_HCV/positive_HCV)
#0.214 -> 21% 


##HBV and HIV NO CONFIRMATORY TEST
## HBV
#reduce to 5 included sites 
HBV_included_5 <-  HBV_included_diagnoses_sentinel %>% 
  filter(Hospital == "Whipps Cross University Hospital" | Hospital=="St Thomas' Hospital" | Hospital=="The Royal London Hospital" | Hospital=="Newham General Hospital" | Hospital=="Homerton University Hospital")


#attendees with a positive HBV test
positive_HBV <- HBV_included_5 %>% 
  filter(HBsAg == "Positive")%>%
  distinct(NHS_NUMBER)%>%
  count()
#450

#attendees with a confirmatory HBV test
confirmatory_HBV <- HBV_included_5 %>% 
  filter(HBsAg == "Positive")%>%
  distinct(NHS_NUMBER)%>%
  count()
#450

indicator4_hbv <- (confirmatory_HBV/positive_HBV)
#100%

##NO DIAGNOSES TABLE FOR HIV



#############Indicator 5#############
#HCV reflex testing rate

#Number of patients who received a HCV RNA test with x days following a positive HCV antibody test in ED / Number of patients attending ED who test positive for HCV antibodies


reflex_HCV <- HCV_included_5 %>%
  filter(RNAtiming == "Reflex")%>%
  distinct(NHS_NUMBER)%>%
  count()


antiHCV_pos <- HCV_included_5 %>% 
  filter(antiHCV == "positive")%>%
  distinct(NHS_NUMBER)%>%
  count()


indicator5 <- reflex_HCV / antiHCV_pos
#0.941 -> 94%




                             