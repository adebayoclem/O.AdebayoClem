#Indicators 11-13


#############Indicator 11b##############
#Proportion of previously lost to follow up patients / not engaged in care who re-engage in HCV services 

#(no of previously lost to follow up patients/not engaged in care who are re-engaged in HCV services)/(no of patients with positive HCV tests who are lost to follow up from HCV services/not engaged in care)

HCV_prelost <- HCV_DIAG_SENTINEL %>% 
  filter((preRxoutcome == "Lost to follow-up (Did not attend 12/24 week post-treatment follow-up appointments)"| preRxoutcome == "Did not commence treatment") & !is.na(postRxdate)) 
#7

HCVlost <- HCV_DIAG_SENTINEL %>% 
  filter(preRxoutcome == "Lost to follow-up (Did not attend 12/24 week post-treatment follow-up appointments)"| preRxoutcome == "Did not commence treatment")
#52

indicator11b <- (HCV_prelost %>% count()) / (HCVlost %>% count())
#50%


#############Indicator 11c##############
#Proportion of previously lost to follow up patients / not engaged in care who re-engage in HBV services 

#(no of previously lost to follow up patients/not engaged in care who are re-engaged in HBV services)/(no of patients with positive HBV tests who are lost to follow up from HCV services/not engaged in care)




#####NEEDS TX / LOST TO FOLLOW UP DATA ############




#############Indicator 12b##############
#Proportion of newly diagnosed people with HBV who are linked to care within x months of positive HBV test 

#(HBV DNA as a proxy or sequential tests suggested link to care)/(no of patients diagnosed with HBV)



#####NEEDS TX / LOST TO FOLLOW UP DATA ############




#############Indicator 12c##############
#Proportion of diagnosed people with HCV who are linked to care within 28 days of positive HCV PCR test 

#(no of patients attending HCV services within 28 days of positive HCV PCR test)/(no of patients diagnosed with HCV)

#Create column for date range (HCVRNAdate + 28) inclusion to check whether
#diagnosed people with HCV were linked to care within 28 days

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(max_tx_date = HCVRNAdate + 28, 
         include = case_when(HCVRNAdate < postRxdate &
                               (postRxdate < max_tx_date)~ "INCLUDE",
                             FALSE ~ "EXCLUDE"))

#no of patients attending HCV services within 28 days of positive HCV PCR test
HCV_attendee_28 <- HCV_DIAG_SENTINEL %>% 
  filter(include == "INCLUDE")


#indicator 12c calculation
indicator12c <- (HCV_attendee_28 %>% count()) / (HCV_DIAG_SENTINEL %>% count())


#############Indicator 13##############
#Median time between positive confirmatory BBV test and linkage to care

#HCV
HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(time_to_tx = postRxdate - HCVRNAdate)

timetotx_HCV <- HCV_DIAG_SENTINEL %>% 
  filter(!is.na(time_to_tx))
  
median(timetotx_HCV$time_to_tx)
#52 days


#not enough information for this for HBV - using HBV_DNA as a proxy, but giving v short median timeline?
#HBV
HBV_DIAGNOSES_SENTINEL <- HBV_DIAGNOSES_SENTINEL %>% 
  mutate(time_to_tx = HBV_DNAdate - HBsAGdate)

timetotx_HBV <- HBV_DIAGNOSES_SENTINEL %>% 
  filter(!is.na(time_to_tx))

median(timetotx_HBV$time_to_tx)
#3 days


  
  
  
  
  

