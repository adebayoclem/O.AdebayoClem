#############Indicator 6##############
#Proportion of patients who test positive in ED who are newly diagnosed with HIV/HCV/HBV after confirmatory testing 

#(number of patients with a positive confirmatory HCV test and no prior records in the relevant treatment database)/(number of patients attending ED who have a HCV test)

#numerator: number of patients with a positive confirmatory HCV test and no prior records in the relevant treatment database

#restrict diagnoses to the 5 chosen sites
HCV_included_5 <-  HCV_included_diagnoses_sentinel %>% 
  filter(Hospital == "Whipps Cross University Hospital" | Hospital=="St Thomas' Hospital" | Hospital=="The Royal London Hospital" | Hospital=="Newham General Hospital" | Hospital=="Homerton University Hospital")

HBV_included_5 <- HBV_included_diagnoses_sentinel %>% filter(SITE %in% five_included_sites)

#restrict HCV tests to the 5 chosen sites
attendees_HCV <- included_tests_HCV4
attendees_HCV <- attendees_HCV %>% filter(SITE %in%five_included_sites)

#restrict HBV tests to the 5 chosen sites
attendees_HBV <- included_tests_HBV4
attendees_HBV <- attendees_HBV %>% filter(SITE %in%five_included_sites)


##HCV

#numerator 

numerator_6_HCV <- HCV_included_5 %>% 
  filter(rna_positive_new == "New") %>%
  distinct(NHS_NUMBER) %>% 
  count() %>% 
  glimpse()
#41

#denominator: number of patients attending ED who have a HCV test

HCV_tested <- attendees_HCV %>% 
  distinct(NHS_NUMBER)%>%
  count() %>% 
  glimpse()
#58600

indicator_6_hcv <- numerator_6_HCV/HCV_tested


##HBV

#restrict diagnoses to the 5 chosen sites
HBV_included_5 <-  HBV_included_diagnoses_sentinel %>% 
  filter(Hospital == "Whipps Cross University Hospital" | Hospital=="St Thomas' Hospital" | Hospital=="The Royal London Hospital" | Hospital=="Newham General Hospital" | Hospital=="Homerton University Hospital")

#restrict HCV tests to the 5 chosen sites
attendees_HBV <- included_tests_HBV4
attendees_HBV <- attendees_HBV %>% filter(SITE %in%five_included_sites)


#numerator 

numerator_6_HBV <- HBV_included_5 %>% 
  filter(Newdiag == "Yes") %>%
  distinct(NHS_NUMBER) %>% 
  count() %>% 
  glimpse
#170

#denominator: number of patients attending ED who have a HBV test
HBV_tested <- attendees_HBV %>%
  distinct(NHS_NUMBER)%>%
  count() %>% 
  glimpse()
#56892

indicator_6_hbv <- numerator_6_HBV/HBV_tested


#############Indicator 7b##############
#proportion of patients who tested positive HCV AG/PCR (new or previous)

#(No attending ED who are PCR/antigen positive for HCV)/(number of patients attending ED who have a HCV test)

numerator_7b <- HCV_included_5 %>% 
  filter(HCVRNA == "positive" | HCVAg == "Positive") %>% 
  distinct(NHS_NUMBER) %>% 
  count() %>% 
  glimpse()
#88


indicator_7b <- numerator_7b/HCV_tested


#############Indicator 7c##############

#HBsAg Positivity Rate - proportion of patients who tested positive for HBsAg
#(no of patients attending ED who are HBsAg positive)/(No of patients attending ED who have a HBV test)


numerator7c <- HBV_included_5 %>%
  filter(HBsAg == "Positive") %>% 
  distinct(NHS_NUMBER) %>% 
  count() %>% 
  glimpse()
#573


indicator_7c <- numerator7c/HBV_tested


