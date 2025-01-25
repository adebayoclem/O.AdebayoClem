#Run attendances and blood tests script for testing indicators
source("00_setup/Attendances_blood_tests_setup.R")
#Run HCV_sentinel_linking to get included HCV tests
source("01_linking/HCV_Sentinel_linking.R")

######################################################################################################################
####cleaning and formatting

#Checking date ranges to make sure diagnoses sit within pilot time- they do

print(min(HCV_DIAGNOSES_SENTINEL$antiHCVdate, na.rm= TRUE))
print(max(HCV_DIAGNOSES_SENTINEL$antiHCVdate, na.rm= TRUE))
print(min(HCV_DIAGNOSES_SENTINEL$HCVRNAdate, na.rm= TRUE))
print(max(HCV_DIAGNOSES_SENTINEL$HCVRNAdate, na.rm= TRUE))

# format dates  

HCV_DIAG_SENTINEL <- HCV_DIAGNOSES_SENTINEL %>% 
  mutate(antiHCVdate = ymd(antiHCVdate),
         HCVRNAdate = ymd(HCVRNAdate),
         FirstHCVtestdate = ymd(FirstHCVtestdate),
         EED = ymd(EED),
         preRxdate = ymd(preRxdate),
         postRxdate = ymd(postRxdate),
         mincreatedate = ymd(mincreatedate),
         minreferraldate = ymd(minreferraldate),
         minsentineldate = ymd(minsentineldate),
         DOB = ymd(DOB)) %>% 
  glimpse


##checking if sentinel sites only? think so

unique(HCV_DIAG_SENTINEL$Trust)
#there is one called dup
print(HCV_DIAG_SENTINEL)
filter(HCV_DIAG_SENTINEL, Trust == "dup") #not sure what to do with these records? used below code to check whether they were for duplicate NHS numbers but not the case

HCV_DIAG_SENTINEL %>% 
  filter(Trust == "dup") %>% 
  count()


HCV_DIAG_SENTINEL %>% 
  filter(Trust == "dup") %>%
  distinct(NHSNumber) %>% 
  count()



glimpse(HCV_DIAG_SENTINEL)
#need to standardise datset


tolower

unique(HCV_DIAG_SENTINEL$antiHCV)
unique(HCV_DIAG_SENTINEL$HCVRNA)
unique(HCV_DIAG_SENTINEL$Newdiag)



HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, antiHCV = tolower(antiHCV)) 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, HCVRNA = tolower(HCVRNA)) 

####################DEMOGRAPHIC groupings etc.- add to this
#Age groups

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(age_group = case_when(Age <= 24 ~ "15-24",
                               Age >= 25 & Age <= 34 ~ "25-34",
                               Age >= 35 & Age <= 44 ~ "35-44",
                               Age >= 45 & Age <= 54 ~ "45-54",
                               Age >= 55 & Age <= 64 ~ "55-64",
                               Age >= 65 & Age <= 74 ~ "65-74",
                               Age >= 75 & Age <= 84 ~ "75-84",
                               Age >= 85 & Age <= 94 ~ "85-94",
                               Age > 94 ~ "95+")) %>%
  mutate(age_group2 = case_when(Age < 45 ~ "16-44",
                                Age > 44 & Age < 65 ~ "45-64",
                                Age > 64 ~ "65+")) 


#Ethnicity

unique(HCV_DIAG_SENTINEL$Eth)
#Need to decide on this variable



#PWID
unique(HCV_DIAG_SENTINEL$PWID)

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(PWID_2 = case_when(PWID == "Current/RecentPWID" | PWID == "Yes" ~ "Current/Recent PWID", # is yes current or past?
                            PWID == "PastPWID" ~ "Past PWID",
                            PWID == "NeverPWID" | PWID == "No" | PWID == "Never PWID" ~ "Never PWID"))  


#homelessflag- either Yes or NA- can leave as is  
unique(HCV_DIAG_SENTINEL$homelessflag)

#COB- how many positive diagnoses have COB avaialble? 308 are NA, 255/475 NA for those with NHS number- also 1 with not completed and 5 "". Small numbers for countries so would not be able to present
unique(HCV_DIAG_SENTINEL$COB)

HCV_DIAG_SENTINEL %>% 
  filter(HCVRNA == 'positive') %>% 
  group_by(COB) %>% 
  distinct(NHSNumber) %>% 
  tally() %>% 
  print(n = 42)

#need sex or gender adding to table
#IMD- linking to other tables?

#diagnosis indicators HCV

glimpse(HCV_DIAG_SENTINEL)

####################### RNA positives- all, new and previous##################################

#all positives

#all positives counts- NHS number check

#total 529
HCV_DIAG_SENTINEL %>% 
  filter(HCVRNA == 'positive') %>% 
  count() %>% 
  glimpse

#total 475 NS number
HCV_DIAG_SENTINEL %>% 
  filter(HCVRNA == 'positive') %>% 
  distinct(NHSNumber)%>%
  count() %>% 
  glimpse

#Total 54 NHS number missing
HCV_DIAG_SENTINEL %>% 
  filter(HCVRNA == 'positive' & is.na(NHSNumber)) %>% 
  count() %>% 
  glimpse


#coUNT ALL- CHECK WHETHER REMOVE ONES WITH NO NHS NUMBER
rna_positive <- HCV_DIAG_SENTINEL %>% 
  filter(HCVRNA == 'positive') %>%
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse

#counts for positives and new diag flag
HCV_DIAG_SENTINEL  %>%
  filter(HCVRNA == 'positive' & Newdiag == 'Yes') %>% 
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse()


HCV_DIAG_SENTINEL  %>%
  filter(HCVRNA == 'positive' & Newdiag == 'No') %>% 
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse()

#10 that are positive new diagnoses with treatment date and earliest event date before rna date - currently recode so not considered new but need to check whether these are actually reinfection
HCV_DIAG_SENTINEL %>% 
  filter(HCVRNA == 'positive' & Newdiag == 'Yes' & (preRxdate < HCVRNAdate| EED < HCVRNAdate)) %>% 
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse()

#code so have a flag that can be used to remove them

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(rna_recode = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' & (preRxdate < HCVRNAdate | EED < HCVRNAdate) ~ "recode")) 

HCV_DIAG_SENTINEL  %>%
  filter(rna_recode == "recode") %>% 
  count() %>% 
  glimpse()

#creating new cases definition and variable- strict poss definition
#HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
#mutate(rna_positive_new = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' & is.na(rna_recode) ~ "New",
#                            HCVRNA == 'positive' & (is.na(Newdiag) | Newdiag == 'POSS') & (preRxdate >= HCVRNAdate | EED >= HCVRNAdate) ~ "Possible New",
#                           .default = "Null")) %>% 
# mutate(rna_positive_new = if_else(HCVRNA == 'positive' & rna_positive_new == 'Null',  "Previous", rna_positive_new)) %>%
#  group_by(rna_positive_new) %>% 
# tally()

#wider poss defintion used- POSS can be considered as new diagnosis- check in ED meetings

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(rna_positive_new = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' & is.na(rna_recode) ~ "New",
                                      HCVRNA == 'positive' & Newdiag == 'POSS'~ "Possible New",
                                      .default = "Null")) %>% 
  mutate(rna_positive_new = if_else(HCVRNA == 'positive' & rna_positive_new == 'Null',  "Previous", rna_positive_new)) %>%
  group_by(rna_positive_new) %>% 
  tally()


#counts for new and previous
#-103 new
HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "New") %>% 
  count() %>% 
  glimpse

HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "New") %>%
  distinct(NHSNumber)
count() %>% 
  glimpse

# 0 possible
HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "Possible New") %>% 
  count() %>% 
  glimpse

# 425
HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "Previous") %>% 
  count() %>% 
  glimpse

HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "Previous") %>% 
  distinct(NHSNumber)
count() %>% 
  glimpse

#checking coding- below confirms that there are zero that fit the possible criteria when using strict definition


HCV_DIAG_SENTINEL  %>%
  filter(HCVRNA == 'positive' & Newdiag == 'POSS' & (preRxdate >= HCVRNAdate | EED >= HCVRNAdate)) %>% 
  count() %>% 
  glimpse()

HCV_DIAG_SENTINEL  %>%
  filter(HCVRNA == 'positive' & is.na(Newdiag) & (preRxdate >= HCVRNAdate | EED >= HCVRNAdate)) %>% 
  count() %>% 
  glimpse()

HCV_DIAG_SENTINEL  %>%
  filter(HCVRNA == 'positive' & Newdiag == 'POSS' & (is.na(preRxdate)| is.na(EED))) %>% 
  count() %>% 
  glimpse()

#antiHCV positives

antiHCV_positive <- HCV_DIAG_SENTINEL %>% 
  filter(antiHCV == 'positive') %>% 
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse


#indicator 6: proportion of patients who test positive in ED who are newly diagnosed with HCV after confirmatory testing 

#numerator: number if patients with a positive confirmatory HCV test and no prior records in the relevant treatment database

#numerator 

numerator_6 <- HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "New") %>%
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse

#denominator: number of patients attending ED who have a HCV test- 475 people with confirmatory HCV test (this is the number of people with an NHS number)


indicator_6 <- numerator_6/confirmatory_HCV
# 0.21 -> 21%

#indicator  7b - proportion of patients who tested positive HCV AG/PCR (new or previous)

#numerator number attending ED who are new vs previous

#new diagnosis 22%

numerator_7 <- HCV_DIAG_SENTINEL %>% 
  filter(rna_positive_new == "Previous") %>% 
  distinct(NHSNumber) %>% 
  count() %>% 
  glimpse

previous_prop <- numerator_7/confirmatory_HCV

#previously diagnosed proportion confirmatory test 0.80 -> 80%



#demographics

####################people in this dataset######################

#Age

HCV_DIAG_SENTINEL %>%
  group_by(age_group) %>%
  distinct(NHSNumber) %>% 
  count()

HCV_DIAG_SENTINEL %>%
  group_by(age_group2) %>%
  distinct(NHSNumber) %>% 
  count()

#PWID

HCV_DIAG_SENTINEL %>% 
  group_by(PWID_2) %>% 
  distinct(NHSNumber) %>% 
  count()

#homeless flag
HCV_DIAG_SENTINEL %>% 
  group_by(homelessflag) %>% 
  distinct(NHSNumber) %>% 
  count()

##########################RNA_positives####################################

#Age

HCV_DIAG_SENTINEL %>%
  group_by(age_group, HCVRNA) %>%
  distinct(NHSNumber) %>% 
  count() %>% 
  print(n=30)

HCV_DIAG_SENTINEL %>%
  group_by(age_group2, HCVRNA) %>%
  distinct(NHSNumber) %>% 
  count() 

#PWID

HCV_DIAG_SENTINEL %>% 
  group_by(PWID_2, HCVRNA) %>% 
  distinct(NHSNumber) %>% 
  count()

#homeless flag
HCV_DIAG_SENTINEL %>% 
  group_by(homelessflag, HCVRNA) %>% 
  distinct(NHSNumber) %>% 
  count()


#####################stratified by those newly positive after confirmatory test (indicator 1)#####################



#Age

HCV_DIAG_SENTINEL %>%
  group_by(age_group, rna_positive_new) %>%
  distinct(NHSNumber) %>% 
  count() %>% 
  print(n=30)

HCV_DIAG_SENTINEL %>%
  group_by(age_group2, rna_positive_new) %>%
  distinct(NHSNumber) %>% 
  count() 

#PWID

HCV_DIAG_SENTINEL %>% 
  group_by(PWID_2, rna_positive_new) %>% 
  distinct(NHSNumber) %>% 
  count()

#homeless flag
HCV_DIAG_SENTINEL %>% 
  group_by(homelessflag, rna_positive_new) %>% 
  distinct(NHSNumber) %>% 
  count()  


################anti HCV

#Age

HCV_DIAG_SENTINEL %>%
  group_by(age_group, antiHCV) %>%
  distinct(NHSNumber) %>% 
  count() %>% 
  print(n=30)

HCV_DIAG_SENTINEL %>%
  group_by(age_group2, antiHCV) %>%
  distinct(NHSNumber) %>% 
  count() 

#PWID

HCV_DIAG_SENTINEL %>% 
  group_by(PWID_2, antiHCV) %>% 
  distinct(NHSNumber) %>% 
  count()

#homeless flag
HCV_DIAG_SENTINEL %>% 
  group_by(homelessflag, antiHCV) %>% 
  distinct(NHSNumber) %>% 
  count()  

