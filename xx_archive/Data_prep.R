options(scipen=999999)
library(data.table)
library(clipr)
if (!require(arrow)) install.packages("arrow")

###Setup_tables script must be run before this one - don't need all of it but need to load ECDS_JOIN_PII, HCV_Tests, HBV_Tests


# Basic statistics - attendances  ---------------- this should be in Attendances and blood tests script - move this section to that script

##This query can also take a very long time so if no new data read in parquet
#attendances <- ECDS_JOIN_PII %>% select(PheKey, TOKEN_PERSON_ID, Provider_derived, SITE, ARRIVAL_MONTH, ARRIVAL_DATE, CHIEF_COMPLAINT_Description) %>% unique()

#attendances %>% group_by(ARRIVAL_MONTH, Provider_derived, SITE) %>% summarise(n=n()) %>% write_clip()

##write parquet file if newly run query 
#attendances %>% write_dataset(path = "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/attendances", format = "parquet")

####-------------------------------------------------------------------------------------

## Load attendances parquet
attendances <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/attendances/part-0.parquet")


# Create hospital-specific inclusion criteria flag ------------------------
#need to use the go-live tracker





## ----------------------------------------------------

## ECDS Reference tables - for reference, not needed in later script
# Investigation_codes <- ECDS_JOIN_PII %>% select(INVESTIGATION_CODE, INVESTIGATION_CODE_Description) %>% unique()
# Provider_codes <- ECDS_JOIN_PII %>% select(SITE, PROVIDER_CODE_DERIVED) %>% unique()

investigations <- ECDS_JOIN_PII %>% select(PheKey, TOKEN_PERSON_ID, NHS_NUMBER, ARRIVAL_DATE, ARRIVAL_MONTH, AGE_AT_ARRIVAL, Provider_derived, IMD_19, ETHNIC_CATEGORY_Description, INVESTIGATION_DATE, INVESTIGATION_CODE) %>% unique()

## ----------------------------------------------------------------------

## Processing HCV tests from Sentinel ----------------------------------
#Need to use all tests before DEC as cut-off to match with latest ECDS data
HCV_tests <- HCV_tests %>% filter(DATETEST < "2023-04-01")

#Look at number of HCV tests that have an NHS number
HCV_tests_NHS <- HCV_tests %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1) )
#Count number and percentage of tests with no NHS number per lab
HCV_tests_LAB <- HCV_tests_NHS %>% group_by(PHLS2, TESTMONTH) %>% summarise(n=n(),
                                                                          valid = sum(NA_tally)) %>% mutate(pct = valid/n*100)

## the below doesn't group by Dupe_sample and NHS no to get tests per person - not sure what was wanted - need to remove datetest in grouping?
HCV_tests_person <- HCV_tests_NHS %>% group_by(DUPE_SAMPLE, NHS_NUMBER, DATETEST) %>% summarise (n=n()) #%>% select(-n)

# Select HCV tests that have an NHS number
HCV_tests_NHSn <- HCV_tests_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, DATETEST)

# Get HCV tests with an NHS number and order tests for each person by date
HCV_tests_dates <- dcast(data.table(HCV_tests_NHSn), NHS_NUMBER~rowid(NHS_NUMBER, prefix="date"), value.var="DATETEST")

#Keep only first 5 tests
#might want to expand this to first 10? To improve linkage in future.
HCV_tests_dates <- HCV_tests_dates %>% select(NHS_NUMBER, date1, date2, date3, date4, date5)

#Create a list of NHS numbers for those who have HCV test recorded
HCV_TESTED <- as.list(HCV_tests_NHSn[1])



# Prepare linkage tables --------------------------------------------------

#Link to ECDS to keep ECDS records who have an HCV test in Sentinel (linking on NHS no)
ECDS_HCV_TESTS <- subset(ECDS_JOIN_PII,NHS_NUMBER %in% HCV_TESTED$NHS_NUMBER) %>% unique()

# #Quick summary of most common tests had by people who had a HCV test -(what is the purpose of this? - not needed every time)
# SUMMARY_LINKED_TESTS <- ECDS_HCV_TESTS %>% group_by(INVESTIGATION_CODE_Description) %>% summarise(n=n())
# write_clip(SUMMARY_LINKED_TESTS)

#widen data out so each test has own column, using PheKey NOT NHS number (so it accounts for diff visits)
#added the underscore to the prefix to support with column dropping later on
ECDS_HCV_TESTS_2 <- dcast(data.table(ECDS_HCV_TESTS),PheKey~rowid(PheKey, prefix="test_"), value.var="INVESTIGATION_CODE_Description")

#determine max number of investigations per person
ECDS_HCV_TESTS %>% group_by(ECDS_HCV_TESTS$InvestigationIdx) %>% summarise(n=n())

#reorder data
#max number of tests per person is 12
ECDS_HCV_TESTS_2 <- ECDS_HCV_TESTS_2 %>% select(PheKey, test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_8, test_9, test_10, 
                                              test_11, test_12)  

#join back to original ECDS dataset to get arrival date, investigation date etc


tests <- Investigation_codes$INVESTIGATION_CODE_Description
#test <- ECDS_HCV_TESTS_ALL %>% mutate(ftccol = apply(ECDS_HCV_TESTS_ALL,1,function(x){any(x %in% "Full blood count (FBC)")}))

ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL

#Create a single new column for each type of test, then create flag to indicate if that test 
#carried out on each visit
for (test in tests){ 
 print(test)
y <- paste(test,sep="_")
ECDS_HCV_TESTS_ALL_WIDE[[y]]<- apply(ECDS_HCV_TESTS_ALL,1,function(x){any(x %in% test)})  }


#tidy column names 
ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% select(-contains("test_"))
#Replace true/false with 1/0
ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% mutate_if(is.logical, as.numeric)


# Not sure what this is doing, is it checking that the people with HCV tests also had a blood test?
# Would it be better to link to the ECDS people who had a blood test table?
#Create combined 'blood test' variable
ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% mutate(Blood_any =
                                                      case_when(Serology == 1 ~1,
                                                                `Urea & Electrolytes (U&Es)` ==1 ~ 1,
                                                                `Full blood count (FBC)` == 1 ~ 1,
                                                                `Liver function tests (LFTs)` ==1 ~ 1,
                                                                TRUE ~ 0))


ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% mutate(Blood_any_2 =
                                                                case_when(Serology == 1 ~1,
                                                                          `Urea & Electrolytes (U&Es)` ==1 ~ 1,
                                                                          `Full blood count (FBC)` == 1 ~ 1,
                                                                          `Liver function tests (LFTs)` == 1 ~1,
                                                                           Lactate ==1 ~1,
                                                                          `C reactive protein (CRP)`==1 ~1,
                                                                          `Clotting studies`==1 ~1,
                                                                          TRUE ~ 0))
                                                                                                                                                    
#join on the arrival dates and provider codes for grouping
all_linked_blood_tests <- ECDS_HCV_TESTS_ALL_WIDE %>% left_join(., attendances, by=c("TOKEN_PERSON_ID", "ARRIVAL_DATE", "PheKey"))


#produce some numbers
all_linked_blood_tests %>% group_by(Blood_any, Provider_derived, SITE) %>% summarise(n=n()) %>% write_clip()
all_linked_blood_tests %>% group_by(Blood_any_2, Provider_derived, SITE) %>% summarise(n=n())
all_linked_blood_tests %>% group_by(Blood_any,Blood_any_2) %>% summarise(n=n())

#look at blood tests by provider
tests_by_provider <- all_linked_blood_tests %>% 
  select(6:55,Provider_derived, Blood_any)%>% 
  group_by(Provider_derived) %>% 
  summarise(across(everything(), sum, na.rm=TRUE))%>% write_clip()

#look at blood tests by site
tests_by_provider_nonblood1 <- all_linked_blood_tests %>% 
  select(6:55,Provider_derived, SITE, Blood_any_2)%>% 
  filter(Blood_any_2 == 0) %>% 
  group_by(Provider_derived, SITE) %>% 
  summarise(across(everything(), sum, na.rm=TRUE))%>% write_clip()

#look at linked tests that don't meet definition of blood tests (as above)
tests_by_provider_nonblood2 <- all_linked_blood_tests %>% 
  select(6:55,Provider_derived, Blood_any)%>% 
  filter(Blood_any == 0) %>% 
  group_by(Provider_derived) %>% 
  summarise(across(everything(), sum, na.rm=TRUE))%>% write_clip()


#linked_non_blood1 <- ECDS_HCV_TESTS_ALL_WIDE %>% 
#  filter(Blood_any == 0) %>%  
#  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>% write_clip()

#linked_non_blood2 <- ECDS_HCV_TESTS_ALL_WIDE %>% 
#  filter(Blood_any_2 == 0) %>%  
#  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>% write_clip()

#x <- ECDS_HCV_TESTS_ALL_WIDE %>%
#  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))




#Keep only records defined as having a blood test
linked_blood_tests <- ECDS_HCV_TESTS_ALL_WIDE %>% select(PheKey, TOKEN_PERSON_ID, NHS_NUMBER, ARRIVAL_DATE, ARRIVAL_MONTH, AGE_AT_ARRIVAL, Provider_derived, INVESTIGATION_DATE,
                                                         Blood_any) %>% filter(Blood_any == 1)

#Remove investigation date and keep arrival date to avoid duplication
linked_blood_tests  <-linked_blood_tests %>% select(-INVESTIGATION_DATE) %>% unique() 

#Join HCV test dates 
linked_blood_tests <- left_join(linked_blood_tests, HCV_tests_dates, by="NHS_NUMBER")


ECDS_HCV_JOINED <- left_join(ECDS_JOIN_PII, HCV_tests_NHSn, by="NHS_NUMBER") %>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         DATETEST = as.Date(DATETEST))



#######
#Create column for date range (investigation date + 14) inclusion to check whether
#tests were carried out during person's a+e visit --- questionable - work out why 14 days - this seems too long? Maybe 7 days instead
ECDS_HCV_JOINED <- ECDS_HCV_JOINED %>% mutate(max_test_date = ARRIVAL_DATE + 7,
                                              include = case_when(
    DATETEST < INVESTIGATION_DATE ~ "EXCLUDE",
    max_test_date < DATETEST ~ "EXCLUDE",
    DATETEST > max_test_date ~ "EXCLUDE",
    DATETEST == INVESTIGATION_DATE ~ "INCLUDE",
    FALSE ~ "INCLUDE"))






# Repeat for HBV -------------------------------------------------------
#Need to use all tests before DEC as cut-off to match with latest ECDS data
ECDS_HBV_TESTS <- subset(ECDS_JOIN_PII,NHS_NUMBER %in% HBV_TESTED$NHS_NUMBER) %>% unique()

#Quick summary of most common tests
SUMMARY_LINKED_TESTS <- ECDS_HBV_TESTS %>% group_by(INVESTIGATION_CODE_Description) %>% summarise(n=n())
write_clip(SUMMARY_LINKED_TESTS)

#widen data out so each test has own column, using PheKey NOT NHS number (so it accounts for diff visits)
#added the underscore to the prefix to support with column dropping later on
ECDS_HBV_TESTS_2 <- dcast(data.table(ECDS_HBV_TESTS),PheKey~rowid(PheKey, prefix="test_"), value.var="INVESTIGATION_CODE_Description")

#determine max number of investigations per person
ECDS_HBV_TESTS %>% group_by(ECDS_HBV_TESTS$InvestigationIdx) %>% summarise(n=n())

#reorder data
#max number of tests per person is 12
ECDS_HBV_TESTS_2 <- ECDS_HBV_TESTS_2 %>% select(PheKey, test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_8, test_9, test_10, 
                                              test_11, test_12)  

#join back to original ECDS dataset to get arrival date, investigation date etc
ECDS_HBV_TESTS_ALL <- left_join(ECDS_HBV_TESTS_2, investigations, by="PheKey", keep = FALSE) %>% select(-INVESTIGATION_CODE) %>% unique()

tests <- Investigation_codes$INVESTIGATION_CODE_Description
#test <- ECDS_HBV_TESTS_ALL %>% mutate(ftccol = apply(ECDS_HBV_TESTS_ALL,1,function(x){any(x %in% "Full blood count (FBC)")}))

ECDS_HBV_TESTS_ALL_WIDE <- ECDS_HBV_TESTS_ALL

#Create a single new column for each type of test, then create flag to indicate if that test 
#carried out on each visit
for (test in tests){ 
 print(test)
y <- paste(test,sep="_")
ECDS_HBV_TESTS_ALL_WIDE[[y]]<- apply(ECDS_HBV_TESTS_ALL,1,function(x){any(x %in% test)})  }


#tidy column names 
ECDS_HBV_TESTS_ALL_WIDE <- ECDS_HBV_TESTS_ALL_WIDE %>% select(-contains("test_"))
#Replace true/false with 1/0
ECDS_HBV_TESTS_ALL_WIDE <- ECDS_HBV_TESTS_ALL_WIDE %>% mutate_if(is.logical, as.numeric)



#Create combined 'blood test' variable
ECDS_HBV_TESTS_ALL_WIDE <- ECDS_HBV_TESTS_ALL_WIDE %>% mutate(Blood_any =
                                                      case_when(Serology == 1 ~1,
                                                                `Urea & Electrolytes (U&Es)` ==1 ~ 1,
                                                                `Full blood count (FBC)` == 1 ~ 1,
                                                                `Liver function tests (LFTs)` ==1 ~ 1,
                                                                TRUE ~ 0))


ECDS_HBV_TESTS_ALL_WIDE <- ECDS_HBV_TESTS_ALL_WIDE %>% mutate(Blood_any_2 =
                                                                case_when(Serology == 1 ~1,
                                                                          `Urea & Electrolytes (U&Es)` ==1 ~ 1,
                                                                          `Full blood count (FBC)` == 1 ~ 1,
                                                                          `Liver function tests (LFTs)` == 1 ~1,
                                                                           Lactate ==1 ~1,
                                                                          `C reactive protein (CRP)`==1 ~1,
                                                                          `Clotting studies`==1 ~1,
                                                                          TRUE ~ 0))
                                                                                                                                                    
#join on the arrival dates and provider codes for grouping
all_linked_blood_tests <- ECDS_HBV_TESTS_ALL_WIDE %>% left_join(., attendances, by=c("TOKEN_PERSON_ID", "ARRIVAL_DATE", "PheKey"))


#produce some numbers
all_linked_blood_tests %>% group_by(Blood_any, Provider_derived, SITE) %>% summarise(n=n()) %>% write_clip()
all_linked_blood_tests %>% group_by(Blood_any_2, Provider_derived, SITE) %>% summarise(n=n())
all_linked_blood_tests %>% group_by(Blood_any,Blood_any_2) %>% summarise(n=n())

#look at blood tests by provider
tests_by_provider <- all_linked_blood_tests %>% 
  select(6:55,Provider_derived, Blood_any)%>% 
  group_by(Provider_derived) %>% 
  summarise(across(everything(), sum, na.rm=TRUE))%>% write_clip()

#look at blood tests by site
tests_by_provider_nonblood1 <- all_linked_blood_tests %>% 
  select(6:55,Provider_derived, SITE, Blood_any_2)%>% 
  filter(Blood_any_2 == 0) %>% 
  group_by(Provider_derived, SITE) %>% 
  summarise(across(everything(), sum, na.rm=TRUE))%>% write_clip()

#look at linked tests that don't meet definition of blood tests (as above)
tests_by_provider_nonblood2 <- all_linked_blood_tests %>% 
  select(6:55,Provider_derived, Blood_any)%>% 
  filter(Blood_any == 0) %>% 
  group_by(Provider_derived) %>% 
  summarise(across(everything(), sum, na.rm=TRUE))%>% write_clip()


#linked_non_blood1 <- ECDS_HBV_TESTS_ALL_WIDE %>% 
#  filter(Blood_any == 0) %>%  
#  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>% write_clip()

#linked_non_blood2 <- ECDS_HBV_TESTS_ALL_WIDE %>% 
#  filter(Blood_any_2 == 0) %>%  
#  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>% write_clip()

#x <- ECDS_HBV_TESTS_ALL_WIDE %>%
#  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))




#Keep only records defined as having a blood test
linked_blood_tests <- ECDS_HBV_TESTS_ALL_WIDE %>% select(PheKey, TOKEN_PERSON_ID, NHS_NUMBER, ARRIVAL_DATE, INVESTIGATION_DATE,
                                                         Blood_any) %>% filter(Blood_any == 1)

#Remove investigation date and keep arrival date to avoid duplication
linked_blood_tests  <-linked_blood_tests %>% select(-INVESTIGATION_DATE) %>% unique() 

#Join HBV test dates 
linked_blood_tests <- left_join(linked_blood_tests, HBV_tests_dates, by="NHS_NUMBER")


ECDS_HBV_JOINED <- left_join(ECDS_JOIN_PII, HBV_tests_NHSn, by="NHS_NUMBER") %>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         DATETEST = as.Date(DATETEST))



#######
#Create column for date range (investigation date + 14) inclusion to check whether
#tests were carried out during person's a+e visit --- questionable - work out why 14 days - this seems too long? Maybe 7 days instead
ECDS_HBV_JOINED <- ECDS_HBV_JOINED %>% mutate(max_test_date = ARRIVAL_DATE + 7,
                                              include = case_when(
    DATETEST < INVESTIGATION_DATE ~ "EXCLUDE",
    max_test_date < DATETEST ~ "EXCLUDE",
    DATETEST > max_test_date ~ "EXCLUDE",
    DATETEST == INVESTIGATION_DATE ~ "INCLUDE",
    FALSE ~ "INCLUDE"))