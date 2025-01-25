options(scipen=999999)
library(data.table)
library(clipr)
###Setup_tables script must be run before this one

## ECDS Reference tables
Investigation_codes <- ECDS_JOIN_PII %>% select(INVESTIGATION_CODE, INVESTIGATION_CODE_Description) %>% unique()
Provider_codes <- ECDS_JOIN_PII %>% select(SITE, PROVIDER_CODE_DERIVED, London_Provider) %>% unique()

attendances <- ECDS_JOIN_PII %>% select(PheKey, TOKEN_PERSON_ID, NHS_NUMBER, ARRIVAL_DATE, CHIEF_COMPLAINT_Description) %>% unique()
investigations <- ECDS_JOIN_PII %>% select(PheKey, TOKEN_PERSON_ID, NHS_NUMBER, ARRIVAL_DATE, INVESTIGATION_DATE, INVESTIGATION_CODE) %>% unique()


#Look at number of HCV tests per person
HCV_tests_NHS <- HCV_tests %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1) )
HCV_tests_LAB <- HCV_tests_NHS %>% group_by(PHLS, TESTMONTH) %>% summarise(n=n(),
                                                                          valid = sum(NA_tally)) %>% mutate(pct = valid/n*100)
HCV_tests_person <- HCV_tests_NHS %>% group_by(DUPE_SAMPLE, NHS_NUMBER, DATETEST) %>% summarise (n=n()) #%>% select(-n)
HCV_tests_NHSn <- HCV_tests_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, DATETEST)

HCV_tests_dates <- dcast(data.table(HCV_tests_NHSn), NHS_NUMBER~rowid(NHS_NUMBER, prefix="date"), value.var="DATETEST")

#Keep only first 4 tests
HCV_tests_dates <- HCV_tests_dates %>% select(NHS_NUMBER, date1, date2, date3, date4)

HCV_TESTED <- as.list(HCV_tests_NHSn[1])



### Prepare linkage tables

#Keep only ECDS records who we know have had HCV test (based on NHS no)
ECDS_HCV_TESTS <- subset(ECDS_JOIN_PII,NHS_NUMBER %in% HCV_TESTED$NHS_NUMBER) %>% unique()

#Quick summary of most common tests
SUMMARY_LINKED_TESTS <- ECDS_HCV_TESTS %>% group_by(INVESTIGATION_CODE_Description) %>% summarise(n=n())
write_clip(SUMMARY_LINKED_TESTS)

#widen data out so each test has own column, using PheKey NOT NHS number (so it accounts for diff visits)
#added the underscore to the prefic to support with column dropping later on
ECDS_HCV_TESTS_2 <- dcast(data.table(ECDS_HCV_TESTS),PheKey~rowid(PheKey, prefix="test_"), value.var="INVESTIGATION_CODE_Description")

#reorder data
ECDS_HCV_TESTS_2 <- ECDS_HCV_TESTS_2 %>% select(PheKey, test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_8, test_9, test_10, 
                                              test_11, test_12)  

#join back to original ECDS dataset

ECDS_HCV_TESTS_ALL <- left_join(ECDS_HCV_TESTS_2, investigations, by="PheKey", all.x=TRUE) %>% select(-INVESTIGATION_CODE) %>% unique()


tests <- Investigation_codes$INVESTIGATION_CODE_Description
test <- ECDS_HCV_TESTS_ALL %>% mutate(ftccol = apply(ECDS_HCV_TESTS_ALL,1,function(x){any(x %in% "Full blood count (FBC)")}))

ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL


for (test in tests){ 
 print(test)
y <- paste(test,sep="_")
ECDS_HCV_TESTS_ALL_WIDE[[y]]<- apply(ECDS_HCV_TESTS_ALL,1,function(x){any(x %in% test)})  }


ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% select(-contains("test_"))
ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% mutate_if(is.logical, as.numeric)

#Create combined 'blood test' variable

ECDS_HCV_TESTS_ALL_WIDE <- ECDS_HCV_TESTS_ALL_WIDE %>% mutate(Blood_any =
                                                      case_when(Serology == 1 ~1,
                                                                `Urea & Electrolytes (U&Es)` ==1 ~ 1,
                                                                `Full blood count (FBC)` == 1 ~ 1,
                                                                `Liver function tests (LFTs)` ==1 ~ 1,
                                                                TRUE ~ 0))



x <- ECDS_HCV_TESTS_ALL_WIDE %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))



#Join HCV test dates 
ECDS_HCV_JOINED <- left_join(ECDS_HCV_JOINED, HCV_tests_NHSn, by="NHS_NUMBER")



#Create column for date range (investigation date + 14)
ECDS_HIV_JOINED <- ECDS_HIV_JOINED %>% mutate(max_test_date = INVESTIGATION_DATE + 14,
                                              include = case_when(
    DATETEST < INVESTIGATION_DATE ~ "EXCLUDE",
    max_test_date < DATETEST ~ "EXCLUDE",
    DATETEST > max_test_date ~ "EXCLUDE",
    DATETEST = INVESTIGATION_DATE ~ "INCLUDE",
    FALSE ~ "INCLUDE"))

?between


#manipulate ED data into single rows

attendees <- ECDS_JOIN_PII %>% group_by(NHS_NUMBER) %>% summarise(n=n()) #%>% select(-n) %>% ungroup()

attendees2 = dcast(data.table(attendees), NHS_NUMBER~rowid(NHS_NUMBER, prefix="PheKey"), value.var="PheKey")

attendees_clean <- ECDS_JOIN_PII %>% group_by(NHS_NUMBER)  


investigations <- ECDS_JOIN_PII %>% 
  group_by(NHS_NUMBER, INVESTIGATION_CODE, INVESTIGATION_CODE_Description, INVESTIGATION_DATE) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  mutate(ID = row_number()) %>% 
  filter(!is.na(NHS_NUMBER))

investigations_unique <- investigations %>% select(-n, -ID) %>% unique()
investigations_person = dcast(data.table(investigations), NHS_NUMBER~rowid(NHS_NUMBER, prefix="TEST"), value.var="INVESTIGATION_DATE")

inv_codes <- ECDS_JOIN_PII %>% group_by(INVESTIGATION_CODE, INVESTIGATION_CODE_Description) %>% summarise(n=n())

