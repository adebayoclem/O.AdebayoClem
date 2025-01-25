#Table 3 of MasterTables_export.csv

##Load packages
if (!require(tidyverse)) install.packAges("tidyverse")
if (!require(lubridate)) install.packAges("lubridate")
if (!require(readxl)) install.packAges("readxl")
if (!require(DBI)) install.packAges("DBI")
if (!require(odbc)) install.packAges("odbc")
if (!require(arrow)) install.packAges("arrow")


# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())


# Load the HCV diagnoses table from Sentinel
# unlinked so includes all HCV diagnoses coded as ED for sentinel sites
HCV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelEDApril22_HCVnew
where FirstHCVtestdate <= '2023-04-07'AND (antiHCV like 'Positive' or HCVAg like 'Positive' or HCVRNA like 'Positive')
                                          and LIVE = 'Live'")


## data cleaning for HCV_DIAGNOSES_SENTINEL ########

# format dates  
HCV_DIAG_SENTINEL <- HCV_DIAGNOSES_SENTINEL %>% 
  mutate(antiHCVdate =as.Date(antiHCVdate, format = '%Y-%m-%d'),
         HCVRNAdate = as.Date(HCVRNAdate, format = '%Y-%m-%d'),
         FirstHCVtestdate = as.Date(FirstHCVtestdate, format = '%Y-%m-%d'),
         EED = as.Date(EED, format = '%Y-%m-%d'),
         preRxdate = as.Date(preRxdate, format = '%Y-%m-%d'),
         postRxdate = as.Date(postRxdate, format = '%Y-%m-%d'),
         mincreatedate = as.Date(mincreatedate, format = '%Y-%m-%d'),
         minreferraldate = as.Date(minreferraldate, format = '%Y-%m-%d'),
         earliestRxdate = as.Date(earliestRxdate, format = '%Y-%m-%d'),
         minsentineldate = as.Date(minsentineldate, format = '%Y-%m-%d'),
         DOB = as.Date(DOB, format = '%Y-%m-%d'))

# lower case 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, antiHCV = tolower(antiHCV)) 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, HCVRNA = tolower(HCVRNA)) 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, RNA_all = tolower(RNA_all)) 

# Replace "15-24" with "16-24" in the age group column
HCV_DIAG_SENTINEL$agegroup[HCV_DIAG_SENTINEL$agegroup == "15-24"] <- "16-24"

#Rename age category in agegroup column
HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(agegroup = str_replace_all(agegroup, "-", " to "),
         agegroup = str_replace_all(agegroup, "80\\+", "80 and over"),
         agegroup = str_replace_all(agegroup, "(\\d+)(to)(\\d+)", "\\1 to \\3"))

# Rename gender variable in the sex column
HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(sex = replace(sex,sex== 1, "Men")) %>% 
  mutate(sex = replace(sex,sex== 2, "Women")) 
  

#####Create Table 3######

#Total RNA Positive

RNA_pos <- HCV_DIAG_SENTINEL %>%
  filter(RNA_all == "positive") %>% 
  mutate(var = replace(RNA_all,RNA_all== "positive", "Yes")) %>% 
  group_by(var) %>%
  count() %>% 
  rename("Total RNA positive" = n)

#Total linked to care (all)
HCV_linkedtx <- HCV_DIAG_SENTINEL %>%
  filter(RNA_all =="positive" & linkedintorxpost == "Yes") %>% 
  group_by(linkedintorxpost) %>% 
  rename(var = linkedintorxpost) %>% 
  count() %>% 
  rename("Total linked (all)" = n)
  
#Proportion linked
proportionlinked <- HCV_linkedtx$"Total linked (all)" / RNA_pos$"Total RNA positive"

proportionlinked <- data.frame(var = RNA_pos$var, proportionlinked = proportionlinked) %>%
    rename("Proportion linked" = proportionlinked)

print(proportionlinked)


#Total linked to care within 28 days
HCV_linked28 <- HCV_DIAG_SENTINEL %>%
  filter(HCVRNA == "positive" & RX28days == "Yes") %>% 
  group_by(RX28days) %>% 
  rename(var = RX28days) %>% 
  count() %>% 
  rename("Total linked (28 days)" = n)

#Proportion linked (28 days)
proportionlinked28 <- HCV_linked28$"Total linked (28 days)" / RNA_pos$"Total RNA positive"

proportionlinked28 <- data.frame(var = RNA_pos$var, proportionlinked28 = proportionlinked28) %>%
    rename("Proportion linked (28 days)" = proportionlinked28)


print(proportionlinked28)

#New RNA positives
RNA_newpos <- HCV_DIAG_SENTINEL %>%
  filter(rna_positive_new == "New") %>% 
  mutate(var = replace(rna_positive_new, rna_positive_new== "New", "Yes")) %>% 
  group_by(var) %>% 
  count() %>% 
  rename("New RNA positives" = n)

#Newly diagnosed AND linked to care
RNA_newpos_linkedtx <- HCV_DIAG_SENTINEL %>%
  mutate(var = if_else(rna_positive_new == "New" & linkedintorxpost == "Yes", "Yes", "No")) %>% 
  filter(var == "Yes") %>% 
  group_by(var) %>% 
  count() %>% 
  rename("Newly diagnosed and linked to care (all)" = n)
  
#Proportion of new diagnoses linked
proportion_newlinked <- RNA_newpos_linkedtx$"Newly diagnosed and linked to care (all)" / RNA_newpos$"New RNA positives"

proportion_newlinked <- data.frame(var = RNA_newpos_linkedtx$var, proportion_newlinked = proportion_newlinked) %>%
    rename("Proportion of new diagnoses linked to care" = proportion_newlinked)

print(proportion_newlinked)

#Newly diagnosed AND linked to care (28 days)
RNA_newpos_linked28 <- HCV_DIAG_SENTINEL %>%
  mutate(var = if_else(rna_positive_new == "New" & RX28days == "Yes", "Yes", "No")) %>% 
  filter(var == "Yes") %>% 
  group_by(var) %>% 
  count() %>% 
  rename("Newly diagnosed and linked to care (28 days)" = n)
  
#Proportion of new diagnoses linked (28 days)
proportion_newlinked28 <- RNA_newpos_linked28$"Newly diagnosed and linked to care (28 days)" / RNA_newpos$"New RNA positives"

proportion_newlinked28 <- data.frame(var = RNA_newpos_linked28$var, proportion_newlinked28 = proportion_newlinked28) %>%
    rename("Proportion of new diagnoses linked to care (28 days)" = proportion_newlinked28)

print(proportion_newlinked28)

#Previously diagnosed and not currently in care
lost_diagnosed <- HCV_DIAG_SENTINEL %>%
  mutate(var = if_else(lost_diagnosed == 'Yes', "Yes", "No")) %>% 
  filter(var == "Yes") %>% 
  group_by(var) %>% 
  count() %>% 
  rename("Previously diagnosed and not in care" = n)

#reengaged
reengaged <- HCV_DIAG_SENTINEL %>%
  mutate(var = if_else(reengaged == 'Yes', "Yes", "No")) %>% 
  filter(var == "Yes") %>% 
  group_by(var) %>% 
  count() %>% 
  rename("Reengaged (all)" = n)
  
#Proportion previously diagnosed and not in care reengaged
proportion_reengaged <- reengaged$"Reengaged (all)" / lost_diagnosed$"Previously diagnosed and not in care"

proportion_reengaged <- data.frame(var = reengaged$var, proportion_reengaged = proportion_reengaged) %>%
  rename("Proportion of previously diagnosed and not in care reengaged" = proportion_reengaged)

print(proportion_reengaged)

### reengaged 28 days
reengaged28 <- HCV_DIAG_SENTINEL %>%
  mutate(var = if_else(reengaged == 'Yes' & RX28days == "Yes", "Yes", "No")) %>% 
  filter(var == "Yes") %>% 
  group_by(var) %>% 
  count() %>% 
  rename("Reengaged (28 days)" = n)

#Proportion previously diagnosed and not in care reengaged (28 days)
proportion_reengaged28 <- reengaged28$"Reengaged (28 days)" / lost_diagnosed$"Previously diagnosed and not in care"

proportion_reengaged28 <- data.frame(var = reengaged28$var, proportion_reengaged28 = proportion_reengaged28) %>%
  rename("Proportion previously diagnosed and not in care linked within 28 days)" = proportion_reengaged28)

print(proportion_reengaged28)

#Previously treated and cleared infection (reinfections)
previously_cleared <- HCV_DIAG_SENTINEL %>% 
  mutate(var = if_else(RNA_all== "positive" & preRxSVR == "Yes", "Yes", "No"))%>%
  filter(var == "Yes") %>% 
  group_by(var) %>% 
  count() %>% 
  rename("Indicator 8: Detection rate for HCV reinfection" = n)


#Indicator 13b: median time between diagnosis and linkage to care - HCV

medianHCV_diaglinkagetx <- median(HCV_DIAG_SENTINEL$RXtiming, na.rm =TRUE)

indicator13b <- data.frame(var = "Yes", n = medianHCV_diaglinkagetx) %>% 
  rename("Indicator 13b: median time between diagnosis and linkage to care - HCV" = n)


###
## to bind cols together at the end, we can use a left join
###
joined_table3 <- RNA_pos %>% 
  left_join(HCV_linkedtx, by = "var") %>% 
  left_join(proportionlinked, by = "var") %>% 
  left_join(HCV_linked28, by = "var") %>% 
  left_join(proportionlinked28, by = "var") %>% 
  left_join(RNA_newpos, by = "var") %>% 
  left_join(RNA_newpos_linkedtx, by = "var") %>% 
  left_join(proportion_newlinked, by = "var") %>% 
  left_join(RNA_newpos_linked28, by = "var") %>% 
  left_join(proportion_newlinked28, by = "var") %>% 
  left_join(lost_diagnosed, by = "var") %>% 
  left_join(reengaged, by = "var") %>% 
  left_join(proportion_reengaged, by = "var") %>% 
  left_join(reengaged28, by = "var") %>% 
  left_join(proportion_reengaged28, by = "var") %>% 
  left_join(previously_cleared, by = "var") %>%
  left_join(indicator13b, by = "var") %>% 
  mutate(breakdown = case_when(
    var == "Yes" ~ "Total")) %>% 
  relocate(breakdown, .before = var) %>%
  view()

#read in HIV data
hiv_data3 <- read_csv(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/hiv_indicator_tab_3.csv"
) %>%
  rename(var = Group,
         breakdown = "Breakdown type") %>%
  filter(var == "All") %>% 
  mutate(var = replace(var, var == "All", "Yes"))

# join with joined_table
joined_table3_hiv <- joined_table3 %>%
  left_join(hiv_data3, by = c("breakdown", "var")) %>%
  rename("Indicator 9a: Detection rate for PLWHIV previously in care and no longer attending" = "Detection rate for those previously in care and no longer attending HIV services (per 100 previous diagnoses)") %>%
  relocate(26, .after = 21) %>% 
  relocate(27, .after = 22) %>% 
  view()



