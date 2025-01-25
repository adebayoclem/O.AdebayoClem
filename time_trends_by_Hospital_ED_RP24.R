#load or install required packages

# Ensure the "pacman" package is installed
if (!require("pacman")) {
  install.packages("pacman") }

# Load necessary packages using pacman
#currently taken from Report site package list with everything additional commented out until use
pacman::p_load(
  # rio,        # For data import/export
  here,       # For relative file paths
  # skimr,      # For summarizing data
  janitor,    # For data cleaning
  lubridate,  # For date handling
  #epikit,     # For epidemiological tools
tsibble,
  #flextable,  # For creating pretty tables
  scales,     # For scaling functions
  gtsummary,  # For summary statistics and tables
  arrow,      # For data interoperability
  # ggplot2,    # For data visualization
  # rlang,      # For programming tools
  readxl,     # For reading Excel files
  # writexl,    # For writing Excel files
  DBI,        # For database connections
  odbc,       # For ODBC connections
  purrr,      # For functional programming
  tidyverse  # For data manipulation and visualization
  
)



# Establish ODBC connections -----------------------------------------------

#ESTABLISH ODBC CONNECTION WITH DATA
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())

#Load data from Sentinel 

# load ED_RP24 table
ED_RP24 <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT [SpecimenDate],
[Hospital],
[HIV],
[HCV_Antigen], 
[HCV_Antibody],
[RNAResult],
[HBV_Antigen]
FROM [Y006_BBV_PID].[dbo].[ED_RP24]
WHERE (SpecimenDate <= '2024-03-31' and Waves like 'Wave 1')"
)

ED_RP24$SpecimenDate <- as.Date(ED_RP24$SpecimenDate)

#add year and month column
glimpse(testing$yearmonth)

testing <- ED_RP24 %>% 
  mutate(yearmonth = floor_date(SpecimenDate, unit = "month")) %>%   
  arrange(yearmonth)

###  HIV test count
hiv_tests <- testing %>% 
  filter(!is.na(HIV)) %>%
  group_by(yearmonth, Hospital) %>%
  tally() %>% 
  collect() %>%
  rename(tests = n) %>%
  mutate(event = "HIV test")

### HCV test count
hcv_tests <- testing %>% 
  filter(!is.na(HCV_Antigen) | !is.na(HCV_Antibody) | !is.na(RNAResult)) %>%
  group_by(yearmonth, Hospital) %>%
  tally() %>% 
  collect() %>%
  rename(tests = n) %>%
  mutate(event = "HCV test")


###  HBV test count
hbv_tests<- testing %>% 
  filter(!is.na(HBV_Antigen)) %>%
  group_by(yearmonth, Hospital) %>%
  tally() %>% 
  collect() %>%
  rename(tests = n) %>%
  mutate(event = "HBV test")

#group by sites
#(1)
BLA_CRO <- hiv_tests %>% 
  filter(Hospital %in% c("BLACKPOOL VICTORIA HOSPITAL", "CHARING CROSS HOSPITAL", "CHELSEA AND WESTMINSTER HOSPITAL", 
                         "CROYDON UNIVERSITY HOSPITAL"))

BLA_CRO_p <- ggplot(BLA_CRO, aes(x= yearmonth,
                y = tests, color = Hospital))+
  geom_line() +
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HIV tests April 2022 - March 2024",
       x= "Time", 
       y= "HIV test numbers")

ggsave("BLA_CRO_p.png", plot = BLA_CRO_p, width = 12, height = 6, bg="white")

## (2)
HIL_MRI <- hiv_tests %>% 
  filter(Hospital %in% c("HILLINGDON HOSPITAL", "HOMERTON UNIVERSITY HOSPITAL", "KINGS COLLEGE HOSPITAL", "KINGSTON HOSPITAL",	
                         "MANCHESTER ROYAL INFIRMARY"))

HIL_MRI_p <-ggplot(HIL_MRI, aes(x= yearmonth,
                y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HIV tests April 2022 - March 2024",
       x= "Time", 
       y= "HIV test numbers")


ggsave("HIL_MRI_p.png", plot = HIL_MRI_p, width = 12, height = 6, bg="white")

#(3)
NEW_STG <- hiv_tests %>% 
  filter(Hospital %in% c("NEWHAM UNIVERSITY HOSPITAL", "PRINCESS ROYAL UNIVERSITY HOSPITAL", "QUEEN ELIZABETH HOSPITAL", 
                         "ROYAL SUSSEX COUNTY HOSPITAL", "ST GEORGES HOSPITAL"))

NEW_STG_p <- ggplot(NEW_STG, aes(x= yearmonth, breaks = 1,
                y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HIV tests April 2022 - March 2024",
       x= "Time", 
       y= "HIV test numbers") #+
  #theme(legend.position="bottom")

ggsave("NEW_STG_p.png", plot = NEW_STG_p, width = 12, height = 6, bg="white")

#(4)
STM_UHL <- hiv_tests %>% 
  filter(Hospital %in% c("ST MARYS HOSPITAL", "ST THOMAS HOSPITAL", 	
                         "THE ROYAL LONDON HOSPITAL", "UNIVERSITY HOSPITAL LEWISHAM"))

STM_UHL_p <- ggplot(STM_UHL, aes(x= yearmonth,
                  y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HIV tests April 2022 - March 2024",
       x= "Time", 
       y= "HIV test numbers")

ggsave("STM_UHL_p.png", plot = STM_UHL_p, width = 12, height = 6, bg="white")

#(5)
WMX_WYT <- hiv_tests %>% 
  filter(Hospital %in% c("WEST MIDDLESEX UNIVERSITY HOSPITAL", "WHIPPS CROSS HOSPITAL", 	
                         "WYTHENSHAWE HOSPITAL"))

WMX_WYT_p <- ggplot(WMX_WYT, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HIV tests April 2022 - March 2024",
       x= "Time", 
       y= "HIV test numbers") 

ggsave("WMX_WYT_p.png", plot = WMX_WYT_p, width = 12, height = 6, bg="white")


###----------------------------- HBV------------------------------------------------- ###

BLA_CRO <- HBV_tests %>% 
  filter(Hospital %in% c("BLACKPOOL VICTORIA HOSPITAL", "CHARING CROSS HOSPITAL", "CHELSEA AND WESTMINSTER HOSPITAL", 
                         "CROYDON UNIVERSITY HOSPITAL"))

ggplot(BLA_CRO, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line() +
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HBV tests April 2022 - March 2024",
       x= "Time", 
       y= "HBV test numbers")

## (2)
HIL_MRI <- HBV_tests %>% 
  filter(Hospital %in% c("HILLINGDON HOSPITAL", "HOMERTON UNIVERSITY HOSPITAL", "KINGS COLLEGE HOSPITAL", "KINGSTON HOSPITAL",	
                         "MANCHESTER ROYAL INFIRMARY"))

ggplot(HIL_MRI, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HBV tests April 2022 - March 2024",
       x= "Time", 
       y= "HBV test numbers")

#(3)
NEW_STG <- HBV_tests %>% 
  filter(Hospital %in% c("NEWHAM UNIVERSITY HOSPITAL", "PRINCESS ROYAL UNIVERSITY HOSPITAL", "QUEEN ELIZABETH HOSPITAL", 
                         "ROYAL SUSSEX COUNTY HOSPITAL", "ST GEORGES HOSPITAL"))

ggplot(NEW_STG, aes(x= yearmonth, breaks = 1,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HBV tests April 2022 - March 2024",
       x= "Time", 
       y= "HBV test numbers") #+
#theme(legend.position="bottom")

#(4)
STM_UHL <- HBV_tests %>% 
  filter(Hospital %in% c("ST MARYS HOSPITAL", "ST THOMAS HOSPITAL", 	
                         "THE ROYAL LONDON HOSPITAL", "UNIVERSITY HOSPITAL LEWISHAM"))

ggplot(STM_UHL, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HBV tests April 2022 - March 2024",
       x= "Time", 
       y= "HBV test numbers")

#(5)
WMX_WYT <- HBV_tests %>% 
  filter(Hospital %in% c("WEST MIDDLESEX UNIVERSITY HOSPITAL", "WHIPPS CROSS HOSPITAL", 	
                         "WYTHENSHAWE HOSPITAL"))

ggplot(WMX_WYT, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HBV tests April 2022 - March 2024",
       x= "Time", 
       y= "HBV test numbers") 


###----------------------------- HBV------------------------------------------------- ###

BLA_CRO <- HCV_tests %>% 
  filter(Hospital %in% c("BLACKPOOL VICTORIA HOSPITAL", "CHARING CROSS HOSPITAL", "CHELSEA AND WESTMINSTER HOSPITAL", 
                         "CROYDON UNIVERSITY HOSPITAL"))

ggplot(BLA_CRO, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line() +
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HCV tests April 2022 - March 2024",
       x= "Time", 
       y= "HCV test numbers")

## (2)
HIL_MRI <- HCV_tests %>% 
  filter(Hospital %in% c("HILLINGDON HOSPITAL", "HOMERTON UNIVERSITY HOSPITAL", "KINGS COLLEGE HOSPITAL", "KINGSTON HOSPITAL",	
                         "MANCHESTER ROYAL INFIRMARY"))

ggplot(HIL_MRI, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HCV tests April 2022 - March 2024",
       x= "Time", 
       y= "HCV test numbers")

#(3)
NEW_STG <- HCV_tests %>% 
  filter(Hospital %in% c("NEWHAM UNIVERSITY HOSPITAL", "PRINCESS ROYAL UNIVERSITY HOSPITAL", "QUEEN ELIZABETH HOSPITAL", 
                         "ROYAL SUSSEX COUNTY HOSPITAL", "ST GEORGES HOSPITAL"))

ggplot(NEW_STG, aes(x= yearmonth, breaks = 1,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HCV tests April 2022 - March 2024",
       x= "Time", 
       y= "HCV test numbers") #+
#theme(legend.position="bottom")

#(4)
STM_UHL <- HCV_tests %>% 
  filter(Hospital %in% c("ST MARYS HOSPITAL", "ST THOMAS HOSPITAL", 	
                         "THE ROYAL LONDON HOSPITAL", "UNIVERSITY HOSPITAL LEWISHAM"))

ggplot(STM_UHL, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HCV tests April 2022 - March 2024",
       x= "Time", 
       y= "HCV test numbers")

#(5)
WMX_WYT <- HCV_tests %>% 
  filter(Hospital %in% c("WEST MIDDLESEX UNIVERSITY HOSPITAL", "WHIPPS CROSS HOSPITAL", 	
                         "WYTHENSHAWE HOSPITAL"))

ggplot(WMX_WYT, aes(x= yearmonth,
                    y = tests, color = Hospital))+
  geom_line()+
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y") +
  labs(title= "HCV tests April 2022 - March 2024",
       x= "Time", 
       y= "HCV test numbers") 



