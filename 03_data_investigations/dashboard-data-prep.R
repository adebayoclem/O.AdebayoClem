# load or install required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")


data <- read.csv("All_data.csv")

# get Hep B diagnoses data
hepb_diag <- data%>%
  filter(Metric.Name == 'Positive Hep B antigen tests')

# concatenate the month and year into one field 
hepb_diag$Date <- with(hepb_diag, sprintf("%d-%02s", Year.of.Reporting.Period.Start,Month.of.Reporting.Period.Start))

#remove dates after March 2023
hepb_diag <-hepb_diag %>%
  filter(!Date %in% c('2023-April', '2023-May', '2023-June'))

# get totals by trust
totals_site <- hepb_diag%>%
  group_by(Site.Hospital.Name)%>%
  mutate (total = sum(Metric.or.proportion..Integer.))%>%
  distinct(Site.Hospital.Name, total)

write.csv(totals_site, "nhse-hbv-site.csv")
# repeat for Hep C

hepc_diag <- data%>%
  filter(Metric.Name == 'Positive Hep C antibody tests')

hepc_diag$Date <- with(hepc_diag, sprintf("%d-%02s", Year.of.Reporting.Period.Start,Month.of.Reporting.Period.Start))

hepc_diag <-hepc_diag %>%
  filter(!Date %in% c('2023-April', '2023-May', '2023-June'))
         
totals_hepc <- hepc_diag%>%
group_by(Provider.Name)%>%
mutate (total = sum(Metric.or.proportion..Integer.))%>%
distinct(Provider.Name, total)
         