############################## GRAPHS ##############################

# load or install required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")
if (!require(pacman)) install.packages("pacman")
if (!require(EpiFunc)) install.packages("EpiFunc")
if (!require(janitor)) install.packages("janitor")
if (!require(gtsummary)) install.packages("gtsummary")
if (!require(scales)) install.packages("scales")
if (!require(forcats)) install.packages("forcats")

# Read in data
source("mastertable.R")
source("Table_2.R")
source("Table_3.R")
source("theme_ukhsa.R")
source("ukhsa_colours.R")

# display numbers

# standardise variables in joined tables
colnames(joined_table_hiv) = gsub(" ", "_", colnames(joined_table_hiv))
colnames(joined_table_hiv) = gsub(":", "", colnames(joined_table_hiv))
colnames(joined_table_hiv) = gsub("[(]", "", colnames(joined_table_hiv))
colnames(joined_table_hiv) = gsub("[)]", "", colnames(joined_table_hiv))

joined_table_5sites <- joined_table_hiv %>%
  mutate(var = recode(var,
    "FEMALE" = "Female",
    "MALE" = "Male",
    "Indian, Pakistani or Bangladeshi" = "Asian"
  )) %>%
  filter(var != "Unknown Sex" &
    var != "Unknown" &
    var != "Unknown IMD") %>%
    mutate(indicator_7b_percent = Indicator_7b_Proportion_of_patients_who_test_positive_for_HCV * 100,
    indicator_7c_percent = Indicator_7c_Proportion_of_patients_who_test_positive_for_HBV * 100,
    indicator_6b_percent = Indicator_6b_HCV_new_diagnosis_rate * 100,
    indicator_6c_percent = Indicator_6c_HBV_new_diagnosis_rate * 100)

joined_table_sites <- joined_table4 %>%
  mutate(var = recode(var,
    "FEMALE" = "Female",
    "MALE" = "Male",
    "Indian, Pakistani or Bangladeshi" = "Asian"
  )) %>%
  filter(var != "Unknown Sex" &
    var != "Unknown Age" &
    var != "Unknown" &
    var != "Unknown IMD")

# create diagnosis type variable in hiv data
hiv_diags <- joined_table_5sites %>%
  select(
    breakdown, 
    var, 
    Indicator_7a_Proportion_of_patients_who_test_positive_for_HIV, 
    Indicator_6a_HIV_new_diagnosis_rate) %>%
  rename(
    "All diagnoses" = Indicator_7a_Proportion_of_patients_who_test_positive_for_HIV,
    "New diagnoses" = Indicator_6a_HIV_new_diagnosis_rate
  ) %>%
  gather(diagnosis_type, proportion, "All diagnoses":"New diagnoses")

# if reading data in manually
# joined_table <- read_excel(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/MasterTables_export.xlsx"
# )

# joined_table4 <- read_excel(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/MasterTables_export.xlsx",
#  sheet = "Table 4"
# )

# attendees breakdowns
## 5 sites data
attendees_5sites <- as.data.frame(joined_table_5sites) %>%
  select(breakdown, var, Number_of_patients_attending_ED_HIV_got_live_dates) %>%
  mutate(site_type = "5 sites") %>%
  rename(num_attendees = Number_of_patients_attending_ED_HIV_got_live_dates) %>%
  group_by(breakdown) %>%
  mutate(proportion = num_attendees / sum(num_attendees) * 100)

## all sites data
attendees_by_site <- as.data.frame(joined_table_sites) %>%
  select(breakdown:"Number of attendees - Outside London sites") %>%
  gather(
    site_type, num_attendees,
    "Number of attendees - all sites":"Number of attendees - Outside London sites"
  ) %>%
  transform(site_type = str_replace(site_type, "Number of attendees - ", "")) %>%
  mutate(site_type = recode(site_type, "all sites" = "All sites")) %>%
  group_by(breakdown, site_type) %>%
  mutate(proportion = num_attendees / sum(num_attendees) * 100)

### join 5 sites and all sites for age
attendees_joined <- bind_rows(attendees_by_site, attendees_5sites)

# blood tests
## 5 sites
bloodtests_5 <- as.data.frame(joined_table_5sites) %>%
  select(breakdown, var, Indicator_1a_Proportion_of_patients_attending_ED_who_have_a_blood_test) %>%
  rename(proportion = Indicator_1a_Proportion_of_patients_attending_ED_who_have_a_blood_test) %>%
  mutate(
    site_type = "5 sites",
    proportion = proportion * 100
  )

## all sites
bloodtests_all <- as.data.frame(joined_table_sites) %>%
  select(breakdown, var, proportion1A_all:proportion1A_outside) %>%
  gather(
    site_type, proportion,
    proportion1A_all:proportion1A_outside
  ) %>%
  transform(site_type = str_replace(site_type, "proportion1A_", "")) %>%
  mutate(
    site_type = recode(site_type, "all" = "All sites"),
    site_type = recode(site_type, "sentinel" = "Sentinel sites"),
    site_type = recode(site_type, "ldn" = "London sites"),
    site_type = recode(site_type, "outside" = "Outside London sites"),
    proportion = proportion * 100
  )

### join all sites and 5 sites for age
bloodtests_joined <- bind_rows(bloodtests_all, bloodtests_5)

# BBV tests (all attendees)
#bbvtests_all_attendees <- as.data.frame(joined_table_5sites) %>%
#  rename(
#    "HIV_test_no" = "Number of patients who have a blood test and HIV test",
#    "HCV_test_no" = "Number of patients who have a blood test and HCV test",
#    "HBV_test_no" = "Number of patients who have a blood test and HBV test",
#    "ED_attendees_HIV" = "Number of patients attending ED (HIV got live dates)",
#    "ED_attendees_HCV" = "Number of patients attending ED (HCV got live dates)",
#    "ED_attendees_HBV" = "Number of patients attending ED (HBV got live dates)"
#  ) %>%
#  mutate(
#    HIV = (
#      HIV_test_no / ED_attendees_HIV
#    ) * 100,
#    HCV = (
#      HCV_test_no / ED_attendees_HCV
#    ) * 100,
#    HBV = (
#      HBV_test_no / ED_attendees_HBV
#    ) * 100
#  ) %>%
#  select(breakdown, var, HIV, HCV, HBV) %>%
#  gather(BBV, test_uptake, HIV:HBV)

# BBV tests (attendees that had a blood test)
bbvtests_bt <- joined_table_5sites %>%
  select(breakdown, 
  var, 
  'Indicator_2a_Proportion_of_patients_with_a_blood_test_who_have_an_HIV_test', 
  'Indicator_2b_Proportion_of_patients_with_a_blood_test_who_have_an_HCV_test',
  'Indicator_2c_Proportion_of_patients_with_a_blood_test_who_have_an_HBV_test') %>%
  rename(
    HIV = "Indicator_2a_Proportion_of_patients_with_a_blood_test_who_have_an_HIV_test",
    HCV = "Indicator_2b_Proportion_of_patients_with_a_blood_test_who_have_an_HCV_test",
    HBV = "Indicator_2c_Proportion_of_patients_with_a_blood_test_who_have_an_HBV_test"
  ) %>%
  gather(BBV, test_uptake, HIV:HBV) %>%
  mutate(test_uptake = test_uptake * 100)

# test uptake by site
test_uptake_by_site <- read_excel("//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/site_uptake_chart_data.xlsx") %>%
gather(BBV, test_uptake, "HIV percent":"HBV percent") %>%
transform(BBV = str_replace(BBV, " percent", ""))

# BBV positivity (new/existing diagnoses)
bbv_positivity <- joined_table_5sites %>%
select(
  var,
  breakdown,
  Indicator_7a_Proportion_of_patients_who_test_positive_for_HIV, 
  Indicator_7b_Proportion_of_patients_who_test_positive_for_HCV, 
  Indicator_7c_Proportion_of_patients_who_test_positive_for_HBV,
  Indicator_6a_HIV_new_diagnosis_rate,
  Indicator_6b_HCV_new_diagnosis_rate,
  Indicator_6c_HBV_new_diagnosis_rate
  ) %>%
  rename(
    HIV_all = Indicator_7a_Proportion_of_patients_who_test_positive_for_HIV,
    HCV_all = Indicator_7b_Proportion_of_patients_who_test_positive_for_HCV,
    HBV_all = Indicator_7c_Proportion_of_patients_who_test_positive_for_HBV,
    HIV_new = Indicator_6a_HIV_new_diagnosis_rate,
    HCV_new = Indicator_6b_HCV_new_diagnosis_rate,
   HBV_new = Indicator_6c_HBV_new_diagnosis_rate
  ) %>%
  gather(BBV, positivity, HIV_all:HBV_new) %>%
  mutate(diagnosis_type = case_when(BBV %in% c(
    "HIV_all",
    "HCV_all",
    "HBV_all"
  ) ~ "All diagnoses",
  BBV %in% c(
    "HIV_new",
    "HCV_new",
    "HBV_new") ~ "New diagnoses")) %>%
  mutate(positivity = positivity * 100)

bbv_positivity <- bbv_positivity %>% 
  transform(BBV = str_replace(BBV, "_all", "")) %>%
  transform(BBV = str_replace(BBV, "_new", "")) 

# Linkage to care
## HCV linkage
hcv_linkage <- joined_table3 %>%
mutate(`New RNA positives` = recode(`New RNA positives`, if_else(
  is.na(`New RNA positives`) == TRUE, 0, `New RNA positives`, 0
)))
gather(outcome, number, "Total RNA positive":"Number of HCV positive patients who were previously but not currently in care who attend care")  %>%
view()

# create theme
theme1 <- function() {
  theme_ukhsa() %+replace%
    ggplot2::theme(legend.position = "left")
}

theme2 <- function() {
  theme_ukhsa() %+replace%
    ggplot2::theme(legend.position = "none")
}

# set colour palette
pal6 <- ukhsa_colours("ukhsa_teals", 6)
pal_hc <- ukhsa_colours("high_contrast")
pal5 <- ukhsa_colours("ukhsa_teals", 5)
pal9 <- ukhsa_colours("ukhsa_teals", 9)
pal3 <- ukhsa_colours("ukhsa_teals", 3)

# 1. Demographic breakdowns of ED attendees by site type
# age_attendees <-
#  ggplot(
#    attendees_age,
#    aes(x = site_type, y = proportion, group = order(var))
#  ) +
#  scale_fill_manual(values = pal1) +
#  geom_col(aes(fill = var), position = "dodge2") +
#  labs(x = "Site type", y = "Proportion of attendees (%)", title = "Proportion of ED attendees by age group and site type") +
#  guides(fill=guide_legend(title="Age group")) +
#  coord_flip() +
#  theme_ukhsa()
# age_attendees

# ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/age_attendees2.svg",
#  plot = age_attendees,
#  width=960,
#  height=640,
#  units="px",
#  dpi=72
# )

eth_attendees <-
  ggplot(
    filter(attendees_by_site, breakdown == "Ethnic group"),
    aes(x = site_type, y = proportion, group = forcats::fct_rev(reorder(var, var)))
  ) +
  scale_fill_manual(values = pal9) +
  geom_col(aes(fill = var), position = "dodge2") +
  labs(x = "Site type", y = "Proportion of attendees (%)", title = "Proportion of ED attendees by ethnic group and site type") +
  coord_flip() +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme_ukhsa()
eth_attendees

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/eth_attendees.svg",
  plot = eth_attendees,
  width = 960,
  height = 960,
  units = "px",
  dpi = 72
)

## IF want to do pie charts
pacman::p_load(
  patchwork,
  scales,
  ggrepel
)

## separate out data
attendees_eth_all_sites <- attendees_by_site %>%
  filter(breakdown == "Ethnic group" & site_type == "All sites") %>%
  mutate(proportion = round(proportion, digits = 0)) %>%
  arrange(-proportion)

attendees_eth_sentinel <- attendees_by_site %>%
  filter(breakdown == "Ethnic group" & site_type == "Sentinel sites") %>%
  mutate(proportion = round(proportion, digits = 0)) %>%
  arrange(-proportion)

attendees_eth_ldn <- attendees_by_site %>%
  filter(breakdown == "Ethnic group" & site_type == "London sites") %>%
  mutate(proportion = round(proportion, digits = 0)) %>%
  arrange(-proportion)

attendees_eth_outside <- attendees_by_site %>%
  filter(breakdown == "Ethnic group" & site_type == "Outside London sites") %>%
  mutate(proportion = round(proportion, digits = 0)) %>%
  arrange(-proportion)

attendees_eth_5 <- attendees_by_site %>%
  filter(breakdown == "Ethnic group" & site_type == "5 sites") %>%
  mutate(proportion = round(proportion, digits = 0)) %>%
  arrange(-proportion)

### All sites
pie1 <- ggplot(attendees_eth_all_sites, aes(x = "", y = proportion, fill = var)) +
  geom_bar(data = attendees_eth_all_sites, stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, title = "All sites") +
  geom_label_repel(
    data = attendees_eth_all_sites,
    aes(y = proportion, label = paste0(proportion, "%")),
    size = 4.5, nudge_x = 1, show.legend = FALSE
  ) +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme1() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = pal_hc)
pie1

### Sentinel
pie2 <- ggplot(attendees_eth_sentinel, aes(x = "", y = proportion, fill = var)) +
  geom_bar(data = attendees_eth_sentinel, stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, title = "Sentinel sites") +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme2() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = pal_hc)


### London
pie3 <- ggplot(attendees_eth_ldn, aes(x = "", y = proportion, fill = var)) +
  geom_bar(data = attendees_eth_ldn, stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, title = "London sites") +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme2() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = pal_hc)


### Outside
pie4 <- ggplot(attendees_eth_outside, aes(x = "", y = proportion, fill = var)) +
  geom_bar(data = attendees_eth_outside, stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, title = "Outside London sites") +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme2() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = pal_hc)


### 5 sites
pie5 <- ggplot(attendees_eth_5, aes(x = "", y = proportion, fill = var)) +
  geom_bar(data = attendees_eth_5, stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, title = "5 sites") +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme2() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = pal_hc)

### Add together
pies <- pie1 + pie2 + pie3 + pie4 + pie5 +
  plot_layout(ncol = 2)

pies

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/pies.svg",
  plot = pies,
  width = 960,
  height = 960,
  units = "px",
  dpi = 72
)

imd_attendees <-
  ggplot(filter(attendees_by_site, breakdown == "IMD"), aes(
    x = site_type, y = proportion, group = forcats::fct_rev(reorder(var, var))
  )) +
  scale_fill_manual(values = pal5) +
  geom_col(aes(fill = var), position = "dodge2") +
  labs(x = "Site type", y = "Proportion of attendees (%)", title = "Proportion of ED attendees by IMD quintile and site type") +
  coord_flip() +
  guides(fill = guide_legend(title = "IMD quintile")) +
  theme_ukhsa()
imd_attendees

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/imd_attendees.svg",
  plot = imd_attendees,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

# 2. Population pyramids for age and sex of ED attendees
## all sites
percent <- function(x, digits = 0, format = "f", ...) { # Create user-defined function
  paste0(formatC(x * 1, format = format, digits = digits, ...), "%")
}
attendees_age_sex_pyramid <- ggplot(attendees_age_sex, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(attendees_age_sex, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(attendees_age_sex, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("Proportion of ED attendees by age and sex (All sites)") +
  labs(x = "Age group", y = "Proportion of attendees", fill = "Sex") +
  coord_flip() +
  theme2()

attendees_age_sex_pyramid

attendees_age_sex_pyramid_notitle <- attendees_age_sex_pyramid +
  ggtitle("All sites")

attendees_age_sex_pyramid

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/attendees_age_sex.svg",
  plot = attendees_age_sex_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

## 5 sites
attendees_age_sex5_pyramid <- ggplot(attendees_age_sex5, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(attendees_age_sex5, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(attendees_age_sex5, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("Proportion of ED attendees by age and sex (5 sites)") +
  labs(x = "Age group", y = "Proportion of attendees", fill = "Sex") +
  coord_flip() +
  theme_ukhsa()

attendees_age_sex5_pyramid

attendees_age_sex5_pyramid_notitle <- attendees_age_sex5_pyramid +
  ggtitle("5 sites") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

attendees_age_sex5_pyramid_notitle

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/attendees_age_sex_5_sites.svg",
  plot = attendees_age_sex5_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

## join together
attendees_age_sex_joined <- attendees_age_sex_pyramid_notitle + attendees_age_sex5_pyramid_notitle
attendees_age_sex_joined

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/attendees_age_sex_joined.svg",
  plot = attendees_age_sex_joined,
  width = 1350,
  height = 900,
  units = "px",
  dpi = 72
)

# 3. Demographic breakdowns of ED attendees with a blood test by site type
eth_bloodtests <-
  ggplot(
    filter(bloodtests_joined, breakdown == "Ethnic group"),
    aes(x = site_type, y = proportion, group = forcats::fct_rev(reorder(var, var)))
  ) +
  scale_fill_manual(values = pal9) +
  geom_col(aes(fill = var), position = "dodge2") +
  labs(x = "Site type", y = "Blood test uptake (%)", title = "Blood test uptake among ED attendees by ethnic group and site type") +
  coord_flip() +
  guides(fill = guide_legend(title = "Ethnic group")) +
  theme_ukhsa()
eth_bloodtests

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/1c_eth_bloodtests.svg",
  plot = eth_bloodtests,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

imd_bloodtests <-
  ggplot(
    filter(bloodtests_joined, breakdown == "IMD"),
    aes(x = site_type, y = proportion, group = forcats::fct_rev(reorder(var, var)))
  ) +
  scale_fill_manual(values = pal5) +
  geom_col(aes(fill = var), position = "dodge2") +
  labs(x = "Site type", y = "Blood test uptake (%)", title = "Blood test uptake among ED attendees by IMD quintile and site type") +
  coord_flip() +
  guides(fill = guide_legend(title = "IMD quintile")) +
  theme_ukhsa()
imd_bloodtests

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/1d_imd_bloodtests.svg",
  plot = imd_bloodtests,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

# 4. Population pyramids for age and sex of ED attendees with a blood test
## all sites
bloodtests_age_sex_pyramid <- ggplot(bloodtests_age_sex, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(bloodtests_age_sex, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(bloodtests_age_sex, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("Blood test uptake among ED attendees by age and sex (All sites)") +
  labs(x = "Age group", y = "Blood test uptake", fill = "Sex") +
  coord_flip() +
  theme2()

bloodtests_age_sex_pyramid

bloodtests_age_sex_pyramid_notitle <- bloodtests_age_sex_pyramid +
  ggtitle("All sites")

bloodtests_age_sex_pyramid

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/1a_bloodtests_age_sex.svg",
  plot = bloodtests_age_sex_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

## 5 sites
bloodtests_age_sex5_pyramid <- ggplot(bloodtests_age_sex_5, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(bloodtests_age_sex_5, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(bloodtests_age_sex_5, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("Blood test uptake among ED attendees by age and sex (5 sites)") +
  labs(x = "Age group", y = "Blood test uptake", fill = "Sex") +
  coord_flip() +
  theme_ukhsa()

bloodtests_age_sex5_pyramid

bloodtests_age_sex5_pyramid_notitle <- bloodtests_age_sex5_pyramid +
  ggtitle("5 sites") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

bloodtests_age_sex5_pyramid_notitle

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/1a_bloodtests_age_sex_5_sites.svg",
  plot = bloodtests_age_sex5_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

## join together
bloodtests_age_sex_joined <- bloodtests_age_sex_pyramid_notitle + bloodtests_age_sex5_pyramid_notitle
bloodtests_age_sex_joined

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/1a_bloodtests_age_sex_joined.svg",
  plot = bloodtests_age_sex_joined,
  width = 1350,
  height = 900,
  units = "px",
  dpi = 72
)

# 5. BBV test uptake for ED attendees overall
## age sex pyramids
#hivtests_age_sex5_pyramid <- ggplot(hivtests_age_sex_5, aes(x = age_group, y = proportion, fill = Sex)) +
#  geom_bar(data = subset(hivtests_age_sex_5, Sex == "Female"), stat = "identity") +
#  geom_bar(data = subset(hivtests_age_sex_5, Sex == "Male"), stat = "identity") +
#  scale_fill_manual(values = pal_hc) +
#  scale_x_discrete(limits = rev) +
#  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
#  ggtitle("HIV test uptake among ED attendees by age and sex") +
#  labs(x = "Age group", y = "HIV test uptake", fill = "Sex", subtitle = "All attendees") +
#  coord_flip() +
#  theme_ukhsa()

#hivtests_age_sex5_pyramid

#ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/hiv_tests_age_sex.svg",
#  plot = hivtests_age_sex5_pyramid,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
#)

#hcvtests_age_sex5_pyramid <- ggplot(hcvtests_age_sex_5, aes(x = age_group, y = proportion, fill = Sex)) +
#  geom_bar(data = subset(hcvtests_age_sex_5, Sex == "Female"), stat = "identity") +
#  geom_bar(data = subset(hcvtests_age_sex_5, Sex == "Male"), stat = "identity") +
#  scale_fill_manual(values = pal_hc) +
#  scale_x_discrete(limits = rev) +
#  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
#  ggtitle("HCV test uptake among ED attendees by age and sex") +
#  labs(x = "Age group", y = "HCV test uptake", fill = "Sex", subtitle = "All attendees") +
#  coord_flip() +
#  theme_ukhsa()

#hcvtests_age_sex5_pyramid

#ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/hcv_tests_age_sex.svg",
#  plot = hcvtests_age_sex5_pyramid,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
#)

#hbvtests_age_sex5_pyramid <- ggplot(hbvtests_age_sex_5, aes(x = age_group, y = proportion, fill = Sex)) +
#  geom_bar(data = subset(hbvtests_age_sex_5, Sex == "Female"), stat = "identity") +
#  geom_bar(data = subset(hbvtests_age_sex_5, Sex == "Male"), stat = "identity") +
#  scale_fill_manual(values = pal_hc) +
#  scale_x_discrete(limits = rev) +
#  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
#  ggtitle("HBV test uptake among ED attendees by age and sex") +
#  labs(x = "Age group", y = "HBV test uptake", fill = "Sex", subtitle = "All attendees") +
#  coord_flip() +
#  theme_ukhsa()

#hbvtests_age_sex5_pyramid

#ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/hbv_tests_age_sex.svg",
#  plot = hbvtests_age_sex5_pyramid,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
#)

## ethnic groups
#eth_bbvtests1 <-
#  ggplot(
#    filter(bbvtests_all_attendees, breakdown == "Ethnic group"),
#    aes(x = forcats::fct_rev(reorder(var, var)), y = test_uptake, group = forcats::fct_rev(reorder(BBV, BBV)))
#  ) +
#  scale_fill_manual(values = pal3) +
#  geom_col(aes(fill = BBV), position = "dodge2") +
#  labs(
#    x = "Ethnic group", y = "BBV test uptake (%)",
#    title = "BBV test uptake among ED attendees by ethnic group and BBV",
#    subtitle = "All ED attendees"
#  ) +
#  coord_flip() +
#  guides(fill = guide_legend(title = "BBV")) +
#  theme_ukhsa()
#eth_bbvtests1

#ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/eth_bbvtests_all_attendees.svg",
#  plot = eth_bbvtests1,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
#)

## IMD quintiles
#imd_bbvtests1 <-
#  ggplot(
#    filter(bbvtests_all_attendees, breakdown == "IMD"),
#    aes(x = forcats::fct_rev(reorder(var, var)), y = test_uptake, group = forcats::fct_rev(reorder(BBV, BBV)))
#  ) +
#  scale_fill_manual(values = pal3) +
#  geom_col(aes(fill = BBV), position = "dodge2") +
#  labs(
#    x = "IMD quintile", y = "BBV test uptake (%)",
#    title = "BBV test uptake among ED attendees by IMD quintile and BBV",
#    subtitle = "All ED attendees"
#  ) +
# coord_flip() +
#  guides(fill = guide_legend(title = "BBV")) +
#  theme_ukhsa()
#imd_bbvtests1

#ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/imd_bbvtests_all_attendees.svg",
#  plot = imd_bbvtests1,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
#)

# 6. BBV test uptake for ED attendees with a blood test
## by site
test_uptake_by_site_graph <-
  ggplot(
    test_uptake_by_site,
    aes(x = SITE, y = test_uptake, group = BBV)
  ) +
  scale_fill_manual(values = pal3) +
  geom_col(aes(fill = BBV), position = "dodge2") +
  labs(
    x = "Site", y = "BBV test uptake (%)",
    title = "BBV test uptake among ED attendees by BBV and site"
  ) +
  guides(fill = guide_legend(title = "BBV")) +
  theme_ukhsa()
test_uptake_by_site_graph

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/3_test_uptake_by_site_graph.svg",
  plot = test_uptake_by_site_graph,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)


## age sex pyramids
hivtests_age_sex5_bt_pyramid <- ggplot(hivtests_age_sex_5_bt, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(hivtests_age_sex_5_bt, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(hivtests_age_sex_5_bt, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("HIV test uptake among ED attendees by age and sex") +
  labs(x = "Age group", y = "HIV test uptake", fill = "Sex", subtitle = "ED attendees with a blood test") +
  coord_flip() +
  theme_ukhsa()

hivtests_age_sex5_bt_pyramid
ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/3a_hiv_tests_age_sex_bt.svg",
  plot = hivtests_age_sex5_bt_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

hcvtests_age_sex5_bt_pyramid <- ggplot(hcvtests_age_sex_5_bt, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(hcvtests_age_sex_5_bt, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(hcvtests_age_sex_5_bt, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("HCV test uptake among ED attendees by age and sex") +
  labs(x = "Age group", y = "HCV test uptake", fill = "Sex", subtitle = "ED attendees with a blood test") +
  coord_flip() +
  theme_ukhsa()

hcvtests_age_sex5_bt_pyramid

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/3a_hcv_tests_age_sex_bt.svg",
  plot = hcvtests_age_sex5_bt_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

hbvtests_age_sex5_bt_pyramid <- ggplot(hbvtests_age_sex_5_bt, aes(x = age_group, y = proportion, fill = Sex)) +
  geom_bar(data = subset(hbvtests_age_sex_5, Sex == "Female"), stat = "identity") +
  geom_bar(data = subset(hbvtests_age_sex_5, Sex == "Male"), stat = "identity") +
  scale_fill_manual(values = pal_hc) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +
  ggtitle("HBV test uptake among ED attendees by age and sex") +
  labs(x = "Age group", y = "HBV test uptake", fill = "Sex", subtitle = "ED attendees with a blood test") +
  coord_flip() +
  theme_ukhsa()

hbvtests_age_sex5_bt_pyramid

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/3a_hbv_tests_age_sex_bt.svg",
  plot = hbvtests_age_sex5_bt_pyramid,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

eth_bbvtests2 <-
  ggplot(
    filter(bbvtests_bt, breakdown == "Ethnic group"),
    aes(x = forcats::fct_rev(reorder(var, var)), y = test_uptake, group = forcats::fct_rev(reorder(BBV, BBV)))
  ) +
  scale_fill_manual(values = pal3) +
  geom_col(aes(fill = BBV), position = "dodge2") +
  labs(
    x = "Ethnic group", y = "BBV test uptake (%)",
    title = "BBV test uptake among ED attendees by ethnic group and BBV",
    subtitle = "ED attendees that had a blood test"
  ) +
  coord_flip() +
  guides(fill = guide_legend(title = "BBV")) +
  theme_ukhsa()
eth_bbvtests2

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/3c_eth_bbvtests_attendees_with_blood_test.svg",
  plot = eth_bbvtests2,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

imd_bbvtests2 <-
  ggplot(
    filter(bbvtests_bt, breakdown == "IMD"),
    aes(x = forcats::fct_rev(reorder(var, var)), y = test_uptake, group = forcats::fct_rev(reorder(BBV, BBV)))
  ) +
  scale_fill_manual(values = pal3) +
  geom_col(aes(fill = BBV), position = "dodge2") +
  labs(
    x = "IMD quintile", y = "BBV test uptake (%)",
    title = "BBV test uptake among ED attendees by IMD quintile and BBV",
    subtitle = "ED attendees that had a blood test"
  ) +
  coord_flip() +
  guides(fill = guide_legend(title = "BBV")) +
  theme_ukhsa()
imd_bbvtests2

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/3d_imd_bbvtests_attendees_with_blood_test.svg",
  plot = imd_bbvtests2,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

# 7. BBV positivity
bbv_positivity_graph <-
  ggplot(
    filter(bbv_positivity, breakdown == "Total"),
    aes(x = BBV, y = positivity, group = diagnosis_type)
  ) +
  scale_fill_manual(values = pal_hc) +
  geom_col(aes(fill = diagnosis_type), position = "dodge2") +
  labs(
    x = "BBV", y = "Test positivity (%)",
    title = "BBV test positivity among ED attendees by BBV and diagnosis type"
  ) +
  guides(fill = guide_legend(title = "Diagnosis type")) +
  theme_ukhsa()
bbv_positivity_graph

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/bbv_positivity.svg",
  plot = bbv_positivity_graph,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)


# HIV positivity
hivdiags <-
  ggplot(
    filter(bbv_positivity, breakdown == "Total" & BBV == "HIV"),
    aes(x = diagnosis_type, y = positivity)
  ) +
  scale_fill_manual(values = pal_hc) +
  geom_col(aes(fill = diagnosis_type), position = "dodge2") +
  labs(
    x = "Diagnosis type", y = "HIV positivity (%)",
    title = "HIV positivity among ED attendees by diagnosis type"
  ) +
  guides(fill = guide_legend(title = "Diagnosis type")) +
  theme_ukhsa()
hivdiags

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/4_hiv_diags.svg",
  plot = hivdiags,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

hcvdiags <-
  ggplot(
    filter(bbv_positivity, breakdown == "Total" & BBV == "HCV"),
    aes(x = diagnosis_type, y = positivity)
  ) +
  scale_fill_manual(values = pal_hc) +
  geom_col(aes(fill = diagnosis_type), position = "dodge2") +
  labs(
    x = "Diagnosis type", y = "HCV positivity (%)",
    title = "HCV positivity among ED attendees by diagnosis type"
  ) +
  guides(fill = guide_legend(title = "Diagnosis type")) +
  theme_ukhsa()
hcvdiags

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/4_hcv_diags.svg",
  plot = hcvdiags,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)

hbvdiags <-
  ggplot(
    filter(bbv_positivity, breakdown == "Total" & BBV == "HBV"),
    aes(x = diagnosis_type, y = positivity)
  ) +
  scale_fill_manual(values = pal_hc) +
  geom_col(aes(fill = diagnosis_type), position = "dodge2") +
  labs(
    x = "Diagnosis type", y = "HBV positivity (%)",
    title = "HBV positivity among ED attendees by diagnosis type"
  ) +
  guides(fill = guide_legend(title = "Diagnosis type")) +
  theme_ukhsa()
hbvdiags

ggsave(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/4_hbv_diags.svg",
  plot = hbvdiags,
  width = 960,
  height = 640,
  units = "px",
  dpi = 72
)


# 10. HIV linkage proportions
# hiv_link <-
#  ggplot(hiv_linkage, aes(x = (
#    reorder(group, outcome_order)
#  ), y = count)) +
#  scale_fill_brewer(palette = "Paired") +
#  geom_col(aes(fill = group), position = "dodge2") +
#  labs(x = "Outcome", y = "Number of patients", title = "Number of HIV positive patients by linkage outcomes") +
#  theme2() +
#  theme(axis.text.x = element_text(angle = 90))
# hiv_link

# ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/hiv_link.svg",
#  plot = hiv_link,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
# )

## HCV linkage proportions
# hcv_link <-
#  ggplot(hcv_linkage, aes(x = (
#    reorder(group, outcome_order)
#  ), y = count)) +
#  scale_fill_brewer(palette = "Paired") +
#  geom_col(aes(fill = group), position = "dodge2") +
#  labs(x = "Outcome", y = "Number of patients", title = "Number of HCV positive patients by linkage outcomes") +
#  theme2() +
#  theme(axis.text.x = element_text(angle = 90))
# hcv_link

# ggsave(
#  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/graphs_wip/hcv_link.svg",
#  plot = hcv_link,
#  width = 960,
#  height = 640,
#  units = "px",
#  dpi = 72
# )