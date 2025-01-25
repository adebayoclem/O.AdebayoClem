# set wd
setwd("Y:/NEW NCSP/RESEARCH AND DEVELOPMENT/J Edney/ED opt out/ed-monitoring/")

# load packages
pacman::p_load(
  pacman,
  logr
)

# start a log
log_open("temp")

# print all variable options
colnames_list <- (colnames(ECDS))

for (i in colnames_list) {
  log_print(i)
  table <- table(ECDS[[i]])
  log_print(dput(table))
}

# save log
log_close()

writeLines(readLines(lf))

# find variable options
table <- as.data.frame(table(SGSS$lsoa))
dput(table)

# check for unique values
unique(ECDS$accommodation_status_description)
unique(SGSS$accomodationstatus)