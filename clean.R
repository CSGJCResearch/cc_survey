#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 7/13/2021
#######################################

# load necessary packages
requiredPackages = c('dplyr',
                     'reshape',
                     'readxl',
                     'tidyverse',
                     'knitr',
                     'data.table',
                     'formattable',
                     'scales',
                     'janitor',
                     'Hmisc')

# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# read excel population data for 2018-2020
population18 <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Population 2020", .name_repair = "universal")

# read excel admissions data for 2018-2020
admissions18 <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
admissions19 <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2019", .name_repair = "universal")
admissions20 <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Admissions 2020", .name_repair = "universal")

# read cost data for 2019-2020
costs <- read_xlsx("data/Data for web team 2021 v13.xlsx", sheet = "Costs", .name_repair = "universal")

##############
# Population
##############

# add year variable
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"

# combine pop data
population <- rbind(population18, population19, population20)

population <- population %>% mutate_at(vars(-c("State.Abbrev","States", "year")), as.numeric)
population$year <- factor(population$year)

# add labels
var.labels = c(States="State name", 
               year="Year",
               Total.population = "Total population",
               Total.violation.population = "Total probation and parole violation population",
               Total.probation.violation_population = "Total probation violation population (new offense + technical)",
               New.offense.probation.violation.population = "New offense probation violation population",
               Technical.probation.violation.population = "Technical probation violation population",
               Total.parole.violation.population = "Total parole violation population (new offense + technical)",
               New.offense.parole.violation.population = "New offense parole violation population",
               Technical.parole.violation.population = "Technical parole violation population")

label(population) = as.list(var.labels[match(names(population), names(var.labels))])

##############
# Admissions
##############

# add year variable
admissions18$year <- "2018"
admissions19$year <- "2019"
admissions20$year <- "2020"

# combine data and remove unwanted data (NAs, etc)
admissions <- rbind(admissions18, admissions19, admissions20)

# change data type to numeric
admissions <- admissions %>% mutate_at(vars(-c("State.Abbrev","States", "year")), as.numeric)
admissions$year <- factor(admissions$year)

# add labels
var.labels = c(States="State name", 
               year="Year",
               Total.admissions = "Total admissions",
               Total.violation.admissions = "Total probation and parole violation admissions",
               Total.probation.violation.admissions = "Total probation violation admissions (new offense + technical)",
               New.offense.probation.violation.admissions = "New offense probation violation admissions",
               Technical.probation.violation.admissions = "Technical probation violation admissions",
               Total.parole.violation.admissions = "Total parole violation admissions (new offense + technical)",
               New_offense.parole.violation.admissions = "New offense parole violation admissions",
               Technical.parole.violation.admissions = "Technical parole violation admissions")
label(admissions) = as.list(var.labels[match(names(admissions), names(var.labels))])