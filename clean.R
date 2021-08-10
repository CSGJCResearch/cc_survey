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
                     'Hmisc',
                     'xlsx',
                     )
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

# clean variable names (make lowercase and replace periods with underscores)
population <- clean_names(population)

# remove state abb
population <- population %>% select(-state_abbrev)

population <- population %>% mutate_at(vars(-c("states", "year")), as.numeric)
population$year <- factor(population$year)

# add labels
var.labels = c(states="State name", 
               year="Year",
               total_population = "Total population",
               total_violation_population = "Total probation and parole violation population",
               total_probation_violation_population = "Total probation violation population (new offense + technical)",
               new_offense_probation_violation_population = "New offense probation violation population",
               technical_probation_violation_population = "Technical probation violation population",
               total_parole_violation_population = "Total parole violation population (new offense + technical)",
               new_offense_parole_violation_population = "New offense parole violation population",
               technical_parole_violation_population = "Technical parole violation population")
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

# clean variable names (make lowercase and replace periods with underscores)
admissions <- clean_names(admissions)

# remove state abb
admissions <- admissions %>% select(-state_abbrev)

# change data type to numeric
admissions <- admissions %>% mutate_at(vars(-c("states", "year")), as.numeric)
admissions$year <- factor(admissions$year)

# add labels
var.labels = c(states="State name", 
               year="Year",
               total_admissions = "Total admissions",
               total_violation_admissions = "Total probation and parole violation admissions",
               total_probation_violation_admissions = "Total probation violation admissions (new offense + technical)",
               new_offense_probation_violation_admissions = "New offense probation violation admissions",
               technical_probation_violation_admissions = "Technical probation violation admissions",
               total_parole_violation_admissions = "Total parole violation admissions (new offense + technical)",
               new_offense_parole_violation_admissions = "New offense parole violation admissions",
               technical_parole_violation_admissions = "Technical parole violation admissions")
label(admissions) = as.list(var.labels[match(names(admissions), names(var.labels))])

