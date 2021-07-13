#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 7/13/2021
#######################################

# load necessary packages
requiredPackages = c('dplyr',
                     'openxlsx',
                     'readr',
                     'reshape',
                     'ggplot2',
                     'readxl',
                     'tidyverse',
                     'knitr',
                     'data.table',
                     'formattable',
                     'scales',
                     'janitor',
                     'Hmisc'
                     )
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# get working directory depending on login
getwd <- function(){
  thisLogin <- Sys_info()['login']
  # if(thisLogin=="amund") {
  #   base <- '/home'
  #   csgF <- 'directory'
  # }
  if(thisLogin=="mr4909"){
    base <- '/Users'
    csgF <- 'csgjc/cc_survey'
  }
  if(thisLogin=="mari") {
    base <- '/Users'
    csgF <- 'csgjc/cc_survey'
  }
  wd <- paste(base,thisLogin,csgF,sep="/")
  return(wd)
}

# read excel population data for 2017-2019
# read_xlsx is causing issues and creating duplicate rows
population18 <- read_xlsx("data/Data for web team 2021 v8.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v8.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v8.xlsx", sheet = "Population 2020", .name_repair = "universal")

# read excel admissions data for 2017-2019
adm18 <- read_xlsx("data/Data for web team 2021 v8.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
adm19 <- read_xlsx("data/Data for web team 2021 v8.xlsx", sheet = "Admissions 2019", .name_repair = "universal")
adm20 <- read_xlsx("data/Data for web team 2021 v8.xlsx", sheet = "Admissions 2020", .name_repair = "universal")

# remove unwanted variables
population20 <- population20 %>% select(-Numbers.were.corrected.or.validated.in.the.2021.survey.)

# add year variable
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"

# combine pop data
population <- rbind(population18, population19, population20)
# rm(population17, population18, population19, population20) # remove old dfs

# save state abb
state_abb <- population %>% select(state_abb = State.Abbrev, states = States)
state_abb <- state_abb %>% distinct()

# remove state abb
population <- population %>% select(-State.Abbrev)

# make variables lowercase and replace periods with underscore
population <- population %>% janitor::clean_names()

####
# Check for zeros, NAs and data errors
####

# create variable for total viol pop, total prob pop, and total parole pop
population <- population %>% mutate(total_violation_population_calc = total_probation_violation_population+total_parole_violation_population,
                                    total_probation_violation_population_calc = new_offense_probation_violation_population + technical_probation_violation_population,
                                    total_parole_violation_population_calc = new_offense_parole_violation_population + technical_parole_violation_population)

# create variables that tests if totals are incorrect
population <- population %>% mutate(total_violation_pop_correct = ifelse(total_violation_population_calc==total_violation_population, TRUE, FALSE),
                                    total_probation_pop_correct = ifelse(total_probation_violation_population_calc==total_probation_violation_population, TRUE, FALSE),
                                    total_parole_pop_correct = ifelse(total_parole_violation_population_calc==total_parole_violation_population, TRUE, FALSE))

# reorder variables
population <- population %>% select(states, total_violation_pop_correct, total_probation_pop_correct, total_parole_pop_correct, everything())

# replace NAs with zero
# total viol pop = total parole pop + total prob pop
# if total viol pop = total parole pop, then prob pop is zero

# total probation
population <- population %>% mutate(total_probation_pop_new = case_when(
  !is.na(total_violation_population) & total_violation_population==total_parole_violation_population ~ 0,
  !is.na(total_violation_population) ~ total_probation_violation_population,
  is.na(total_violation_population) ~ total_probation_violation_population
))

# new offense probation 
population <- population %>% mutate(new_offense_probation_pop_new = case_when(
  !is.na(total_violation_population) & total_violation_population==total_parole_violation_population ~ 0,
  !is.na(total_probation_violation_population) & total_probation_violation_population==technical_probation_violation_population ~ 0,
  !is.na(new_offense_probation_violation_population) ~ new_offense_probation_violation_population,
  is.na(new_offense_probation_violation_population) ~ new_offense_probation_violation_population
))

# technical probation
population <- population %>% mutate(technical_probation_pop_new = case_when(
  !is.na(total_violation_population) & total_violation_population==total_parole_violation_population ~ 0,
  !is.na(total_probation_violation_population) & total_probation_violation_population==new_offense_probation_violation_population ~ 0,
  !is.na(technical_probation_violation_population) ~ technical_probation_violation_population,
  is.na(technical_probation_violation_population) ~ technical_probation_violation_population
))

# total parole
population <- population %>% mutate(total_parole_pop_new = case_when(
  !is.na(total_violation_population) & total_violation_population==total_probation_violation_population ~ 0,
  !is.na(total_violation_population) ~ total_parole_violation_population,
  is.na(total_violation_population) ~ total_parole_violation_population
))

# new offense parole 
population <- population %>% mutate(new_offense_parole_pop_new = case_when(
  !is.na(total_violation_population) & total_violation_population==total_probation_violation_population ~ 0,
  !is.na(total_parole_violation_population) & total_parole_violation_population==technical_parole_violation_population ~ 0,
  !is.na(new_offense_parole_violation_population) ~ new_offense_parole_violation_population,
  is.na(new_offense_parole_violation_population) ~ new_offense_parole_violation_population
))

# technical parole
population <- population %>% mutate(technical_parole_pop_new = case_when(
  !is.na(total_violation_population) & total_violation_population==total_probation_violation_population ~ 0,
  !is.na(total_parole_violation_population) & total_parole_violation_population==new_offense_parole_violation_population ~ 0,
  !is.na(technical_parole_violation_population) ~ technical_parole_violation_population,
  is.na(technical_parole_violation_population) ~ technical_parole_violation_population
))

# reorder variables
population <- population %>% select(states, 
                                    year, 
                                    total_population, 
                                    total_violation_population,
                                    total_probation_pop_new,
                                    new_offense_probation_pop_new,
                                    technical_probation_pop_new,
                                    total_parole_pop_new,
                                    new_offense_parole_pop_new,
                                    technical_parole_pop_new)

# rename variables
population <- population %>% select(states, 
                                    year, 
                                    total_population, 
                                    total_violation_population,
                                    total_probation_violation_population = total_probation_pop_new, 
                                    new_offense_probation_violation_population = new_offense_probation_pop_new, 
                                    technical_probation_violation_population = technical_probation_pop_new,
                                    total_parole_violation_population = total_parole_pop_new, 
                                    new_offense_parole_violation_population = new_offense_parole_pop_new, 
                                    technical_parole_violation_population = technical_parole_pop_new)

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

# remove unwanted variables
adm20 <- adm20 %>% select(-`Admissions.Year`,-`Reporting.Year`,
                          -`Months.Reported`, -Numbers.were.corrected.or.validated.in.the.2021.survey.,
                          -State.Abbrev)
adm19 <- adm19 %>% select(-State.Abbrev)
adm18 <- adm18 %>% select(-State.Abbrev)

# add year variable
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"

# combine data and remove unwanted data (NAs, etc)
adm <- rbind(adm18, adm19, adm20)

# make variables lowercase and replace periods with underscore
admissions <- adm %>% janitor::clean_names()

####
# Check for zeros, NAs and data errors
####

# create variable for total viol adm, total prob adm, and total parole adm
admissions <- admissions %>% mutate(total_violation_admissions_calc = total_probation_violation_admissions+total_parole_violation_admissions,
                                    total_probation_violation_admissions_calc = new_offense_probation_violation_admissions + technical_probation_violation_admissions,
                                    total_parole_violation_admissions_calc = new_offense_parole_violation_admissions + technical_parole_violation_admissions)

# create variables that tests if totals are incorrect
admissions <- admissions %>% mutate(total_violation_adm_correct = ifelse(total_violation_admissions_calc==total_violation_admissions, TRUE, FALSE),
                                    total_probation_adm_correct = ifelse(total_probation_violation_admissions_calc==total_probation_violation_admissions, TRUE, FALSE),
                                    total_parole_adm_correct = ifelse(total_parole_violation_admissions_calc==total_parole_violation_admissions, TRUE, FALSE))

# reorder variables
admissions <- admissions %>% select(states, total_violation_adm_correct, total_probation_adm_correct, total_parole_adm_correct, everything())

# replace NAs with zero
# total viol adm = total parole adm + total prob adm
# if total viol adm = total parole adm, then prob adm is zero

# total probation
admissions <- admissions %>% mutate(total_probation_adm_new = case_when(
  !is.na(total_violation_admissions) & total_violation_admissions==total_parole_violation_admissions ~ 0,
  !is.na(total_violation_admissions) ~ total_probation_violation_admissions,
  is.na(total_violation_admissions) ~ total_probation_violation_admissions
))

# new offense probation 
admissions <- admissions %>% mutate(new_offense_probation_adm_new = case_when(
  !is.na(total_violation_admissions) & total_violation_admissions==total_parole_violation_admissions ~ 0,
  !is.na(total_probation_violation_admissions) & total_probation_violation_admissions==technical_probation_violation_admissions ~ 0,
  !is.na(new_offense_probation_violation_admissions) ~ new_offense_probation_violation_admissions,
  is.na(new_offense_probation_violation_admissions) ~ new_offense_probation_violation_admissions
))

# technical probation
admissions <- admissions %>% mutate(technical_probation_adm_new = case_when(
  !is.na(total_violation_admissions) & total_violation_admissions==total_parole_violation_admissions ~ 0,
  !is.na(total_probation_violation_admissions) & total_probation_violation_admissions==new_offense_probation_violation_admissions ~ 0,
  !is.na(technical_probation_violation_admissions) ~ technical_probation_violation_admissions,
  is.na(technical_probation_violation_admissions) ~ technical_probation_violation_admissions
))

# total parole
admissions <- admissions %>% mutate(total_parole_adm_new = case_when(
  !is.na(total_violation_admissions) & total_violation_admissions==total_probation_violation_admissions ~ 0,
  !is.na(total_violation_admissions) ~ total_parole_violation_admissions,
  is.na(total_violation_admissions) ~ total_parole_violation_admissions
))

# new offense parole 
admissions <- admissions %>% mutate(new_offense_parole_adm_new = case_when(
  !is.na(total_violation_admissions) & total_violation_admissions==total_probation_violation_admissions ~ 0,
  !is.na(total_parole_violation_admissions) & total_parole_violation_admissions==technical_parole_violation_admissions ~ 0,
  !is.na(new_offense_parole_violation_admissions) ~ new_offense_parole_violation_admissions,
  is.na(new_offense_parole_violation_admissions) ~ new_offense_parole_violation_admissions
))

# technical parole
admissions <- admissions %>% mutate(technical_parole_adm_new = case_when(
  !is.na(total_violation_admissions) & total_violation_admissions==total_probation_violation_admissions ~ 0,
  !is.na(total_parole_violation_admissions) & total_parole_violation_admissions==new_offense_parole_violation_admissions ~ 0,
  !is.na(technical_parole_violation_admissions) ~ technical_parole_violation_admissions,
  is.na(technical_parole_violation_admissions) ~ technical_parole_violation_admissions
))

# reorder variables
admissions <- admissions %>% select(states, 
                                    year, 
                                    total_admissions, 
                                    total_violation_admissions,
                                    total_probation_adm_new,
                                    new_offense_probation_adm_new,
                                    technical_probation_adm_new,
                                    total_parole_adm_new,
                                    new_offense_parole_adm_new,
                                    technical_parole_adm_new)

# rename variables
admissions <- admissions %>% select(states, 
                                    year, 
                                    total_admissions, 
                                    total_violation_admissions,
                                    total_probation_violation_admissions = total_probation_adm_new, 
                                    new_offense_probation_violation_admissions = new_offense_probation_adm_new, 
                                    technical_probation_violation_admissions = technical_probation_adm_new,
                                    total_parole_violation_admissions = total_parole_adm_new, 
                                    new_offense_parole_violation_admissions = new_offense_parole_adm_new, 
                                    technical_parole_violation_admissions = technical_parole_adm_new)

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
