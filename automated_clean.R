#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 2/21/2020
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
                     'data_table',
                     'formattable',
                     'scales',
                     'extrafont',
                     'janitor'
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
population18 <- read_xlsx("data/Data for web team 2021 v7.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v7.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v7.xlsx", sheet = "Population 2020", .name_repair = "universal")

# read excel admissions data for 2017-2019
adm18 <- read_xlsx("data/Data for web team 2021 v7.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
adm19 <- read_xlsx("data/Data for web team 2021 v7.xlsx", sheet = "Admissions 2019", .name_repair = "universal")
adm20 <- read_xlsx("data/Data for web team 2021 v7.xlsx", sheet = "Admissions 2020", .name_repair = "universal")

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
population <- population %>%  
  dplyr::mutate(total_probation_pop_new = ifelse(total_violation_population==total_parole_violation_population &
                                                 !is.na(total_violation_population) &
                                                 !is.na(total_parole_violation_population), 0, total_probation_violation_population))

# new offense probation         
population <- population %>% mutate(new_offense_prob_pop_new = 
                                      case_when(total_violation_population==total_parole_violation_population |
                                                total_probation_violation_population==technical_probation_violation_population ~ 0,
                                                total_violation_population!=total_parole_violation_population | 
                                                total_probation_violation_population!=technical_probation_violation_population  ~ new_offense_probation_violation_population))

# technical probation
population <- population %>% mutate(tech_prob_pop_new = case_when(total_violation_population==total_parole_violation_population |
                                                                  total_probation_violation_population==new_offense_parole_violation_population ~ 0,
                                                                  total_violation_population!=total_parole_violation_population |
                                                                  total_probation_violation_population!=new_offense_parole_violation_population ~ technical_probation_violation_population))
# total parole 
population <- population %>%  
  dplyr::mutate(total_parole_pop_new = ifelse(total_violation_population==total_probation_violation_population &
                                              !is.na(total_violation_population) &
                                              !is.na(total_probation_violation_population), 0, total_parole_violation_population))

# new offense parole  
population <- population %>% mutate(new_offense_parole_pop_new = case_when(total_violation_population==total_probation_violation_population | 
                                                                         total_parole_violation_population==technical_parole_violation_population ~ 0,
                                                                       total_violation_population!=total_probation_violation_population |
                                                                         total_parole_violation_population!=technical_parole_violation_population ~ new_offense_parole_violation_population))
# technical parole  
population <- population %>% mutate(tech_parole_pop_new = case_when(total_violation_population==total_probation_violation_population | 
                                                                  total_parole_violation_population==new_offense_parole_violation_population ~ 0,
                                                                total_violation_population!=total_probation_violation_population | 
                                                                  total_parole_violation_population!=new_offense_parole_violation_population ~ technical_parole_violation_population))
# reorder variables
population <- population %>% select(states, 
                                    year, 
                                    total_population, 
                                    total_violation_population,
                                    total_prob_pop_new, 
                                    new_offense_prob_pop_new, 
                                    tech_prob_pop_new,
                                    total_parole_pop_new, 
                                    new_offense_parole_pop_new, 
                                    tech_parole_pop_new)

# rename variables
population <- population %>% select(states, 
                                    year, 
                                    total_population, 
                                    total_violation_population,
                                    total_probation_violation_population = total_prob_pop_new, 
                                    new_offense_probation_violation_population = new_offense_prob_pop_new, 
                                    technical_probation_violation_population = tech_prob_pop_new,
                                    total_parole_violation_population = total_parole_pop_new, 
                                    new_offense_parole_violation_population = new_offense_parole_pop_new, 
                                    technical_parole_violation_population = tech_parole_pop_new)

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

# combine data and remove unwanted data (nA, etc)
adm <- rbind(adm18, adm19, adm20)

# make variables lowercase and replace periods with underscore
admissions <- adm %>% janitor::clean_names()

# make variables lowercase and replace periods with underscore
admissions <- admissions %>% janitor::clean_names()

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
admissions <- admissions %>%  
  dplyr::mutate(total_prob_adm_new = ifelse(total_violation_admissions==total_parole_violation_admissions &
                                                   !is.na(total_violation_admissions) &
                                                   !is.na(total_parole_violation_admissions), 0, total_probation_violation_admissions))

# new offense probation         
admissions <- admissions %>% mutate(new_offense_prob_adm_new = 
                                      case_when(total_violation_admissions==total_parole_violation_admissions |
                                                  total_probation_violation_admissions==technical_probation_violation_admissions ~ 0,
                                                total_violation_admissions!=total_parole_violation_admissions | 
                                                  total_probation_violation_admissions!=technical_probation_violation_admissions  ~ new_offense_probation_violation_admissions))

# technical probation
admissions <- admissions %>% mutate(tech_prob_adm_new = case_when(total_violation_admissions==total_parole_violation_admissions |
                                                                    total_probation_violation_admissions==new_offense_parole_violation_admissions ~ 0,
                                                                  total_violation_admissions!=total_parole_violation_admissions |
                                                                    total_probation_violation_admissions!=new_offense_parole_violation_admissions ~ technical_probation_violation_admissions))
# total parole 
admissions <- admissions %>%  
  dplyr::mutate(total_parole_adm_new = ifelse(total_violation_admissions==total_probation_violation_admissions &
                                                !is.na(total_violation_admissions) &
                                                !is.na(total_probation_violation_admissions), 0, total_parole_violation_admissions))

# new offense parole  
admissions <- admissions %>% mutate(new_offense_parole_adm_new = case_when(total_violation_admissions==total_probation_violation_admissions | 
                                                                             total_parole_violation_admissions==technical_parole_violation_admissions ~ 0,
                                                                           total_violation_admissions!=total_probation_violation_admissions |
                                                                             total_parole_violation_admissions!=technical_parole_violation_admissions ~ new_offense_parole_violation_admissions))
# technical parole  
admissions <- admissions %>% mutate(tech_parole_adm_new = case_when(total_violation_admissions==total_probation_violation_admissions | 
                                                                      total_parole_violation_admissions==new_offense_parole_violation_admissions ~ 0,
                                                                    total_violation_admissions!=total_probation_violation_admissions | 
                                                                      total_parole_violation_admissions!=new_offense_parole_violation_admissions ~ technical_parole_violation_admissions))
# reorder variables
admissions <- admissions %>% select(states, 
                                    year, 
                                    total_admissions, 
                                    total_violation_admissions,
                                    total_prob_adm_new, 
                                    new_offense_prob_adm_new, 
                                    tech_prob_adm_new,
                                    total_parole_adm_new, 
                                    new_offense_parole_adm_new, 
                                    tech_parole_adm_new)

# rename variables
admissions <- admissions %>% select(states, 
                                    year, 
                                    total_admissions, 
                                    total_violation_admissions,
                                    total_probation_violation_admissions = total_prob_adm_new, 
                                    new_offense_probation_violation_admissions = new_offense_prob_adm_new, 
                                    technical_probation_violation_admissions = tech_prob_adm_new,
                                    total_parole_violation_admissions = total_parole_adm_new, 
                                    new_offense_parole_violation_admissions = new_offense_parole_adm_new, 
                                    technical_parole_violation_admissions = tech_parole_adm_new)