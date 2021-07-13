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
  if(!require(p,character_only = TRUE)) install_packages(p)
  library(p,character_only = TRUE)
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
state_abb <- population %>% select(state_abb = State.Abbrev, States = states)
state_abb <- state_abb %>% distinct()

# remove state abb
population <- population %>% select(-State.Abbrev)

# make variables lowercase and replace periods with underscore
population <- population %>% clean_names()

####
# Check for zeros, nAs and data errors
####

# create variable for total viol pop, total prob pop, and total parole pop
population <- population %>% mutate(total_violation_population_calc = total_probation_violation_population+total_parole_violation_population,
                                    total_probation_violation_population_calc = new_offense_probation_violation_population + technical_probation_violation_population,
                                    total_parole_violation_population_calc = new_offense_parole_violation_population + technical_parole_violation_population)

# create variables that tests if totals are incorrect
population <- population %>% mutate(total_violation_correct = ifelse(total_violation_population_calc==total_violation_population, TRUE, FALSE),
                                    total_probation_correct = ifelse(total_probation_violation_population_calc==total_probation_violation_population, TRUE, FALSE),
                                    total_parole_correct = ifelse(total_parole_violation_population_calc==total_parole_violation_population, TRUE, FALSE))

# reorder variables
population <- population %>% select(states, total_violation_correct, total_probation_correct, total_parole_correct, everything())

# replace NAs with zero
# total viol pop = total parole pop + total prob pop
# if total viol pop = total parole pop, then prob pop is zero
population <- population %>% 
         # probation
  mutate(total_prob_new = case_when(total_violation_population==total_parole_violation_population ~ 0,
                                    total_violation_population!=total_parole_violation_population ~ total_probation_violation_population),
         
         new_offense_prob_new = case_when(total_violation_population==total_parole_violation_population ~ 0,
                                          total_violation_population!=total_parole_violation_population ~ new_offense_probation_violation_population,
                                          total_probation_violation_population==technical_probation_violation_population ~ 0,
                                          total_probation_violation_population!=technical_probation_violation_population ~ new_offense_probation_violation_population),
         
         tech_prob_new = case_when(total_violation_population==total_parole_violation_population ~ 0,
                                   total_violation_population!=total_parole_violation_population ~ technical_probation_violation_population,
                                   total_probation_violation_population==new_offense_parole_violation_population ~ 0,
                                   total_probation_violation_population!=new_offense_parole_violation_population ~ technical_probation_violation_population),
         # parole
         total_parole_new = case_when(total_violation_population==total_probation_violation_population ~ 0,
                                      total_violation_population!=total_probation_violation_population ~ total_parole_violation_population),
         
         new_offense_parole_new = case_when(total_violation_population==total_probation_violation_population ~ 0,
                                            total_violation_population!=total_probation_violation_population ~ new_offense_parole_violation_population,
                                            total_parole_violation_population==technical_parole_violation_population ~ 0,
                                            total_parole_violation_population!=technical_parole_violation_population ~ new_offense_parole_violation_population),
         
         tech_parole_new = case_when(total_violation_population==total_probation_violation_population ~ 0,
                                     total_violation_population!=total_probation_violation_population ~ technical_parole_violation_population,
                                     total_parole_violation_population==new_offense_parole_violation_population ~ 0,
                                     total_parole_violation_population!=new_offense_parole_violation_population ~ technical_parole_violation_population))

# reorder variables
population <- population %>% select(states, year, total_population, total_violation_population,
                                    total_prob_new, new_offense_prob_new, tech_prob_new,
                                    total_parole_new, new_offense_parole_new, tech_parole_new, everything())












##############
# Admissions
##############

# remove unwanted variables
adm20 <- adm20 %>% select(-`Admissions_Year`,-`Reporting_Year`,
                          -`Months_Reported`, -Numbers_were_corrected_or_validated_in_the_2021_survey_,
                          -State_Abbrev)
adm19 <- adm19 %>% select(-State_Abbrev)
adm18 <- adm18 %>% select(-State_Abbrev)

# add year variable
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"

# combine data and remove unwanted data (nA, etc)
adm <- rbind(adm18, adm19, adm20)

# create variable for total viol pop, total prob pop, and total parole pop
adm <- adm %>% mutate(total_violation_admissions_calc = total_probation_violation_admissions+total_parole_violation_admissions,
                      total_probation_violation_admissions_calc = new_offense_probation_violation_admissions + technical_probation_violation_admissions,
                      total_parole_violation_admissions_calc = new_offense_parole_violation_admissions + technical_parole_violation_admissions)

# create variables that tests if totals are incorrect
adm <- adm %>% mutate(total_violation_correct = ifelse(total_violation_admissions_calc==total_violation_admissions, TRUE, FALSE),
                      total_probation_correct = ifelse(total_probation_violation_admissions_calc==total_probation_violation_admissions, TRUE, FALSE),
                      total_parole_correct = ifelse(total_parole_violation_admissions_calc==total_parole_violation_admissions, TRUE, FALSE))

# reorder variables
adm <- adm %>% select(states, total_violation_correct, total_probation_correct, total_parole_correct, everything())






























# replace spaces in variable names with periods
names(adm)<-make_names(names(adm),unique = TRUE)
names(population)<-make_names(names(population),unique = TRUE)

# create new commitments variable
adm <- adm %>%
  mutate(new_commitments = total_admissions - total_violation_admissions)

# create new commitments variable
population <- population %>%
  mutate(new_commitments = total_population - total_violation_population)

# change data to long form
adm_long <- adm %>%
  pivot_longer(cols = -c(states,year), names_to = "category", values_to = "count")

pop_long <- population %>%
  pivot_longer(cols = -c(states,year), names_to = "category", values_to = "count")

# rename category names
adm_long <- adm_long %>%
  mutate(totals = case_when(category == "new_commitments" ~ "new Commitments",
                            category == "total_probation_violation_admissions" ~ "Probation",
                            category == "total_parole_violation_admissions" ~ "Parole")) %>%
  mutate(Probation = case_when(category == "new_offense_probation_violation_admissions" ~ "non-technical",
                               category == "technical_probation_violation_admissions" ~ "technical")) %>%
  mutate(Parole = case_when(category == "new_offense_parole_violation_admissions" ~ "non-technical",
                            category == "technical_parole_violation_admissions" ~ "technical"))

# rename category names
pop_long <- pop_long %>%
  mutate(totals = case_when(category == "new_commitments" ~ "new Commitments",
                            category == "total_probation_violation_population" ~ "Probation",
                            category == "total_parole_violation_population" ~ "Parole")) %>%
  mutate(Probation = case_when(category == "new_offense_probation_violation_population" ~ "non-technical",
                               category == "technical_probation_violation_population" ~ "technical")) %>%
  mutate(Parole = case_when(category == "new_offense_parole_violation_population" ~ "non-technical",
                            category == "technical_parole_violation_population" ~ "technical"))

# create factor variables
adm_long$totals <- as_factor(adm_long$totals)
pop_long$totals <- as_factor(pop_long$totals)

# fix factor levels
adm_long$totals <- factor(adm_long$totals, levels = c("new Commitments","Probation","Parole"))
pop_long$totals <- factor(pop_long$totals, levels = c("new Commitments","Probation","Parole"))

# set up table for adm change
adm_change1 <- adm %>% select(states, year, everything()) %>% arrange(desc(states))

# set up table for pop change
pop_change1 <- population %>% select(states, year, everything()) %>% arrange(desc(states))

# calculate tech violations
adm_change <- adm_change1 %>% 
  mutate(technical_violations = technical_probation_violation_admissions + technical_parole_violation_admissions)

# calculate percent change         
adm_change <- adm_change %>% group_by(states) %>% mutate(total_admissions_pct = (total_admissions / dplyr::lag(total_admissions) -1)*100)
adm_change <- adm_change %>% group_by(states) %>% mutate(total_violation_admissions_pct = (total_violation_admissions / dplyr::lag(total_violation_admissions) -1)*100)
adm_change <- adm_change %>% group_by(states) %>% mutate(total_probation_violation_admissions_pct = (total_probation_violation_admissions / dplyr::lag(total_probation_violation_admissions) -1)*100)
adm_change <- adm_change %>% group_by(states) %>% mutate(total_parole_violation_admissions_pct = (total_parole_violation_admissions / dplyr::lag(total_parole_violation_admissions) -1)*100)
adm_change <- adm_change %>% group_by(states) %>% mutate(technical_violations_pct = (technical_violations / dplyr::lag(technical_violations) -1)*100)
adm_change <- adm_change %>% group_by(states) %>% mutate(new_commitments = (new_commitments / dplyr::lag(new_commitments) -1)*100)

# calculate tech violations
pop_change <- pop_change1 %>% 
  mutate(technical_violations = technical_probation_violation_population + technical_parole_violation_population)

# calculate percent change         
pop_change <- pop_change %>% group_by(states) %>% mutate(total_population_pct = (total_population / dplyr::lag(total_population) -1)*100)
pop_change <- pop_change %>% group_by(states) %>% mutate(total_violation_population_pct = (total_violation_population / dplyr::lag(total_violation_population) -1)*100)
pop_change <- pop_change %>% group_by(states) %>% mutate(total_probation_violation_population_pct = (total_probation_violation_population / dplyr::lag(total_probation_violation_population) -1)*100)
pop_change <- pop_change %>% group_by(states) %>% mutate(total_parole_violation_population_pct = (total_parole_violation_population / dplyr::lag(total_parole_violation_population) -1)*100)
pop_change <- pop_change %>% group_by(states) %>% mutate(technical_violations_pct = (technical_violations / dplyr::lag(technical_violations) -1)*100)
pop_change <- pop_change %>% group_by(states) %>% mutate(new_commitments = (new_commitments / dplyr::lag(new_commitments) -1)*100)

# create factor variables
adm_long$year <- factor(adm_long$year)
pop_long$year <- factor(pop_long$year)