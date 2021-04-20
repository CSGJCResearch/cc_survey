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
                     'data.table',
                     'formattable',
                     'scales',
                     'extrafont'
                     )
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# get working directory depending on login
getwd <- function(){
  thisLogin <- Sys.info()['login']
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
population18 <- read_xlsx("data/Data for web team 2021 v2.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v2.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v2.xlsx", sheet = "Population 2020", .name_repair = "universal")

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

# read excel admissions data for 2017-2019
adm18 <- read_xlsx("data/Data for web team 2021 v2.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
adm19 <- read_xlsx("data/Data for web team 2021 v2.xlsx", sheet = "Admissions 2019", .name_repair = "universal")
adm20 <- read_xlsx("data/Data for web team 2021 v2.xlsx", sheet = "Admissions 2020", .name_repair = "universal")

# remove unwanted variables
adm20 <- adm20 %>% select(-`Admissions.Year`,-`Reporting.Year`,-`Months.Reported`, -Numbers.were.corrected.or.validated.in.the.2021.survey.)

# add year variable
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"

# combine data and remove unwanted data (NA, etc)
adm <- rbind(adm18, adm19, adm20)
adm <- adm %>% select(-State.Abbrev)
# rm(adm17, adm18, adm19, adm20) # remove old dfs

# replace spaces in variable names with periods
names(adm)<-make.names(names(adm),unique = TRUE)
names(population)<-make.names(names(population),unique = TRUE)

# create new commitments variable
adm <- adm %>%
  mutate(New.commitments = Total.admissions - Total.violation.admissions)

# create new commitments variable
population <- population %>%
  mutate(New.commitments = Total.population - Total.violation.population)

# change data to long form
adm_long <- adm %>%
  pivot_longer(cols = -c(States,year), names_to = "category", values_to = "count")

pop_long <- population %>%
  pivot_longer(cols = -c(States,year), names_to = "category", values_to = "count")

# rename category names
adm_long <- adm_long %>%
  mutate(Totals = case_when(category == "New.commitments" ~ "New Commitments",
                            category == "Total.probation.violation.admissions" ~ "Probation",
                            category == "Total.parole.violation.admissions" ~ "Parole")) %>%
  mutate(Probation = case_when(category == "New.offense.probation.violation.admissions" ~ "Non-Technical",
                               category == "Technical.probation.violation.admissions" ~ "Technical")) %>%
  mutate(Parole = case_when(category == "New.offense.parole.violation.admissions" ~ "Non-Technical",
                            category == "Technical.parole.violation.admissions" ~ "Technical"))

# rename category names
pop_long <- pop_long %>%
  mutate(Totals = case_when(category == "New.commitments" ~ "New Commitments",
                            category == "Total.probation.violation.population" ~ "Probation",
                            category == "Total.parole.violation.population" ~ "Parole")) %>%
  mutate(Probation = case_when(category == "New.offense.probation.violation.population" ~ "Non-Technical",
                               category == "Technical.probation.violation.population" ~ "Technical")) %>%
  mutate(Parole = case_when(category == "New.offense.parole.violation.population" ~ "Non-Technical",
                            category == "Technical.parole.violation.population" ~ "Technical"))

# create factor variables
adm_long$Totals <- as.factor(adm_long$Totals)
pop_long$Totals <- as.factor(pop_long$Totals)

# fix factor levels
adm_long$Totals <- factor(adm_long$Totals, levels = c("New Commitments","Probation","Parole"))
pop_long$Totals <- factor(pop_long$Totals, levels = c("New Commitments","Probation","Parole"))

# set up table for adm change
adm_change1 <- adm %>% select(States, year, everything()) %>% arrange(desc(States))

# set up table for pop change
pop_change1 <- population %>% select(States, year, everything()) %>% arrange(desc(States))

# # calculate changes
# adm_change <- adm_change %>% 
#   mutate(Supervision.violations = New.offense.probation.violation.admissions + New.offense.parole.violation.admissions) %>%
#   mutate(Technical.violations = Technical.probation.violation.admissions + Technical.parole.violation.admissions) %>%
#   select(-c(New.offense.probation.violation.admissions, New.offense.parole.violation.admissions, 
#             Technical.probation.violation.admissions, Technical.parole.violation.admissions)) %>%
#   mutate(Overall.admissions = (Total.admissions / lag(Total.admissions) -1)*100) %>%
#   mutate(Violation.admissions = (Total.violation.admissions / lag(Total.violation.admissions) -1)*100) %>% 
#   mutate(Admissions.supervision.violators = (Supervision.violations / lag(Supervision.violations) -1)*100) %>%
#   mutate(Admissions.technical.violators = (Technical.violations / lag(Technical.violations) -1)*100) %>%
#   filter(year != "2017")
# 
# # calculate changes
# pop_change <- pop_change %>% 
#   mutate(Supervision.violations = New.offense.probation.violation.population + New.offense.parole.violation.population) %>%
#   mutate(Technical.violations = Technical.probation.violation.population + Technical.parole.violation.population) %>%
#   select(-c(New.offense.probation.violation.population, New.offense.parole.violation.population, 
#             Technical.probation.violation.population, Technical.parole.violation.population)) %>%
#   mutate(Overall.population = (Total.population / lag(Total.population) -1)*100) %>%
#   mutate(Violation.population = (Total.violation.population / lag(Total.violation.population) -1)*100) %>%
#   mutate(Population.supervision.violators = (Supervision.violations / lag(Supervision.violations) -1)*100) %>%
#   mutate(Population.technical.violators = (Technical.violations / lag(Technical.violations) -1)*100) %>%
#   filter(year != "2017")

# calculate tech violations
adm_change <- adm_change1 %>% 
  mutate(Technical.violations = Technical.probation.violation.admissions + Technical.parole.violation.admissions)

# calculate percent change         
adm_change <- adm_change %>% group_by(States) %>% mutate(Total.admissions.pct = (Total.admissions / dplyr::lag(Total.admissions) -1)*100)
adm_change <- adm_change %>% group_by(States) %>% mutate(Total.violation.admissions.pct = (Total.violation.admissions / dplyr::lag(Total.violation.admissions) -1)*100)
adm_change <- adm_change %>% group_by(States) %>% mutate(Total.probation.violation.admissions.pct = (Total.probation.violation.admissions / dplyr::lag(Total.probation.violation.admissions) -1)*100)
adm_change <- adm_change %>% group_by(States) %>% mutate(Total.parole.violation.admissions.pct = (Total.parole.violation.admissions / dplyr::lag(Total.parole.violation.admissions) -1)*100)
adm_change <- adm_change %>% group_by(States) %>% mutate(Technical.violations.pct = (Technical.violations / dplyr::lag(Technical.violations) -1)*100)
adm_change <- adm_change %>% group_by(States) %>% mutate(New.commitments = (New.commitments / dplyr::lag(New.commitments) -1)*100)

# calculate tech violations
pop_change <- pop_change1 %>% 
  mutate(Technical.violations = Technical.probation.violation.population + Technical.parole.violation.population)

# calculate percent change         
pop_change <- pop_change %>% group_by(States) %>% mutate(Total.population.pct = (Total.population / dplyr::lag(Total.population) -1)*100)
pop_change <- pop_change %>% group_by(States) %>% mutate(Total.violation.population.pct = (Total.violation.population / dplyr::lag(Total.violation.population) -1)*100)
pop_change <- pop_change %>% group_by(States) %>% mutate(Total.probation.violation.population.pct = (Total.probation.violation.population / dplyr::lag(Total.probation.violation.population) -1)*100)
pop_change <- pop_change %>% group_by(States) %>% mutate(Total.parole.violation.population.pct = (Total.parole.violation.population / dplyr::lag(Total.parole.violation.population) -1)*100)
pop_change <- pop_change %>% group_by(States) %>% mutate(Technical.violations.pct = (Technical.violations / dplyr::lag(Technical.violations) -1)*100)
pop_change <- pop_change %>% group_by(States) %>% mutate(New.commitments = (New.commitments / dplyr::lag(New.commitments) -1)*100)

# create factor variables
adm_long$year <- factor(adm_long$year)
pop_long$year <- factor(pop_long$year)

