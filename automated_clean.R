#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 12/1/2020
#######################################

# load necessary packages
requiredPackages = c('dplyr',
                     'openxlsx',
                     'readr',
                     'reshape',
                     'ggplot2',
                     'readxl')
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
population17 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Population 2017", .name_repair = "universal")
population18 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Population 2019", .name_repair = "universal")

# include fake 2020 data (copy of 2019)
population20 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Population 2019", .name_repair = "universal")

# remove pop year
population17 <- population17 %>% select(-Population.Year)

# add year variable
population17$year <- "2017"
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"

# combine pop data
population <- rbind(population17, population18, population19, population20)
population <- filter(population, State.Abbrev != "NA")
population <- population %>% select(-State.Abbrev)

# read excel admissions data for 2017-2019
adm17 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Admissions 2017-R", .name_repair = "universal")
adm18 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
adm19 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Admissions 2019", .name_repair = "universal")

# include fake 2020 data (copy of 2019)
adm20 <- read_xlsx("data/Data for web team 2020 v6.xlsx", sheet = "Admissions 2019", .name_repair = "universal")

# remove unwanted variables
adm17 <- adm17 %>%
  select(-c(Change.in.Revised.Data, No.Revised.Data.Provided, ...11, State.Abbrev))
adm18 <- adm18 %>%
  select(-c(Publicly.Available.Data, ...11, State.Abbrev))
adm19 <- adm19 %>%
  select(-c(Publicly.Available.Data, ...11, State.Abbrev))
adm20 <- adm20 %>%
  select(-c(Publicly.Available.Data, ...11, State.Abbrev))

# add year variable
adm17$year <- "2017"
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"

# combine data and remove unwanted data (NA, etc)
adm <- rbind(adm17, adm18, adm19, adm20)
adm <- filter(adm, States != "NA")
adm <- filter(adm, States != "Total")
adm <- filter(adm, States != "Count")

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

# calculate admission changes
adm_change <- subset(adm, select = c(States, year, Total.admissions, New.offense.probation.violation.admissions,
                                     Technical.probation.violation.admissions)) %>% arrange(desc(States))

# calculate pop changes
pop_change <- subset(population, select = c(States, year, Total.population, New.offense.probation.violation.population,
                                            Technical.probation.violation.population, New.offense.parole.violation.population,
                                            Technical.parole.violation.population)) %>% arrange(desc(States))

# create overarching categories
pop_change <- pop_change %>% 
  mutate(Supervision.violations = New.offense.probation.violation.population + New.offense.parole.violation.population) %>%
  mutate(Technical.violations = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-c(New.offense.probation.violation.population, New.offense.parole.violation.population, 
            Technical.probation.violation.population, Technical.parole.violation.population)) %>%
  mutate(Overall.population = (Total.population / lag(Total.population) -1)*100) %>%
  mutate(Population.supervision.violators = (Supervision.violations / lag(Supervision.violations) -1)*100) %>%
  mutate(Population.technical.violators = (Technical.violations / lag(Technical.violations) -1)*100) %>%
  filter(year != "2017")





