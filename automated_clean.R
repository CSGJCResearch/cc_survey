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
                     'knitr'
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
population17 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2017", .name_repair = "universal")
population18 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2018-Corrected", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2019-Corrected", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2020-Corrected", .name_repair = "universal")

# remove pop year
population17 <- population17 %>% select(-Population.Year)
population20 <- population20 %>% select(-`...11`)

# remove rows
population17 <- population17[-c(51:52),] 
population18 <- population18[-c(51:54),] # remove empty rows
population19 <- population19[-c(51:54),] # remove empty rows
population20 <- population20[-c(51:55),] # remove empty rows

# add year variable
population17$year <- "2017"
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"

# combine pop data
population <- rbind(population17, population18, population19, population20)
rm(population17, population18, population19, population20) # remove old dfs
population <- filter(population, State.Abbrev != "NA")
population <- population %>% select(-State.Abbrev)

# read excel admissions data for 2017-2019
adm17 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2017-R", .name_repair = "universal")
adm18 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2018-Corrected", .name_repair = "universal")
adm19 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2019-Corrected", .name_repair = "universal")
adm20 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2020-Corrected", .name_repair = "universal")

# remove unwanted variables
adm17 <- adm17 %>%
  select(-c(Change.in.Revised.Data, No.Revised.Data.Provided, ...11, State.Abbrev))
adm18 <- adm18 %>%
  select(-c(Publicly.Available.Data, ...11, State.Abbrev))
adm19 <- adm19 %>% select(-c(Publicly.Available.Data, ...11, State.Abbrev))
# adm20_backup <- adm20 %>% select(-Notes, -`...15`, -`...16`, -`...17`,-State.Abbrev,-Admissions.Year,-Reporting.Year)
# adm20_backup <- adm20_backup %>% filter(Months.Reported == 12) %>% select(-Months.Reported)
adm20 <- adm20 %>% select(-Notes, -`...15`, -`...16`, -`...17`,-State.Abbrev,-Admissions.Year,-Reporting.Year,-Months.Reported)

# remove unwanted rows
adm17 <- adm17[-c(51:54),] 
adm18 <- adm18[-c(51:54),] 
adm19 <- adm19[-c(51:54),] 
adm20 <- adm20[-c(51:54),] 

# add year variable
adm17$year <- "2017"
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"

# combine data and remove unwanted data (NA, etc)
adm <- rbind(adm17, adm18, adm19, adm20)
rm(adm17, adm18, adm19, adm20) # remove old dfs
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
adm_change <- subset(adm, select = c(States, year, Total.admissions, Total.violation.admissions, New.offense.probation.violation.admissions,
                                            Technical.probation.violation.admissions, New.offense.parole.violation.admissions,
                                            Technical.parole.violation.admissions)) %>% arrange(desc(States))

# calculate pop changes
pop_change <- subset(population, select = c(States, year, Total.population, Total.violation.population, New.offense.probation.violation.population,
                                            Technical.probation.violation.population, New.offense.parole.violation.population,
                                            Technical.parole.violation.population)) %>% arrange(desc(States))

# create overarching categories
adm_change <- adm_change %>% 
  mutate(Supervision.violations = New.offense.probation.violation.admissions + New.offense.parole.violation.admissions) %>%
  mutate(Technical.violations = Technical.probation.violation.admissions + Technical.parole.violation.admissions) %>%
  select(-c(New.offense.probation.violation.admissions, New.offense.parole.violation.admissions, 
            Technical.probation.violation.admissions, Technical.parole.violation.admissions)) %>%
  mutate(Overall.admissions = (Total.admissions / lag(Total.admissions) -1)*100) %>%
  mutate(Violation.admissions = (Total.violation.admissions / lag(Total.violation.admissions) -1)*100) %>% 
  mutate(Admissions.supervision.violators = (Supervision.violations / lag(Supervision.violations) -1)*100) %>%
  mutate(Admissions.technical.violators = (Technical.violations / lag(Technical.violations) -1)*100) %>%
  filter(year != "2017")

# create overarching categories
pop_change <- pop_change %>% 
  mutate(Supervision.violations = New.offense.probation.violation.population + New.offense.parole.violation.population) %>%
  mutate(Technical.violations = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-c(New.offense.probation.violation.population, New.offense.parole.violation.population, 
            Technical.probation.violation.population, Technical.parole.violation.population)) %>%
  mutate(Overall.population = (Total.population / lag(Total.population) -1)*100) %>%
  mutate(Violation.population = (Total.violation.population / lag(Total.violation.population) -1)*100) %>%
  mutate(Population.supervision.violators = (Supervision.violations / lag(Supervision.violations) -1)*100) %>%
  mutate(Population.technical.violators = (Technical.violations / lag(Technical.violations) -1)*100) %>%
  filter(year != "2017")

# create factor variables
adm_long$year <- factor(adm_long$year)
pop_long$year <- factor(pop_long$year)

# remove 2017
adm_long <- adm_long %>% filter(year != 2017)
pop_long <- pop_long %>% filter(year != 2017)

# subset to a few states for now ################################################
adm_long <- adm_long %>% filter(States=="Alabama"|States=="Alaska"|States=="Arizona"|
                                  States=="Colorado"|States=="Connecticut"|States== "Delaware"|
                                  States=="Illinois"|States=="Indiana"|States== "Louisiana"|  
                                  States=="Maine"|States=="Maryland"|States== "Missouri"|    
                                  States=="Nebraska"|States=="North Carolina"|States== "North Dakota"|   
                                  States=="Oklahoma"|States=="Rhode Island"|States== "Texas"|
                                  States=="Vermont"|States=="West Virginia"|States== "Wyoming"
)

pop_long <- pop_long %>% filter(States=="Alabama"|States=="Alaska"|States=="Arizona"|
                                  States=="Colorado"|States=="Connecticut"|States== "Delaware"|
                                  States=="Illinois"|States=="Indiana"|States== "Louisiana"|  
                                  States=="Maine"|States=="Maryland"|States== "Missouri"|    
                                  States=="Nebraska"|States=="North Carolina"|States== "North Dakota"|   
                                  States=="Oklahoma"|States=="Rhode Island"|States== "Texas"|
                                  States=="Vermont"|States=="West Virginia"|States== "Wyoming"
)


##################
# Costs
##################

# costs <- read_excel("data/Cost Per Day For Calculation.xlsx")
# costs <- costs %>% select(`State Abbrev`, States, cost = `State Reported CostPerDay`)
# 
# # costs_adm <- adm_long %>% 
# #   filter(category == "Total.violation.admissions"|
# #          category == "Technical.probation.violation.admissions"|
# #          category == "Technical.parole.violation.admissions") %>% select(States, year, category, count)
# 
# # creat costs_adm df
# costs_adm <- adm %>% select(States, year, Total.violation.admissions, Technical.probation.violation.admissions, Technical.parole.violation.admissions)
# 
# # add technical prob and parole together to get tech number
# costs_adm <- costs_adm %>% mutate(total_admissions = Total.violation.admissions,
#                                   technical_admissions = Technical.probation.violation.admissions + Technical.parole.violation.admissions) %>% 
#                            select(-Technical.probation.violation.admissions,
#                                   -Technical.parole.violation.admissions,
#                                   -Total.violation.admissions)
# 
# # merge costs and admissions numbers
# costs_adm_df <- merge(costs_adm, costs, by = "States")
# 
# # calc costs
# costs_adm_df <- costs_adm_df %>% mutate(adm_sup_cost = total_admissions*cost*365,
#                                         adm_tech_cost = technical_admissions*cost*365) %>% 
#                                  select(States,Year=year,adm_sup_cost,adm_tech_cost)
# 
# # add column for "DOC Costs to incarcerate violators"
# costs_adm_df <- costs_adm_df %>% mutate(text = "Admissions")



