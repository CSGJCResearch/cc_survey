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
# read_xlsx is causing issues and creating duplicate rows
# population17 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2017", .name_repair = "universal")
# population18 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2018-Corrected", .name_repair = "universal")
# population19 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2019-Corrected", .name_repair = "universal")
# population20 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Population 2020-Corrected", .name_repair = "universal")

population17 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2017-Table 1.csv")
population18 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2018-Corrected-Table 1.csv")
population19 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2019-Corrected-Table 1.csv")
population20 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2020-Corrected-Table 1.csv")

# remove unwanted variables
population17 <- population17 %>% select(-`Population Year`,-X12,-X13)
population18 <- population18 %>% select(-Notes,-Corrected,-X13, -X14)
population19 <- population19 %>% select(-Notes,-Corrected, -X13)
population20 <- population20 %>% select(-Notes,-Corrected, -X13, -X14)

# remove rows
# population17 <- population17[-c(51:52),]
# population18 <- population18[-c(51:54),] # remove empty rows
# population19 <- population19[-c(51:54),] # remove empty rows
# population20 <- population20[-c(51:52),] # remove empty rows
population17 <- population17 %>% filter(`State Abbrev` != "NA")
population18 <- population18 %>% filter(`State Abbrev` != "NA")
population19 <- population19 %>% filter(`State Abbrev` != "NA")
population20 <- population20 %>% filter(`State Abbrev` != "NA")

######
# check this
######
# issue with pop 20, some rows were duplicated with NAs - weird bug?
population20 <- population20 %>% arrange(States)
population20 <- population20[-c(4, 20, 27, 34, 46, 49),] # remove duplicate rows

# add year variable
population17$year <- "2017"
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"

# combine pop data
population <- rbind(population17, population18, population19, population20)
# rm(population17, population18, population19, population20) # remove old dfs
population <- population %>% select(-`State Abbrev`)

# read excel admissions data for 2017-2019
# adm17 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2017-R", .name_repair = "universal")
# adm18 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2018-Corrected", .name_repair = "universal")
# adm19 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2019-Corrected", .name_repair = "universal")
# adm20 <- read_xlsx("data/Data for web team 2020 v6 CORRECTED.xlsx", sheet = "Admissions 2020-Corrected", .name_repair = "universal")

adm17 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2017-Table 1.csv")
adm18 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2018-Corrected-Table 1.csv")
adm19 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2019-Corrected-Table 1.csv")
adm20 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2020-Corrected-Table 1.csv")

# remove unwanted variables
adm17 <- adm17 %>% select(-X11)
adm18 <- adm18 %>% select(-Notes, -Corrected, -`Publicly Available Data`,-X14)
adm19 <- adm19 %>% select(-Notes, -Corrected, -`Publicly Available Data`,-X14)
adm20 <- adm20 %>% select(-`Admissions Year`,-`Reporting Year`,-`Months Reported`, -Notes, -Corrected, -X16, -X17)

# remove unwanted rows
# adm17 <- adm17[-c(51:52),] 
# adm18 <- adm18[-c(51:54),] 
# adm19 <- adm19[-c(51:54),] 
# adm20 <- adm20[-c(51:54),] 
adm17 <- adm17 %>% filter(`State Abbrev` != "NA")
adm18 <- adm18 %>% filter(`State Abbrev` != "NA")
adm19 <- adm19 %>% filter(`State Abbrev` != "NA")
adm20 <- adm20 %>% filter(`State Abbrev` != "NA")

# add year variable
adm17$year <- "2017"
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"

# combine data and remove unwanted data (NA, etc)
adm <- rbind(adm17, adm18, adm19, adm20)
adm <- adm %>% select(-`State Abbrev`)
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

adm_change <- adm_change %>% filter(year != 2017)
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

pop_change <- pop_change %>% filter(year != 2017)
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

# get 2019 budget data
budget <- read.csv("data/NASBO - State Corrections Expenditures - 1991 to current.csv")
budget <- budget %>% filter(Year == "2019" | Year == "2020") %>% select(States = State, Year, Budget = `Corrections....Total.Funds`)
budget <- budget %>% filter(States != "Puerto Rico")

# get 2020 budget data
budget_2020 <- read.csv("data/budget_2020.csv")
budget_2020$Year <- "2020"
budget_2020 <- budget_2020 %>% select(States, Year, Budget = budget)

# combine data
budget <- rbind(budget, budget_2020) 

# get cost data
costs <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs <- costs %>% select(`State Abbrev`, States, cost = `State Reported CostPerDay`)

################################################################# need to add updated 2020 costs

# costs_adm <- adm_long %>%
#   filter(category == "Total.violation.admissions"|
#          category == "Technical.probation.violation.admissions"|
#          category == "Technical.parole.violation.admissions") %>% select(States, year, category, count)

# creat costs_pop df
costs_pop <- population %>% select(States, year, Total.violation.population, Technical.probation.violation.population, Technical.parole.violation.population)

# add technical prob and parole together to get tech number
costs_pop <- costs_pop %>% mutate(total_population = Total.violation.population,
                                  technical_population = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-Technical.probation.violation.population,
         -Technical.parole.violation.population,
         -Total.violation.population)

# merge costs and population numbers
costs_pop_df <- merge(costs_pop, costs, by = "States")

# calc costs
costs_pop_df <- costs_pop_df %>% mutate(pop_sup_cost = total_population*cost*365,
                                        pop_tech_cost = technical_population*cost*365) %>%
  select(States,Year=year,pop_sup_cost,pop_tech_cost)

# remove 2017 and 2018
costs_pop_df <- costs_pop_df %>% filter(Year != 2017 & Year != 2018)

# calc by millions
costs_pop_df$pop_sup_cost <- (costs_pop_df$pop_sup_cost)/1000000
costs_pop_df$pop_tech_cost <- (costs_pop_df$pop_tech_cost)/1000000

# round - add $ and M
costs_pop_df$pop_sup_cost <- paste("$", round(costs_pop_df$pop_sup_cost, 1), "M", sep="")
costs_pop_df$pop_tech_cost <- paste("$", round(costs_pop_df$pop_tech_cost, 1), "M", sep="")

# add costs and budgets together
expenditures <- merge(budget, costs_pop_df, by = c("States", "Year"))

# replace NAs with "No Data"
expenditures[expenditures == "$NAM"] <- "No Data"  

# rename categories
expenditures$`DOC Budget` <- expenditures$Budget
expenditures$`DOC Cost to Incarcerate Supervision Violators` <- expenditures$pop_sup_cost
expenditures$`DOC Cost to Incarcerate Technical Supervision Violators` <- expenditures$pop_tech_cost
expenditures <- expenditures %>% select(-pop_sup_cost,-pop_tech_cost,-Budget)
# Add expenditures to state name for automation later
expenditures$States <- paste(expenditures$States, "_Expenditures", sep="")

