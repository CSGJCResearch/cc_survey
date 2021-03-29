#######################################
# Confined and Costly Survey
# Story
# by Mari Roberts
# 3/29/2021
#######################################

# read automated_clean to get data
# source("automated_clean.R")

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
                     'scales'
)
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 10),size=10,face="bold"),
                     #axis.text.y = element_text(size=10),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size=10,face="bold"),
                     legend.position = "top",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))

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
population17 <- population17 %>% filter(`State Abbrev` != "NA")
population18 <- population18 %>% filter(`State Abbrev` != "NA")
population19 <- population19 %>% filter(`State Abbrev` != "NA")
population20 <- population20 %>% filter(`State Abbrev` != "NA")

# issue with pop 20, some rows were duplicated with NAs - weird bug?
population20 <- population20 %>% arrange(States)
population20 <- population20[-c(4,12,21,28,35,47,50),] # remove duplicate rows

# add year variable
population17$year <- "2017"
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"

# combine pop data
population <- rbind(population17, population18, population19, population20)
population <- population %>% select(-`State Abbrev`)

# read excel admissions data for 2017-2019
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

adm_backup <- adm
pop_backup <- population

##########
# Imputation of multiple columns 
# https://www.guru99.com/r-replace-missing-values.html#3
# https://predictivehacks.com/how-to-impute-missing-values-in-r/
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
##########

# function that will impute missing values of a dataframe depending on the data type
getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####
# admissions
####

# if the column data type is num, impute with mean
for (cols in colnames(adm)) {
  if (cols %in% names(adm[,sapply(adm, is.numeric)])) {
    adm <- adm %>% mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    adm <- adm %>% mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
  }
}

####
# population
####

# if the column data type is num, impute with mean
for (cols in colnames(population)) {
  if (cols %in% names(population[,sapply(population, is.numeric)])) {
    population <- population %>% mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    population <- population %>% mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
  }
}

############
# Admissions
############

# find % changes by category

# set up tables for change
adm_change_story <- adm %>% select(States, year, everything()) %>% arrange(desc(States))
pop_change_story <- population %>% select(States, year, everything()) %>% arrange(desc(States))

# calculate tech violations
adm_change_story <- adm_change_story %>% 
  mutate(Technical.violations = Technical.probation.violation.admissions + Technical.parole.violation.admissions)
pop_change_story <- pop_change_story %>% 
  mutate(Technical.violations = Technical.probation.violation.population + Technical.parole.violation.population)

# remove 2017
adm_change_story <- adm_change_story %>% filter(year != 2017)
pop_change_story <- pop_change_story %>% filter(year != 2017)

# remove unwanted variables
adm_change_story <- adm_change_story %>% select(-Total.probation.violation.admissions, -Total.parole.violation.admissions, 
                                    -Technical.probation.violation.admissions,-Technical.parole.violation.admissions,
                                    -New.offense.parole.violation.admissions,-New.offense.probation.violation.admissions)
pop_change_story <- pop_change_story %>% select(-Total.probation.violation.population, -Total.parole.violation.population, 
                                    -Technical.probation.violation.population,-Technical.parole.violation.population,
                                    -New.offense.parole.violation.population,-New.offense.probation.violation.population)

#######
# changes in admissions
#######

# calculate percent change         
adm_change_story <- adm_change_story %>% group_by(States) %>% mutate(Total.admissions.pct = (Total.admissions / dplyr::lag(Total.admissions) -1)*100)
adm_change_story <- adm_change_story %>% group_by(States) %>% mutate(Total.violation.admissions.pct = (Total.violation.admissions / dplyr::lag(Total.violation.admissions) -1)*100)
adm_change_story <- adm_change_story %>% group_by(States) %>% mutate(Technical.violations.pct = (Technical.violations / dplyr::lag(Technical.violations) -1)*100)

# calculate percent change         
pop_change_story <- pop_change_story %>% group_by(States) %>% mutate(Total.population.pct = (Total.population / dplyr::lag(Total.population) -1)*100)
pop_change_story <- pop_change_story %>% group_by(States) %>% mutate(Total.violation.population.pct = (Total.violation.population / dplyr::lag(Total.violation.population) -1)*100)
pop_change_story <- pop_change_story %>% group_by(States) %>% mutate(Technical.violations.pct = (Technical.violations / dplyr::lag(Technical.violations) -1)*100)

# create national estimate for admissions change from 2018 to 2020 
adm_national <- adm_change_story %>% group_by(year) %>% summarise(total.admissions = sum(Total.admissions))
adm_national <- adm_national %>%
  mutate(total.admissions.pct = (total.admissions - lag(total.admissions)) / lag(total.admissions),
         total.admissions.change = total.admissions - lag(total.admissions))

# create national estimate for total violators change from 2018 to 2020 
adm_national_violators <- adm_change_story %>% group_by(year) %>% summarise(total.violation.admissions = sum(Total.violation.admissions))
adm_national_violators <- adm_national_violators %>%
  mutate(total.violation.admissions.pct = (total.violation.admissions - lag(total.violation.admissions)) / lag(total.violation.admissions),
         total.violation.admissions.change = total.violation.admissions - lag(total.violation.admissions))

# create national estimate for technical violators change from 2018 to 2020 
adm_national_tech_violators <- adm_change_story %>% group_by(year) %>% summarise(tech.violations = sum(Technical.violations))
adm_national_tech_violators <- adm_national_tech_violators %>%
  mutate(tech.violation.admissions.pct = (tech.violations - lag(tech.violations)) / lag(tech.violations),
         tech.violation.admissions.change = tech.violations - lag(tech.violations))

adm_decline <- merge(adm_national, adm_national_violators, by = "year")
adm_decline <- merge(adm_decline, adm_national_tech_violators, by = "year")

########
# changes in population
########

# create national estimate for population change from 2018 to 2020 
pop_national <- pop_change_story %>% group_by(year) %>% summarise(total.population = sum(Total.population))
pop_national <- pop_national %>%
  mutate(total.population.pct = (total.population - lag(total.population)) / lag(total.population),
         total.population.change = total.population - lag(total.population))

# create national estimate for total violators change from 2018 to 2020 
pop_national_violators <- pop_change_story %>% group_by(year) %>% summarise(total.violation.population = sum(Total.violation.population))
pop_national_violators <- pop_national_violators %>%
  mutate(total.violation.population.pct = (total.violation.population - lag(total.violation.population)) / lag(total.violation.population),
         total.violation.population.change = total.violation.population - lag(total.violation.population))

# create national estimate for technical violators change from 2018 to 2020 
pop_national_tech_violators <- pop_change_story %>% group_by(year) %>% summarise(tech.violations = sum(Technical.violations))
pop_national_tech_violators <- pop_national_tech_violators %>%
  mutate(tech.violation.population.pct = (tech.violations - lag(tech.violations)) / lag(tech.violations),
         tech.violation.population.change = tech.violations - lag(tech.violations))

pop_decline <- merge(pop_national, pop_national_violators, by = "year")
pop_decline <- merge(pop_decline, pop_national_tech_violators, by = "year")

##################################################
# Costs
##################################################

# get cost data for 2021
costs <- read.csv("data/Data for web team 2020 v6 CORRECTED/Survey 2021-Table 1.csv")

# select general cost info
costs <- costs %>% select(States, Cost)

# remove dollar sign
costs$cost_2020 = as.numeric(gsub("\\$", "", costs$Cost))
costs <- costs %>% select(-Cost)

# get cost data from 2019
costs2019 <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs2019 <- costs2019 %>% select(States, cost_2019 = `State Reported CostPerDay`)

# replace NAs in 2020 cost data with data from 2019
setDT(costs); setDT(costs2019)
costs[is.na(cost_2020), cost_2020 := costs2019[.SD, on=.(States), x.cost_2019]]
costs <- merge(costs, costs2019, by = "States")

# creat costs_pop df
costs_pop_2019 <- population %>% filter(year == 2019) %>% select(States, Total.violation.population, Technical.probation.violation.population, Technical.parole.violation.population)
costs_pop_2020 <- population %>% filter(year == 2020) %>% select(States, Total.violation.population, Technical.probation.violation.population, Technical.parole.violation.population)

# add technical prob and parole together to get tech number
costs_pop_2019 <- costs_pop_2019 %>%  mutate(total_population_2019 = Total.violation.population,
                                             technical_population_2019 = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-Technical.probation.violation.population,
         -Technical.parole.violation.population,
         -Total.violation.population)
costs_pop_2020 <- costs_pop_2020 %>%  mutate(total_population_2020 = Total.violation.population,
                                             technical_population_2020 = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-Technical.probation.violation.population,
         -Technical.parole.violation.population,
         -Total.violation.population)

# merge costs and population numbers
costs_pop_df <- merge(costs_pop_2019, costs, by = "States", all.x = TRUE, all.y = TRUE)
costs_pop_df <- merge(costs_pop_df, costs_pop_2020, by = "States", all.x = TRUE, all.y = TRUE)

# calc costs
costs_pop_df <- costs_pop_df %>% mutate(pop_sup_cost_2019 = total_population_2019*cost_2019*365,
                                        pop_tech_cost_2019 = technical_population_2019*cost_2019*365,
                                        pop_sup_cost_2020 = total_population_2020*cost_2020*365,
                                        pop_tech_cost_2020 = technical_population_2020*cost_2020*365) 

# rearrange data
costs_pop_df <- costs_pop_df %>% select(States, cost_2019,cost_2020,
                                        pop_sup_cost_2019,pop_sup_cost_2020,
                                        pop_tech_cost_2019,pop_tech_cost_2020,
                                        everything())

avg_sup_cost <- mean(costs_pop_df$pop_sup_cost_2020)
avg_sup_cost # $178,919,717

########
# prison info
########

prisons_data <- read_csv("data/CSG revocations model v0.3_010421.csv")
prisons_data <- prisons_data %>% select(State, `Data Year`, `Number of facilities [input]`,`State-wide capacity [input]`)

########
# write csv files
########

# write csv files to share and add to shared folder
write.csv(adm_decline, "shared_data/cc_admissions_changes.csv")
write.csv(pop_decline, "shared_data/cc_population_changes.csv")
write.csv(costs_pop_df, "shared_data/supervising_costs.csv")
write.csv(prisons_data, "shared_data/prisons_data.csv")

########
# data for comms
########

# # tables for review
# adm_change_comms_2019 <- adm_change_story %>% select(States, year, Total.admissions, Total.violation.admissions, Technical.violations.admissions = Technical.violations,
#                                                Total.admissions.pct, Total.violation.admissions.pct,Technical.violations.admissions.pct = Technical.violations.pct) %>% filter(year == 2019)
# adm_change_comms_2020 <- adm_change_story %>% select(States, year, Total.admissions, Total.violation.admissions, Technical.violations.admissions = Technical.violations,
#                                                Total.admissions.pct, Total.violation.admissions.pct,Technical.violations.admissions.pct = Technical.violations.pct) %>% filter(year == 2020)
# adm_change_comms <- rbind(adm_change_comms_2019, adm_change_comms_2020)
# 
# # tables for review
# pop_change_comms_2019 <- pop_change_story %>% select(States, year, Total.population, Total.violation.population, Technical.violations.population = Technical.violations,
#                                                Total.population.pct, Total.violation.population.pct, Technical.violations.population.pct = Technical.violations.pct) %>% filter(year == 2019)
# pop_change_comms_2020 <- pop_change_story %>% select(States, year, Total.population, Total.violation.population, Technical.violations.population = Technical.violations,
#                                                Total.population.pct, Total.violation.population.pct, Technical.violations.population.pct = Technical.violations.pct) %>% filter(year == 2020)
# pop_change_comms <- rbind(pop_change_comms_2019, pop_change_comms_2020)
# 
# adm_pop_change_comms <- merge(adm_change_comms, pop_change_comms, by = c("States","year"),all.x = TRUE, all.y = TRUE)
# 
# # write csv for comms
# write.csv(adm_pop_change_comms, "shared_data/cc_adm_pop_change_comms.csv")


##################################################
# Changes in admissions by state
##################################################

# read in data
source("automated_clean.R")

# 1. Chart with states with largest drops overall admissions

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# arrange data by largest drops
largest_drops_adm <- df %>% arrange(Total.admissions.pct) %>% head(10)

# only look at states with largest drops
df <- df %>% filter(States %in% largest_drops_adm$States)

plot1 <- ggplot(df, aes(x=reorder(States, -Total.admissions), y=Total.admissions, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  ggtitle("States with the Largest Drops in Total Admissions") + 
  theme_bw() + theme_csgjc + 
  geom_text(aes(label = scales::comma(round(Total.admissions), accuracy=1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),vjust=-.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
  geom_label(data = df, vjust = 1,
             mapping = aes(label = ifelse(Total.admissions.pct>0,paste0("", round(Total.admissions.pct,1),"%"),
                                          paste0("",Total.admissions.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) 
plot1

# 2. Chart with states with largest drops in supervision violation admissions
# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

# arrange data by largest drops
largest_drops_viol_adm <- df %>% arrange(Total.violation.admissions.pct) %>% head(10)

# only look at states with largest drops
df <- df %>% filter(States %in% largest_drops_viol_adm$States)
df$Total.violation.admissions <- round(df$Total.violation.admissions,0)

plot1 <- ggplot(df, aes(x=reorder(States, -Total.violation.admissions), y=Total.violation.admissions, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  ggtitle("States with the Largest Drops in Violation Admissions") + 
  theme_bw() + theme_csgjc + 
  geom_text(aes(label = scales::comma(round(Total.violation.admissions), accuracy=1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),vjust=-.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
  geom_label(data = df, vjust = 1,
             mapping = aes(label = ifelse(Total.violation.admissions.pct>0,paste0("", round(Total.violation.admissions.pct,1),"%"),
                                          paste0("",Total.violation.admissions.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) 
plot1

# 3. Chart with states with largest drops in technical violation admissions
df_2020 <- adm_change %>% filter(year == 2020) 
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Technical.violations.pct <- round(df$Technical.violations.pct ,1)

# arrange data by largest drops
largest_drops_viol_adm <- df %>% arrange(Technical.violations.pct) %>% head(10)

# only look at states with largest drops
df <- df %>% filter(States %in% largest_drops_viol_adm$States)
df$Technical.violations <- round(df$Technical.violations,0)

plot1 <- ggplot(df, aes(x=reorder(States, -Technical.violations), y=Technical.violations, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  ggtitle("States with the Largest Drops in Technical Violation Admissions") + 
  theme_bw() + theme_csgjc + 
  geom_text(aes(label = scales::comma(round(Technical.violations), accuracy=1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),vjust=-.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
  geom_label(data = df, vjust = 1,
             mapping = aes(label = ifelse(Technical.violations.pct>0,paste0("", round(Technical.violations.pct,1),"%"),
                                          paste0("",Technical.violations.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) 
plot1

########
# data for comms team
########

# adm
adm_change_comms_2018 <- adm_change %>% select(States, year, Total.admissions, Total.violation.admissions, Technical.violations.admissions = Technical.violations,
                                               Total.admissions.pct, Total.violation.admissions.pct,Technical.violations.admissions.pct = Technical.violations.pct) %>% filter(year == 2018)
adm_change_comms_2019 <- adm_change %>% select(States, year, Total.admissions, Total.violation.admissions, Technical.violations.admissions = Technical.violations,
                                                     Total.admissions.pct, Total.violation.admissions.pct,Technical.violations.admissions.pct = Technical.violations.pct) %>% filter(year == 2019)
adm_change_comms_2020 <- adm_change %>% select(States, year, Total.admissions, Total.violation.admissions, Technical.violations.admissions = Technical.violations,
                                                     Total.admissions.pct, Total.violation.admissions.pct,Technical.violations.admissions.pct = Technical.violations.pct) %>% filter(year == 2020)
adm_change_comms <- rbind(adm_change_comms_2018, adm_change_comms_2019, adm_change_comms_2020)

# pop
pop_change_comms_2018 <- pop_change %>% select(States, year, Total.population, Total.violation.population, Technical.violations.population = Technical.violations,
                                               Total.population.pct, Total.violation.population.pct, Technical.violations.population.pct = Technical.violations.pct) %>% filter(year == 2018)
pop_change_comms_2019 <- pop_change %>% select(States, year, Total.population, Total.violation.population, Technical.violations.population = Technical.violations,
                                                     Total.population.pct, Total.violation.population.pct, Technical.violations.population.pct = Technical.violations.pct) %>% filter(year == 2019)
pop_change_comms_2020 <- pop_change %>% select(States, year, Total.population, Total.violation.population, Technical.violations.population = Technical.violations,
                                                     Total.population.pct, Total.violation.population.pct, Technical.violations.population.pct = Technical.violations.pct) %>% filter(year == 2020)
pop_change_comms <- rbind(pop_change_comms_2018, pop_change_comms_2019, pop_change_comms_2020)

adm_pop_change_comms <- merge(adm_change_comms, pop_change_comms, by = c("States","year"),all.x = TRUE, all.y = TRUE)

# write csv for comms
write.csv(adm_pop_change_comms, "shared_data/cc_adm_pop_change_comms.csv")
