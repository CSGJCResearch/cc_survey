#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey
# by Mari Roberts
# 12/1/2020
#######################################

# load custom functions
# source("functions.R")

# load necessary packages
requiredPackages = c('dplyr',
                     'openxlsx',
                     'readr',
                     'reshape',
                     'ggplot2',
                     'dplyr',
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
    csgF <- 'csgjc/cc_survey/data'
  }
  if(thisLogin=="mari") {
    base <- '/Users'
    csgF <- 'csgjc/cc_survey/data'
  }
  wd <- paste(base,thisLogin,csgF,sep="/")
  return(wd)
}

# set working directory
wd <- getwd()
setwd(wd)

##########
# Import data
##########

# import survey responses / sanction info
survey_data <- read_excel("50-State Revocation Survey_October 27, 2020_12.25.xlsx")

# read admissions sheets
admissions_2017 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Admissions 2017")
admissions_2018 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Admissions 2018")
admissions_2019 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Admissions 2019")
admissions_2020 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Admissions 2020")

# read population sheets
population_2017 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Population 2017")
population_2018 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Population 2018")
population_2019 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Population 2019")
population_2020 <- read_excel("Data for web team 2020 v6.xlsx", sheet = "Population 2020")

# read in regions data
regions <- read.csv("regions.csv")

# remove excess rows and columns
admissions_2017 <- admissions_2017[-c(51,52),] # remove excess rows
admissions_2018 <- admissions_2018[-c(51:54),] # remove excess rows
admissions_2018 <- admissions_2018[,-c(11,12)] # remove excess columns
admissions_2019 <- admissions_2019[-c(51:54),] # remove excess rows
admissions_2019 <- admissions_2019[,-c(11,12)] # remove excess columns
# 2020 - removed months reported for now
admissions_2020 <- admissions_2020[-c(51:54),] # remove excess rows
admissions_2020 <- admissions_2020[,-c(11:17)] # remove excess columns

population_2017 <- population_2017[-c(51,52),] # remove excess rows
population_2017 <- population_2017[,-c(11)]    # remove excess columns
population_2018 <- population_2018[-c(51:54),] # remove excess rows
population_2019 <- population_2019[-c(51:54),] # remove excess rows
# 2020 - removed months reported for now
population_2020 <- population_2020[-c(51:54),] # remove excess rows
# population_2020 <- population_2020[,-c(14:17)] # remove excess columns

# add year and type
admissions_2017 <- admissions_2017 %>% mutate(year = "2017",
                                              type = "admissions")
admissions_2018 <- admissions_2018 %>% mutate(year = "2018",
                                              type = "admissions")
admissions_2019 <- admissions_2019 %>% mutate(year = "2019",
                                              type = "admissions")
population_2017 <- population_2017 %>% mutate(year = "2017",
                                              type = "population")
population_2018 <- population_2018 %>% mutate(year = "2018",
                                              type = "population")
population_2019 <- population_2019 %>% mutate(year = "2019",
                                              type = "population")
population_2020 <- population_2020 %>% mutate(year = "2020",
                                              type = "population")
admissions_2020 <- admissions_2020 %>% mutate(year = "2020",
                                              type = "admissions")

# add data together
admissions_df <- rbind(admissions_2017, admissions_2018, admissions_2019, admissions_2020)
population_df <- rbind(population_2017, population_2018, population_2019, population_2020)
# final_df <- plyr::rbind.fill(admissions_df, population_df) error deleting data

# rename variables in admissions df
admissions_df <- admissions_df %>% select(state.abb = "State Abbrev",
                                          state = "States",
                                          total = "Total admissions", 
                                          violations = "Total violation admissions",    
                                          probation = "Total probation violation admissions", 
                                          parole = "Total parole violation admissions",   
                                          tech.probation = "Technical probation violation admissions",
                                          tech.parole = "Technical parole violation admissions",  
                                          year,
                                          type,
                                          new.offense.parole = "New offense parole violation admissions",
                                          new.offense.prob = "New offense probation violation admissions")
# rename variables in population df
population_df <- population_df %>% select(state.abb = "State Abbrev",
                                          state = "States",
                                          total = "Total population", 
                                          violations = "Total violation population",    
                                          probation = "Total probation violation population", 
                                          parole = "Total parole violation population",   
                                          tech.probation = "Technical probation violation population",
                                          tech.parole = "Technical parole violation population",  
                                          year,
                                          type,
                                          new.offense.parole = "New offense parole violation population",
                                          new.offense.prob = "New offense probation violation population")

# add population and admissions dfs together
df <- rbind(admissions_df, population_df)

# convert data types
df$state.abb <- factor(df$state.abb)
df$state <- factor(df$state)
df$type <- factor(df$type)
df$year <- as.numeric(df$year)

# add region info
df <- merge(df, regions, by = "state.abb")
