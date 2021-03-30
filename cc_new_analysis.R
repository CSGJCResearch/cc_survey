#######################################
# Confined and Costly Survey
# New Analysis Using UCR Data
# by Mari Roberts
# 3/29/2021
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
                     'mice'
)
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# get working directory depending on login
getwd <- function(){
  thisLogin <- Sys.info()['login']
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

# read in cc survey data with imputed values
source("cc_story.R")

# import data
q4 <- read_excel("data/2020_Quarter_4_Quarterly Table 4.xlsx", skip = 4)
census.xlsx <- read_excel("data/nst-est2019-01.xlsx", skip = 3)
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

######
# ucr
######

# rows are missing state names, if NA, replace with state name in row above
ucr <- q4 %>% fill(State, City)

# remove unwanted rows
ucr <- ucr[-c(445:452),]

# remove commas from numbers and make numeric
ucr$Population1 <- as.character(gsub("\\,", "", ucr$Population1))
ucr$population <- as.numeric(ucr$Population1)

# factor state
ucr$State <- factor(ucr$State)

# subset to 2020 (only data available)
ucr <- ucr %>% filter(Year == 2020)

# aggregate up to state level
ucr_agg <- ucr %>% group_by(State) %>% summarise(ucr_pop = sum(population))
ucr_agg <- ucr_agg %>% select(state = State, ucr_pop)

# length(unique(ucr$State)) # only 30 states

######
# census
######

# dup for cleaning and select variables (select 2019 for pop year)
census <- census.xlsx %>% select(state = `...1`, census_pop_2019 = `2019`)

# remove periods in state names
census$state <- as.character(gsub("\\.", "", census$state))

# remove unwanted rows
census <- census[-c(1:5,57:63),]

# factor state
census$state <- factor(census$state)

######
# merge with census data 
######

# merge
pop_data <- merge(ucr_agg, census, by = "state", all.x = TRUE)

# get % of pop that UCR data covers per state
pop_data <- pop_data %>% mutate(ucr_prop = ucr_pop/census_pop_2019)

##################
# Correlate change in crime from 2019 to 2020 with change in total prison admissions and pops 2019-2020
##################

# dup for cleaning
pop_change_imputed <- pop_change_story
adm_change_imputed <- adm_change_story

# select variables and filter to change in pop and admissions 2019-2020
pop_change_imputed <- pop_change_imputed %>% filter(year == 2020) %>%
  select(state = States, cc_population_change = Total.population.pct)
adm_change_imputed <- adm_change_imputed %>%  filter(year == 2020) %>% 
  select(state = States, cc_admissions_change = Total.admissions.pct)

# merge all data together
df <- merge(pop_change_imputed, adm_change_imputed, by = c("state"))
df <- merge(pop_data, df, by = "state")
