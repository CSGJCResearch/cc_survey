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
                     'mice',
                     'Hmisc'
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
q1 <- read_excel("data/2020_Quarter_4_Quarterly Table 1.xlsx", skip = 4)
q2 <- read_excel("data/2020_Quarter_4_Quarterly Table 2.xlsx", skip = 4)
q3 <- read_excel("data/2020_Quarter_4_Quarterly Table 3.xls", skip = 4)
q4 <- read_excel("data/2020_Quarter_4_Quarterly Table 4.xlsx", skip = 4)
census.xlsx <- read_excel("data/nst-est2019-01.xlsx", skip = 3)
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

######
# ucr
######

# dup for cleaning
crime_changes <- q1

# remove unwanted rows
crime_changes <- crime_changes[-c(1,10:14),]

# create min and max city sizes for merging
crime_changes <- crime_changes %>% mutate(city_min = case_when(`Population Group` == "Cities 1,000,000 or over" ~ 1000000,
                                                               `Population Group` == "Cities from 10,000 thru 24,999" ~ 10000,
                                                               `Population Group` == "Cities from 100,000 thru 249,999" ~ 100000,
                                                               `Population Group` == "Cities from 25,000 thru 49,999" ~ 25000,
                                                               `Population Group` == "Cities from 250,000 thru 499,999" ~ 250000,
                                                               `Population Group` == "Cities from 50,000 thru 99,999" ~ 50000,
                                                               `Population Group` == "Cities from 500,000 thru 999,999" ~ 500000,
                                                               `Population Group` == "Cities Under 10,000" ~ 0))
crime_changes <- crime_changes %>% mutate(city_max = case_when(`Population Group` == "Cities 1,000,000 or over" ~ 100000000,
                                                               `Population Group` == "Cities from 10,000 thru 24,999" ~ 24999,
                                                               `Population Group` == "Cities from 100,000 thru 249,999" ~ 249999,
                                                               `Population Group` == "Cities from 25,000 thru 49,999" ~ 49999,
                                                               `Population Group` == "Cities from 250,000 thru 499,999" ~ 499999,
                                                               `Population Group` == "Cities from 50,000 thru 99,999" ~ 99999,
                                                               `Population Group` == "Cities from 500,000 thru 999,999" ~ 999999,
                                                               `Population Group` == "Cities Under 10,000" ~ 10000))  
# remove commas and other punctuation
# crime_changes$state <- as.character(gsub("\\,", "", crime_changes$state))
# crime_changes$state <- as.character(gsub("\\+", "", crime_changes$state))

# rename variables
crime_changes <- crime_changes %>% select(violent_crime_change = `Violent crime`,
                                          murder_change = Murder,
                                          rape_change = Rape1,
                                          robbery_change = Robbery,
                                          aggravated_assault_change = `Aggravated Assault`,
                                          property_crime_change = `Property crime`,
                                          burgalry_change = Burglary,
                                          larceny_theft_change = `Larceny-theft`,
                                          motor_theft_change = `Motor vehicle theft`,
                                          arson_change = Arson,
                                          city_min, city_max)

# rows are missing state names, if NA, replace with state name in row above
ucr <- q4 %>% fill(State, City)

# remove unwanted rows
ucr <- ucr[-c(445:452),]

# remove commas from numbers and make numeric
ucr$Population1 <- as.character(gsub("\\,", "", ucr$Population1))
ucr$city_pop <- as.numeric(ucr$Population1)

# factor state
ucr$State <- factor(ucr$State)

# subset to 2020 (only data available)
ucr <- ucr %>% filter(Year == 2020)

# merge data about crime changes depending on the size of the city
# if the city pop lies within the min and max city sizes from q1, merge data
ucr <- data.table(ucr)
crime_changes <- data.table(crime_changes)
ucr[, dummy := city_pop]
setkey(crime_changes, city_min, city_max)
ucr_all = foverlaps(ucr, crime_changes, by.x=c("city_pop", "dummy"), nomatch=0L)[, dummy := NULL]

# rename and rearrange variables
ucr_all <- ucr_all %>% select(state = State, city = City, year = Year,
                              city_pop, 
                              violent_crime_change,
                              murder_change,
                              rape_change,
                              robbery_change,
                              aggravated_assault_change,
                              property_crime_change,
                              burgalry_change,
                              larceny_theft_change,
                              motor_theft_change,
                              arson_change,
                              violent_crime = `Violent crime`,
                              murder = Murder,
                              rape = Rape2,
                              robbery = Robbery,
                              aggravated_assault = `Aggravated Assault`,
                              property_crime = `Property crime`,
                              burgalry = Burglary,
                              larceny_theft = `Larceny-theft`,
                              motor_theft = `Motor vehicle theft`,
                              arson = Arson3)

# remove + and commas from number sand make numeric
ucr_all[] <- lapply(ucr_all, gsub, pattern=',', replacement='')
ucr_all$violent_crime_change <- as.character(gsub("\\+", "", ucr_all$violent_crime_change))
ucr_all$property_crime_change <- as.character(gsub("\\+", "", ucr_all$property_crime_change))
ucr_all$violent_crime <- as.numeric(ucr_all$violent_crime)
ucr_all$property_crime <- as.numeric(ucr_all$property_crime)
ucr_all$violent_crime_change <- as.numeric(ucr_all$violent_crime_change)
ucr_all$property_crime_change <- as.numeric(ucr_all$property_crime_change)
ucr_all$city_pop <- as.numeric(ucr_all$city_pop)

# aggregate up to state level
ucr_agg <- ucr_all %>% group_by(state) %>% summarise(ucr_pop = sum(city_pop),
                                                     violent_crime_avg = mean(violent_crime_change),
                                                     property_crime_avg = mean(property_crime_change),
                                                     state_violent_crime = sum(violent_crime),
                                                     state_property_crime = sum(property_crime))
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
df <- merge(ucr_agg, census, by = "state", all.x = TRUE)

# get % of pop that UCR data covers per state
df <- df %>% mutate(ucr_proportion = ucr_pop/census_pop_2019)

##################
# ANALYSIS
##################

# dup for cleaning
pop_change_imputed <- pop_change_story
adm_change_imputed <- adm_change_story

# select variables and filter to change in pop and admissions 2019-2020
pop_change_imputed <- pop_change_imputed %>% filter(year == 2020) %>%
  select(state = States, cc_population_change = Total.population.pct)
adm_change_imputed <- adm_change_imputed %>%  filter(year == 2020) %>% 
  select(state = States, cc_admissions_change = Total.admissions.pct)

########
# merge all data together
########

df_final <- merge(pop_change_imputed, adm_change_imputed, by = c("state"))
df_final <- merge(df_final, df, by = "state")

# rename and rearrange variables
df_final <- df_final %>% select(state, ucr_pop, census_pop_2019,ucr_proportion,
                    cc_population_change, cc_admissions_change,
                    violent_crime_avg, property_crime_avg, everything())

########
# Correlate change in crime from 2019 to 2020 with change in total prison admissions and pops 2019-2020
########

# subset data for analysis
df_sub <- df_final %>% select(-state,-ucr_pop,-census_pop_2019,-ucr_proportion,-state_violent_crime,-state_property_crime)

# compute correlation matrix
res <- cor(df_sub)
round(res, 2)

# compute correlation matrix
res2 <- rcorr(as.matrix(df_sub))
res2

# extract the correlation coefficients
res2$r

# extract p-values
res2$P

# custom function to format correlation matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# create correlation matrix
res2<-rcorr(as.matrix(df_sub[,1:4]))
flattenCorrMatrix(res2$r, res2$P)


