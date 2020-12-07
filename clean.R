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
                     'dplyr')
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

# set working directory
wd <- getwd()
setwd(wd)

##########
# Import data
##########

# import survey responses
survey_data <- read_excel("data/50-State Revocation Survey_October 27, 2020_12.25.xlsx")

# import population data
population <- read.xlsx("data/population.xlsx")
population <- population[-c(51,52),]

# import population/admissions data
df <- read.xlsx("data/population_admissions.xlsx")

# convert to factor levels
df$state_abb <- factor(df$state_abb)
df$state <- factor(df$state)
df$type <- factor(df$type)
df$region <- factor(df$region)
df$year <- factor(df$year)
                            