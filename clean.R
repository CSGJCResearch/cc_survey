#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey
# by Mari Roberts and Amund Talleksen
# 12/1/2020
#######################################

# load custom functions
# source("functions.R")

# load necessary packages
requiredPackages = c('dplyr',
                     'openxlsx')
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
  #   canyF <- 'directory'
  # }
  if(thisLogin=="mr4909"){
    base <- '/Users'
    canyF <- 'csgjc/cc_survey/data'
  }
  if(thisLogin=="mari") {
    base <- '/Users'
    canyF <- 'csgjc/cc_survey/data'
  }
  wd <- paste(base,thisLogin,canyF,sep="/")
  return(wd)
}

# set working directory
wd <- getwd()
setwd(wd)

##########
# Import data
##########

