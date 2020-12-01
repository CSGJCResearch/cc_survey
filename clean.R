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

# set custom working directory
getwd <- function(){
  thisLogin <- Sys.info()['login']
  # if(thisLogin=="amund") {
  #   base <- '/home'
  #   csgjcF <- 'working directory'
  # }
  if(thisLogin=="mari"){
    base <- '/Users'
    csgjcF <- 'csgjc/cc_survey'
  }
  wd <- paste(base,thisLogin,csgjcF,sep="/")
  return(wd)
}

# set working directory
wd <- getwd()
setwd(wd)

##########
# Import data
##########

