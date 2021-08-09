#######################################
# Confined and Costly
# Impute values, national estimates, state estimates and costs
# by Joshua Mallett
#######################################

# load necessary packages
requiredPackages = c('rstudioapi',
                     'dplyr',
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
                     'VIM',
                     'finalfit',
                     'janitor',
                     'Hmisc'
)
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

#set wd based on your current program path (for collaboration)
#requires rstudioapi package
setwd(dirname(getActiveDocumentContext()$path))

# read automated_clean to get data
source("clean.R")

#rename columns
names(admissions)[names(admissions) == "States"] <- "states"
names(population)[names(population) == "States"] <- "states"

# dup for cleaning
adm_analysis <- admissions
pop_analysis <- population

# merge together
adm_pop_analysis <- merge(admissions, population, by = c("states","year"))

# set up tables for change
adm_pop_analysis <- adm_pop_analysis %>% select(states, year, everything()) %>% arrange(desc(states))

# remove 2017
table(adm_pop_analysis$year)
adm_pop_analysis <- adm_pop_analysis %>% filter(year != 2017) #there is no 2017 year in the data

################################################################################
# IMPUTATION
# MICE
# National Estimates w/95% CI's
################################################################################

#New National Variables/columns
m.imp <- adm_pop_analysis %>% mutate(
  overall_admissions                  = Total.admissions,
  admissions_for_violations           = Total.violation.admissions,
  admissions_for_technical_violations = Technical.probation.violation.admissions + Technical.parole.violation.admissions,
  admissions_for_new_crime_violations = New.offense.probation.violation.admissions + New.offense.parole.violation.admissions,
  overall_population                  = Total.population,
  violator_population                 = Total.violation.population,
  technical_violator_population       = Technical.probation.violation.population + Technical.parole.violation.population,
  new_crime_violator_population       = New.offense.probation.violation.population + New.offense.parole.violation.population
) %>%
  select(states, year,
         overall_admissions,
         admissions_for_violations,
         admissions_for_technical_violations,
         admissions_for_new_crime_violations,
         overall_population,
         violator_population,
         technical_violator_population,
         new_crime_violator_population)

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(m.imp,2,pMiss)

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method, predictive mean matching
temp_data <- mice(m.imp,m=5,maxit=50,meth='pmm',seed=500)
summary(temp_data)

# check imputed data
# each observation (first column left) within each imputed data set (first row at the top)
temp_data$imp$overall_admissions

#each individual imputed data frame (m=5)
#use this for modeling code at the state level
mice_imputed_data1 <- complete(temp_data,1)
mice_imputed_data2 <- complete(temp_data,2)
mice_imputed_data3 <- complete(temp_data,3)
mice_imputed_data4 <- complete(temp_data,4)
mice_imputed_data5 <- complete(temp_data,5)

######
#produce confidence intervals
#post multiple imputation t-test
#grab response columns for creating estimated counts/CIs
tablevals<- colnames(mice_imputed_data1[3:10])

#aggregated data: national level

###IMPUTED DATA m=1
for (i in tablevals) {
  natmice <- setNames(aggregate(mice_imputed_data1[[i]], by=list(Category=mice_imputed_data1$year), FUN=sum)[2],i)
  assign(i,natmice,envir = .GlobalEnv)
}
mice_imputed_data1.nat <- do.call(cbind,mget(tablevals)) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
year                  <- c('2018','2019','2020')
mice_imputed_data1.nat <- cbind(mice_imputed_data1.nat,year)

# add labels
var.labels = c(year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crime violator population"
)
label(mice_imputed_data1.nat) = as.list(var.labels[match(names(mice_imputed_data1.nat), names(var.labels))])

#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (i in tablevals) {
  #j = 1,2,3 - 1=2018, 2=2019, 3=2020
  for (j in 1:3) { 
    test<-data.frame(CI95=poisson.test(mice_imputed_data1.nat[j,i], conf.level = 0.95)$conf.int[1:2])
    colnames(test)[colnames(test)=="CI95"] <- paste0(i,"1.",j)
    assign(paste0(i,"1.",j),test,envir = .GlobalEnv)
  }
}
mice_imputed_data1.natCI <- do.call(cbind,mget(c(paste0(tablevals,"1.1"),paste0(tablevals,"1.2"),paste0(tablevals,"1.3")))) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#rename columns to indicate which imputed data frame (m=1)
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "overall_admissions"] <- "overall_admissions1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "admissions_for_violations"] <- "admissions_for_violations1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "admissions_for_technical_violations"] <- "admissions_for_technical_violations1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "admissions_for_new_crime_violations"] <- "admissions_for_new_crime_violations1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "overall_population"] <- "overall_population1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "violator_population"] <- "violator_population1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "technical_violator_population"] <- "technical_violator_population1"
names(mice_imputed_data1.nat)[names(mice_imputed_data1.nat) == 
                                "new_crime_violator_population"] <- "new_crime_violator_population1"

###IMPUTED DATA m=2
for (i in tablevals) {
  natmice <- setNames(aggregate(mice_imputed_data2[[i]], by=list(Category=mice_imputed_data2$year), FUN=sum)[2],i)
  assign(i,natmice,envir = .GlobalEnv)
}
mice_imputed_data2.nat <- do.call(cbind,mget(tablevals)) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
year                  <- c('2018','2019','2020')
mice_imputed_data2.nat <- cbind(mice_imputed_data2.nat,year)

# add labels
var.labels = c(year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crime violator population"
)
label(mice_imputed_data2.nat) = as.list(var.labels[match(names(mice_imputed_data2.nat), names(var.labels))])

#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (i in tablevals) {
  #j = 1,2,3 - 1=2018, 2=2019, 3=2020
  for (j in 1:3) { 
    test<-data.frame(CI95=poisson.test(mice_imputed_data2.nat[j,i], conf.level = 0.95)$conf.int[1:2])
    colnames(test)[colnames(test)=="CI95"] <- paste0(i,"2.",j)
    assign(paste0(i,"2.",j),test,envir = .GlobalEnv)
  }
}
mice_imputed_data2.natCI <- do.call(cbind,mget(c(paste0(tablevals,"2.1"),paste0(tablevals,"2.2"),paste0(tablevals,"2.3")))) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#rename columns to indicate which imputed data frame (m=2)
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "overall_admissions"]                  <- "overall_admissions2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "admissions_for_violations"]           <- "admissions_for_violations2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "admissions_for_technical_violations"] <- "admissions_for_technical_violations2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "admissions_for_new_crime_violations"] <- "admissions_for_new_crime_violations2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "overall_population"]                  <- "overall_population2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "violator_population"]                 <- "violator_population2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "technical_violator_population"]       <- "technical_violator_population2"
names(mice_imputed_data2.nat)[names(mice_imputed_data2.nat) == 
                                "new_crime_violator_population"]       <- "new_crime_violator_population2"

###IMPUTED DATA m=3
for (i in tablevals) {
  natmice <- setNames(aggregate(mice_imputed_data3[[i]], by=list(Category=mice_imputed_data3$year), FUN=sum)[2],i)
  assign(i,natmice,envir = .GlobalEnv)
}
mice_imputed_data3.nat <- do.call(cbind,mget(tablevals)) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
year                  <- c('2018','2019','2020')
mice_imputed_data3.nat <- cbind(mice_imputed_data3.nat,year)

# add labels
var.labels = c(year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crime violator population"
)
label(mice_imputed_data3.nat) = as.list(var.labels[match(names(mice_imputed_data3.nat), names(var.labels))])

#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (i in tablevals) {
  #j = 1,2,3 - 1=2018, 2=2019, 3=2020
  for (j in 1:3) { 
    test<-data.frame(CI95=poisson.test(mice_imputed_data3.nat[j,i], conf.level = 0.95)$conf.int[1:2])
    colnames(test)[colnames(test)=="CI95"] <- paste0(i,"3.",j)
    assign(paste0(i,"3.",j),test,envir = .GlobalEnv)
  }
}
mice_imputed_data3.natCI <- do.call(cbind,mget(c(paste0(tablevals,"3.1"),paste0(tablevals,"3.2"),paste0(tablevals,"3.3")))) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#rename columns to indicate which imputed data frame (m=3)
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "overall_admissions"]                  <- "overall_admissions3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "admissions_for_violations"]           <- "admissions_for_violations3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "admissions_for_technical_violations"] <- "admissions_for_technical_violations3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "admissions_for_new_crime_violations"] <- "admissions_for_new_crime_violations3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "overall_population"]                  <- "overall_population3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "violator_population"]                 <- "violator_population3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "technical_violator_population"]       <- "technical_violator_population3"
names(mice_imputed_data3.nat)[names(mice_imputed_data3.nat) == 
                                "new_crime_violator_population"]       <- "new_crime_violator_population3"

###IMPUTED DATA m=4
for (i in tablevals) {
  natmice <- setNames(aggregate(mice_imputed_data4[[i]], by=list(Category=mice_imputed_data4$year), FUN=sum)[2],i)
  assign(i,natmice,envir = .GlobalEnv)
}
mice_imputed_data4.nat <- do.call(cbind,mget(tablevals)) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
year                  <- c('2018','2019','2020')
mice_imputed_data4.nat <- cbind(mice_imputed_data4.nat,year)

# add labels
var.labels = c(year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crime violator population"
)
label(mice_imputed_data4.nat) = as.list(var.labels[match(names(mice_imputed_data4.nat), names(var.labels))])

#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (i in tablevals) {
  #j = 1,2,3 - 1=2018, 2=2019, 3=2020
  for (j in 1:3) { 
    test<-data.frame(CI95=poisson.test(mice_imputed_data4.nat[j,i], conf.level = 0.95)$conf.int[1:2])
    colnames(test)[colnames(test)=="CI95"] <- paste0(i,"4.",j)
    assign(paste0(i,"4.",j),test,envir = .GlobalEnv)
  }
}
mice_imputed_data4.natCI <- do.call(cbind,mget(c(paste0(tablevals,"4.1"),paste0(tablevals,"4.2"),paste0(tablevals,"4.3")))) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#rename columns to indicate which imputed data frame (m=4)
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "overall_admissions"]                  <- "overall_admissions4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "admissions_for_violations"]           <- "admissions_for_violations4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "admissions_for_technical_violations"] <- "admissions_for_technical_violations4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "admissions_for_new_crime_violations"] <- "admissions_for_new_crime_violations4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "overall_population"]                  <- "overall_population4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "violator_population"]                 <- "violator_population4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "technical_violator_population"]       <- "technical_violator_population4"
names(mice_imputed_data4.nat)[names(mice_imputed_data4.nat) == 
                                "new_crime_violator_population"]       <- "new_crime_violator_population4"

###IMPUTED DATA m=5
for (i in tablevals) {
  natmice <- setNames(aggregate(mice_imputed_data5[[i]], by=list(Category=mice_imputed_data5$year), FUN=sum)[2],i)
  assign(i,natmice,envir = .GlobalEnv)
}
mice_imputed_data5.nat <- do.call(cbind,mget(tablevals)) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

year                  <- c('2018','2019','2020')
mice_imputed_data5.nat <- cbind(mice_imputed_data5.nat,year)

# add labels
var.labels = c(year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crime violator population"
)
label(mice_imputed_data5.nat) = as.list(var.labels[match(names(mice_imputed_data5.nat), names(var.labels))])

#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (i in tablevals) {
  #j = 1,2,3 - 1=2018, 2=2019, 3=2020
  for (j in 1:3) { 
    test<-data.frame(CI95=poisson.test(mice_imputed_data5.nat[j,i], conf.level = 0.95)$conf.int[1:2])
    colnames(test)[colnames(test)=="CI95"] <- paste0(i,"5.",j)
    assign(paste0(i,"5.",j),test,envir = .GlobalEnv)
  }
}
mice_imputed_data5.natCI <- do.call(cbind,mget(c(paste0(tablevals,"5.1"),paste0(tablevals,"5.2"),paste0(tablevals,"5.3")))) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#rename columns to indicate which imputed data frame (m=5)
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "overall_admissions"]                  <- "overall_admissions5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "admissions_for_violations"]           <- "admissions_for_violations5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "admissions_for_technical_violations"] <- "admissions_for_technical_violations5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "admissions_for_new_crime_violations"] <- "admissions_for_new_crime_violations5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "overall_population"]                  <- "overall_population5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "violator_population"]                 <- "violator_population5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "technical_violator_population"]       <- "technical_violator_population5"
names(mice_imputed_data5.nat)[names(mice_imputed_data5.nat) == 
                                "new_crime_violator_population"]       <- "new_crime_violator_population5"

##########################
#CALCULATE FINAL ESTIMATES, m=1,2,3,4,5 (i.e., calculate average)
#sum of theta estimates divided by total number of imputations
final.parest.imp2018 <- data.frame(
  overall_admissions                  = (mice_imputed_data1.nat[1,1] + mice_imputed_data2.nat[1,1] + mice_imputed_data3.nat[1,1] + mice_imputed_data4.nat[1,1] + mice_imputed_data5.nat[1,1])/5,
  admissions_for_violations           = (mice_imputed_data1.nat[1,2] + mice_imputed_data2.nat[1,2] + mice_imputed_data3.nat[1,2] + mice_imputed_data4.nat[1,2] + mice_imputed_data5.nat[1,2])/5,
  admissions_for_technical_violations = (mice_imputed_data1.nat[1,3] + mice_imputed_data2.nat[1,3] + mice_imputed_data3.nat[1,3] + mice_imputed_data4.nat[1,3] + mice_imputed_data5.nat[1,3])/5,
  admissions_for_new_crime_violations = (mice_imputed_data1.nat[1,4] + mice_imputed_data2.nat[1,4] + mice_imputed_data3.nat[1,4] + mice_imputed_data4.nat[1,4] + mice_imputed_data5.nat[1,4])/5,
  overall_population                  = (mice_imputed_data1.nat[1,5] + mice_imputed_data2.nat[1,5] + mice_imputed_data3.nat[1,5] + mice_imputed_data4.nat[1,5] + mice_imputed_data5.nat[1,5])/5,
  violator_population                 = (mice_imputed_data1.nat[1,6] + mice_imputed_data2.nat[1,6] + mice_imputed_data3.nat[1,6] + mice_imputed_data4.nat[1,6] + mice_imputed_data5.nat[1,6])/5,
  technical_violator_population       = (mice_imputed_data1.nat[1,7] + mice_imputed_data2.nat[1,7] + mice_imputed_data3.nat[1,7] + mice_imputed_data4.nat[1,7] + mice_imputed_data5.nat[1,7])/5,
  new_crime_violator_population       = (mice_imputed_data1.nat[1,8] + mice_imputed_data2.nat[1,8] + mice_imputed_data3.nat[1,8] + mice_imputed_data4.nat[1,8] + mice_imputed_data5.nat[1,8])/5
  )
final.parest.imp2019 <- data.frame(
  overall_admissions                  = (mice_imputed_data1.nat[2,1] + mice_imputed_data2.nat[2,1] + mice_imputed_data3.nat[2,1] + mice_imputed_data4.nat[2,1] + mice_imputed_data5.nat[2,1])/5,
  admissions_for_violations           = (mice_imputed_data1.nat[2,2] + mice_imputed_data2.nat[2,2] + mice_imputed_data3.nat[2,2] + mice_imputed_data4.nat[2,2] + mice_imputed_data5.nat[2,2])/5,
  admissions_for_technical_violations = (mice_imputed_data1.nat[2,3] + mice_imputed_data2.nat[2,3] + mice_imputed_data3.nat[2,3] + mice_imputed_data4.nat[2,3] + mice_imputed_data5.nat[2,3])/5,
  admissions_for_new_crime_violations = (mice_imputed_data1.nat[2,4] + mice_imputed_data2.nat[2,4] + mice_imputed_data3.nat[2,4] + mice_imputed_data4.nat[2,4] + mice_imputed_data5.nat[2,4])/5,
  overall_population                  = (mice_imputed_data1.nat[2,5] + mice_imputed_data2.nat[2,5] + mice_imputed_data3.nat[2,5] + mice_imputed_data4.nat[2,5] + mice_imputed_data5.nat[2,5])/5,
  violator_population                 = (mice_imputed_data1.nat[2,6] + mice_imputed_data2.nat[2,6] + mice_imputed_data3.nat[2,6] + mice_imputed_data4.nat[2,6] + mice_imputed_data5.nat[2,6])/5,
  technical_violator_population       = (mice_imputed_data1.nat[2,7] + mice_imputed_data2.nat[2,7] + mice_imputed_data3.nat[2,7] + mice_imputed_data4.nat[2,7] + mice_imputed_data5.nat[2,7])/5,
  new_crime_violator_population       = (mice_imputed_data1.nat[2,8] + mice_imputed_data2.nat[2,8] + mice_imputed_data3.nat[2,8] + mice_imputed_data4.nat[2,8] + mice_imputed_data5.nat[2,8])/5
)
final.parest.imp2020 <- data.frame(
  overall_admissions                  = (mice_imputed_data1.nat[3,1] + mice_imputed_data2.nat[3,1] + mice_imputed_data3.nat[3,1] + mice_imputed_data4.nat[3,1] + mice_imputed_data5.nat[3,1])/5,
  admissions_for_violations           = (mice_imputed_data1.nat[3,2] + mice_imputed_data2.nat[3,2] + mice_imputed_data3.nat[3,2] + mice_imputed_data4.nat[3,2] + mice_imputed_data5.nat[3,2])/5,
  admissions_for_technical_violations = (mice_imputed_data1.nat[3,3] + mice_imputed_data2.nat[3,3] + mice_imputed_data3.nat[3,3] + mice_imputed_data4.nat[3,3] + mice_imputed_data5.nat[3,3])/5,
  admissions_for_new_crime_violations = (mice_imputed_data1.nat[3,4] + mice_imputed_data2.nat[3,4] + mice_imputed_data3.nat[3,4] + mice_imputed_data4.nat[3,4] + mice_imputed_data5.nat[3,4])/5,
  overall_population                  = (mice_imputed_data1.nat[3,5] + mice_imputed_data2.nat[3,5] + mice_imputed_data3.nat[3,5] + mice_imputed_data4.nat[3,5] + mice_imputed_data5.nat[3,5])/5,
  violator_population                 = (mice_imputed_data1.nat[3,6] + mice_imputed_data2.nat[3,6] + mice_imputed_data3.nat[3,6] + mice_imputed_data4.nat[3,6] + mice_imputed_data5.nat[3,6])/5,
  technical_violator_population       = (mice_imputed_data1.nat[3,7] + mice_imputed_data2.nat[3,7] + mice_imputed_data3.nat[3,7] + mice_imputed_data4.nat[3,7] + mice_imputed_data5.nat[3,7])/5,
  new_crime_violator_population       = (mice_imputed_data1.nat[3,8] + mice_imputed_data2.nat[3,8] + mice_imputed_data3.nat[3,8] + mice_imputed_data4.nat[3,8] + mice_imputed_data5.nat[3,8])/5
)
#rbind final estimates
final.parest.imp <- rbind(final.parest.imp2018,final.parest.imp2019,final.parest.imp2020)
final.parest.imp$year <- c("2018","2019","2020")

##########################
#CALCULATE FINAL CONFIDENCE INTERVALS, m=1,2,3,4,5 (i.e., calculate TOTAL variance)
# equation: w + (1+1/m)*b, where w=within-imputation variance, b=between-imputation variance, m=number of imputations
# w = sum of variance estimates divided by total number of imputations
# b = sum of the squared difference of the combined estimate (theta hat) from each theta estimate divided by the total number of imputations minus 1

#LOOP OVER ALL 8 RESPONSES: 1 through 8 = 'tablevals' vector
for (i in 1:8) {
##2018 (1)
#WITHIN-IMPUTATION VARIANCE
#POISSON DISTRIBUTION - mean of POISSON is also the variance
v1 <- mice_imputed_data1.nat[1,i]
v2 <- mice_imputed_data2.nat[1,i]
v3 <- mice_imputed_data3.nat[1,i]
v4 <- mice_imputed_data4.nat[1,i]
v5 <- mice_imputed_data5.nat[1,i]
w  <- sum(v1,v2,v3,v4,v5)/5
#BETWEEN-IMPUTATION VARIANCE
b  <- (((mice_imputed_data1.nat[1,i] - final.parest.imp[1,i])^2) + ((mice_imputed_data2.nat[1,i] - final.parest.imp[1,i])^2) + ((mice_imputed_data3.nat[1,i] - final.parest.imp[1,i])^2) + ((mice_imputed_data4.nat[1,i] - final.parest.imp[1,i])^2) + ((mice_imputed_data5.nat[1,i] - final.parest.imp[1,i])^2))/4
#TOTAL VARIANCE, then calculate standard error
s  <- sqrt(w + (1 + (1/5))*b)
#final CI
natCI18 <- c(final.parest.imp[1,i] - (s*1.96), final.parest.imp[1,i] + (s*1.96))
assign(paste0("natCI.2018.",i),natCI18,envir = .GlobalEnv)

##2019 (2)
#WITHIN-IMPUTATION VARIANCE
#POISSON DISTRIBUTION - mean of POISSON is also the variance
v1 <- mice_imputed_data1.nat[2,i]
v2 <- mice_imputed_data2.nat[2,i]
v3 <- mice_imputed_data3.nat[2,i]
v4 <- mice_imputed_data4.nat[2,i]
v5 <- mice_imputed_data5.nat[2,i]
w  <- sum(v1,v2,v3,v4,v5)/5
#BETWEEN-IMPUTATION VARIANCE
b  <- (((mice_imputed_data1.nat[2,i] - final.parest.imp[2,i])^2) + ((mice_imputed_data2.nat[2,i] - final.parest.imp[2,i])^2) + ((mice_imputed_data3.nat[2,i] - final.parest.imp[2,i])^2) + ((mice_imputed_data4.nat[2,i] - final.parest.imp[2,i])^2) + ((mice_imputed_data5.nat[2,i] - final.parest.imp[2,i])^2))/4
#TOTAL VARIANCE (calculate standard error)
s  <- sqrt(w + (1 + (1/5))*b)
#final CI
natCI19 <- c(final.parest.imp[2,i] - (s*1.96), final.parest.imp[2,i] + (s*1.96))
assign(paste0("natCI.2019.",i),natCI19,envir = .GlobalEnv)

##2020 (3)
#WITHIN-IMPUTATION VARIANCE
#POISSON DISTRIBUTION - mean of POISSON is also the variance
v1 <- mice_imputed_data1.nat[3,i]
v2 <- mice_imputed_data2.nat[3,i]
v3 <- mice_imputed_data3.nat[3,i]
v4 <- mice_imputed_data4.nat[3,i]
v5 <- mice_imputed_data5.nat[3,i]
w  <- sum(v1,v2,v3,v4,v5)/5
#BETWEEN-IMPUTATION VARIANCE
b  <- (((mice_imputed_data1.nat[3,i] - final.parest.imp[3,i])^2) + ((mice_imputed_data2.nat[3,i] - final.parest.imp[3,i])^2) + ((mice_imputed_data3.nat[3,i] - final.parest.imp[3,i])^2) + ((mice_imputed_data4.nat[3,i] - final.parest.imp[3,i])^2) + ((mice_imputed_data5.nat[3,i] - final.parest.imp[3,i])^2))/4
#TOTAL VARIANCE (calculate standard error)
s  <- sqrt(w + (1 + (1/5))*b)
#final CI
natCI20 <- c(final.parest.imp[3,i] - (s*1.96), final.parest.imp[3,i] + (s*1.96))
assign(paste0("natCI.2020.",i),natCI20,envir = .GlobalEnv)
}

#append confidence intervals for ease of output
natCI.2018all <- rbind(natCI.2018.1,natCI.2018.2,natCI.2018.3,natCI.2018.4,
                       natCI.2018.5,natCI.2018.6,natCI.2018.7,natCI.2018.8)
natCI.2019all <- rbind(natCI.2019.1,natCI.2019.2,natCI.2019.3,natCI.2019.4,
                       natCI.2019.5,natCI.2019.6,natCI.2019.7,natCI.2019.8)
natCI.2020all <- rbind(natCI.2020.1,natCI.2020.2,natCI.2020.3,natCI.2020.4,
                       natCI.2020.5,natCI.2020.6,natCI.2020.7,natCI.2020.8)
rownames(natCI.2018all) <- c(1,2,3,4,5,6,7,8)
rownames(natCI.2019all) <- c(1,2,3,4,5,6,7,8)
rownames(natCI.2020all) <- c(1,2,3,4,5,6,7,8)

#manipulate estimates for merging with CIs
nat.2018all <- as.data.frame(t(as.matrix(final.parest.imp[which(year==2018),]))) %>%
  slice(1:8)
rownames(nat.2018all) <- c(1,2,3,4,5,6,7,8)
colnames(nat.2018all) <- "Estimate"

nat.2019all <- as.data.frame(t(as.matrix(final.parest.imp[which(year==2019),]))) %>%
  slice(1:8)
rownames(nat.2019all) <- c(1,2,3,4,5,6,7,8)
colnames(nat.2019all) <- "Estimate"

nat.2020all <- as.data.frame(t(as.matrix(final.parest.imp[which(year==2020),]))) %>%
  slice(1:8)
rownames(nat.2020all) <- c(1,2,3,4,5,6,7,8)
colnames(nat.2020all) <- "Estimate"

#combine & print
cbind(nat.2018all,natCI.2018all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2019all,natCI.2019all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2020all,natCI.2020all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#############################################################################
###############CALCULATING STATE ESTIMATES
#
#
#############################################################################

###########
#######2019
#CHANGE REFERENCE: 2019
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 2) #2019

#response columns, run model to produce estimates/CIs
fit.overall_admissions                  <- with(temp_data,lm(overall_admissions~year + states + year:states))
fit.admissions_for_violations           <- with(temp_data,lm(admissions_for_violations~year + states + year:states))
fit.admissions_for_technical_violations <- with(temp_data,lm(admissions_for_technical_violations~year + states + year:states))
fit.admissions_for_new_crime_violations <- with(temp_data,lm(admissions_for_new_crime_violations~year + states + year:states))
fit.overall_population                  <- with(temp_data,lm(overall_population~year + states + year:states))
fit.violator_population                 <- with(temp_data,lm(violator_population~year + states + year:states))
fit.technical_violator_population       <- with(temp_data,lm(technical_violator_population~year + states + year:states))
fit.new_crime_violator_population       <- with(temp_data,lm(new_crime_violator_population~year + states + year:states))
  #1
  CI  <- summary(pool(fit.overall_admissions),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  overall_admissions <- CIt 
  #2
  CI  <- summary(pool(fit.admissions_for_violations),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  admissions_for_violations <- CIt 
  #3
  CI  <- summary(pool(fit.admissions_for_technical_violations),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  admissions_for_technical_violations <- CIt 
  #4
  CI  <- summary(pool(fit.admissions_for_new_crime_violations),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  admissions_for_new_crime_violations <- CIt 
  #5
  CI  <- summary(pool(fit.overall_population),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  overall_population <- CIt 
  #6
  CI  <- summary(pool(fit.violator_population),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  violator_population <- CIt 
  #7
  CI  <- summary(pool(fit.technical_violator_population),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  technical_violator_population <- CIt 
  #8
  CI  <- summary(pool(fit.new_crime_violator_population),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- admissions19$States
  new_crime_violator_population <- CIt 

#rbind and finalize formatting
national.est.CI2019            <- do.call(rbind,mget(tablevals))
national.est.CI2019n           <- as.data.frame(sapply(national.est.CI2019, as.numeric))
rownames(national.est.CI2019n) <- rownames(national.est.CI2019)
national.est.CI2019n           <- national.est.CI2019n[,1:50] 
national.est.CI2019n[,51]        <- national.est.CI2019n[,1]
for (i in 2:50){
  national.est.CI2019n[,50+i]    <- national.est.CI2019n[,1] + national.est.CI2019n[,i]
}
national.est.CI2019n           <- national.est.CI2019n[,51:100]
colnames(national.est.CI2019n) <- admissions19$States

national.est.CI2019t           <- transpose(national.est.CI2019n)
colnames(national.est.CI2019t) <- paste0(rownames(national.est.CI2019n),'2019')
rownames(national.est.CI2019t) <- colnames(national.est.CI2019n)

###########
#######2020
#CHANGE REFERENCE: 2020
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 3) #2020

#response columns, run model to produce estimates/CIs
fit.overall_admissions                  <- with(temp_data,lm(overall_admissions~year + states + year:states))
fit.admissions_for_violations           <- with(temp_data,lm(admissions_for_violations~year + states + year:states))
fit.admissions_for_technical_violations <- with(temp_data,lm(admissions_for_technical_violations~year + states + year:states))
fit.admissions_for_new_crime_violations <- with(temp_data,lm(admissions_for_new_crime_violations~year + states + year:states))
fit.overall_population                  <- with(temp_data,lm(overall_population~year + states + year:states))
fit.violator_population                 <- with(temp_data,lm(violator_population~year + states + year:states))
fit.technical_violator_population       <- with(temp_data,lm(technical_violator_population~year + states + year:states))
fit.new_crime_violator_population       <- with(temp_data,lm(new_crime_violator_population~year + states + year:states))
#1
CI  <- summary(pool(fit.overall_admissions),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
overall_admissions <- CIt 
#2
CI  <- summary(pool(fit.admissions_for_violations),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
admissions_for_violations <- CIt 
#3
CI  <- summary(pool(fit.admissions_for_technical_violations),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
admissions_for_technical_violations <- CIt 
#4
CI  <- summary(pool(fit.admissions_for_new_crime_violations),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
admissions_for_new_crime_violations <- CIt 
#5
CI  <- summary(pool(fit.overall_population),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
overall_population <- CIt 
#6
CI  <- summary(pool(fit.violator_population),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
violator_population <- CIt 
#7
CI  <- summary(pool(fit.technical_violator_population),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
technical_violator_population <- CIt 
#8
CI  <- summary(pool(fit.new_crime_violator_population),conf.int=TRUE) %>%
  select(term,estimate)
CIt <- as.data.frame(t(as.matrix(CI)))
rownames(CIt) <- colnames(CI)
colnames(CIt) <- CI[,1]
CIt           <- subset (CIt, select = -c(year2019,year2018))
CIt           <- CIt[-c(1),]
colnames(CIt) <- admissions19$States
new_crime_violator_population <- CIt 

#rbind and finalize formatting
national.est.CI2020            <- do.call(rbind,mget(tablevals))
national.est.CI2020n           <- as.data.frame(sapply(national.est.CI2020, as.numeric))
rownames(national.est.CI2020n) <- rownames(national.est.CI2020)
national.est.CI2020n           <- national.est.CI2020n[,1:50] 
national.est.CI2020n[,51]      <- national.est.CI2020n[,1]
for (i in 2:50){
  national.est.CI2020n[,50+i]  <- national.est.CI2020n[,1] + national.est.CI2020n[,i]
}
national.est.CI2020n           <- national.est.CI2020n[,51:100]
colnames(national.est.CI2020n) <- admissions19$States

national.est.CI2020t           <- transpose(national.est.CI2020n)
colnames(national.est.CI2020t) <- paste0(rownames(national.est.CI2020n),'2020')
rownames(national.est.CI2020t) <- colnames(national.est.CI2020n)

#cbind each data frame: 2019, 2020
national.est1920                                   <- cbind(national.est.CI2019t,national.est.CI2020t)
national.est1920$new_crime_violator_population2019 <- as.numeric(format(round(national.est1920$new_crime_violator_population2019,2), nsmall=0,scientific = F, digits = 3))

#just for state-level cost report
forreport.1920 <- national.est1920[,c("violator_population2019","violator_population2020")]

################################################################################
#COSTS
#
#Take state-level estimates and calculate costs
################################################################################

cost.final <- costs %>%
  mutate(
    Cost.in.2020 = case_when(is.na(Cost.in.2020) ~ Cost.in.2019,
                             TRUE ~ Cost.in.2020)
  ) %>% 
  bind_cols(forreport.1920) %>%
  mutate(
    averted_costs = (violator_population2019*Cost.in.2020*365) - (violator_population2020*Cost.in.2020*365) 
  )

#averted cost total
sum(cost.final$averted_costs)
