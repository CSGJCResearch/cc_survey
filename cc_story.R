#######################################
# Confined and Costly
# Impute values, national estimates for comms team
# by Mari Roberts, Joshua Mallett
# 7/13/2021
# updated: 7/14/2021 - finalize MI counts w/CI's
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
source("automated_clean.R")

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
# CHECK MISSINGNESS
################################################################################

# # rename variables to reflect the year and combine data
# miss_2018 <- adm_pop_analysis %>% filter(year == 2018) %>% select(state = states, admissions_18 = total_admissions, population_18 = total_population)
# miss_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% select(state = states, admissions_19 = total_admissions, population_19 = total_population)
# miss_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% select(state = states, admissions_20 = total_admissions, population_20 = total_population)
# miss_data <- merge(miss_2018, miss_2019, by = c("state"))
# miss_data <- merge(miss_data, miss_2020, by = c("state"))
# 
# # factor state
# miss_data$state <- factor(miss_data$state)
# 
# # examine missing data with ff_glimpse
# explanatory = c("admissions_18","admissions_19", "admissions_20",
#                 "population_18", "population_19")
# dependent =  "population_20"
# miss_data %>%
#   ff_glimpse(dependent, explanatory)
# 
# # visualize missing data and relationships
# miss_data %>%
#   missing_plot()
# 
# # pattern of missingness between variables
# miss_data %>%
#   missing_pattern(dependent, explanatory)
# 
# # 2019 population is missing the most (7)
# aggr(miss_data, prop=FALSE, numbers=TRUE)
# 
# # pairs plots to show relationships between missing values and observed values in all variables
# miss_data %>%
#   missing_pairs(dependent, explanatory)
# 
# # remove state variable
# miss_data1 <- miss_data %>% select(-state)
# 
# # using correlations to explore missing values
# # extracting variables that have missing values
# # see which variables tend to be missing together
# x <- as_data_frame(abs(is.na(miss_data)))
# 
# # elements of x are 1 if a value in the data is missing and 0 if non-missing
# head(miss_data)
# head(x)
# 
# # extracting variables that have some missing values
# y <- x[which(sapply(x, sd) > 0)]
# 
# # correlations among indicator variables
# cor(y)
# 
# # look at the relationship between the presence of missing values in each variable and the observed values in other variables
# # rows are observed variables, and the columns are indicator variables representing missingness
# # ignore the warning message and NA values in the correlation matrix
# cor(miss_data1, y, use="pairwise.complete.obs")

#####
# MCAR test
#####

# MCAR test
# not missing completely at random
# TestMCARNormality(data = miss_data1,del.lesscases = 1)

################################################################################
# IMPUTATION
# MICE

# National Estimate Variables
################################################################################

#New National Variables/columns
m.imp <- adm_pop_analysis %>% mutate(
  overall_admissions                  = total_admissions,
  admissions_for_violations           = total_violation_admissions,
  admissions_for_technical_violations = technical_probation_violation_admissions + technical_parole_violation_admissions,
  admissions_for_new_crime_violations = new_offense_probation_violation_admissions + new_offense_parole_violation_admissions,
  overall_population                  = total_population,
  violator_population                 = total_violation_population,
  technical_violator_population       = technical_probation_violation_population + technical_parole_violation_population,
  new_crime_violator_population       = new_offense_probation_violation_population + new_offense_parole_violation_population
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

#New National Variables/columns
m.imp <- adm_pop_analysis %>% mutate(
  overall_admissions                  = total_admissions,
  admissions_for_violations           = total_violation_admissions,
  admissions_for_technical_violations = technical_probation_violation_admissions + technical_parole_violation_admissions,
  admissions_for_new_crime_violations = new_offense_probation_violation_admissions + new_offense_parole_violation_admissions,
  overall_population                  = total_population,
  violator_population                 = total_violation_population,
  technical_violator_population       = technical_probation_violation_population + technical_parole_violation_population,
  new_crime_violator_population       = new_offense_probation_violation_population + new_offense_parole_violation_population
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

# completed dataset (list data of all imputed data frames)
#mice_imputed_data <- complete(temp_data,"all")
mice_imputed_data <- complete(temp_data,1) #use this for modeling code at the state level

#each individual imputed data frame (m=5)
mice_imputed_data1 <- complete(temp_data,1)
mice_imputed_data2 <- complete(temp_data,2)
mice_imputed_data3 <- complete(temp_data,3)
mice_imputed_data4 <- complete(temp_data,4)
mice_imputed_data5 <- complete(temp_data,5)

# test1 <- as.list(mice_imputed_data.nat)
# test2 <- as.list(mice_imputed_data.nat)
# test3 <- as.list(mice_imputed_data.nat)
# test4 <- as.list(mice_imputed_data.nat)
# test5 <- as.list(mice_imputed_data.nat)

# full.list <- list(test1,test2,test3,test4,test5)
# 
# fit <- with(full.list,glm(unlist(overall_admissions)~1))
# CI  <- summary(pool(fit),conf.int=TRUE)

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
#CALCULATE FINAL ESTIMATES, m=5 (i.e., calculate average)
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

#USE mice_imputed_data.nat for National Totals
#USE mice_imputed_data.natCI for National 95% CIs
#2018
# c(mice_imputed_data.nat[1,1],mice_imputed_data.natCI[1,1],mice_imputed_data.natCI[2,1])
# c(mice_imputed_data.nat[1,2],mice_imputed_data.natCI[1,2],mice_imputed_data.natCI[2,2])
# c(mice_imputed_data.nat[1,3],mice_imputed_data.natCI[1,3],mice_imputed_data.natCI[2,3])
# c(mice_imputed_data.nat[1,4],mice_imputed_data.natCI[1,4],mice_imputed_data.natCI[2,4])
# c(mice_imputed_data.nat[1,5],mice_imputed_data.natCI[1,5],mice_imputed_data.natCI[2,5])
# c(mice_imputed_data.nat[1,6],mice_imputed_data.natCI[1,6],mice_imputed_data.natCI[2,6])
# c(mice_imputed_data.nat[1,7],mice_imputed_data.natCI[1,7],mice_imputed_data.natCI[2,7])
# c(mice_imputed_data.nat[1,8],mice_imputed_data.natCI[1,8],mice_imputed_data.natCI[2,8])
# 
# #2019
# c(mice_imputed_data.nat[2,1],mice_imputed_data.natCI[1,9], mice_imputed_data.natCI[2,9])
# c(mice_imputed_data.nat[2,2],mice_imputed_data.natCI[1,10],mice_imputed_data.natCI[2,10])
# c(mice_imputed_data.nat[2,3],mice_imputed_data.natCI[1,11],mice_imputed_data.natCI[2,11])
# c(mice_imputed_data.nat[2,4],mice_imputed_data.natCI[1,12],mice_imputed_data.natCI[2,12])
# c(mice_imputed_data.nat[2,5],mice_imputed_data.natCI[1,13],mice_imputed_data.natCI[2,13])
# c(mice_imputed_data.nat[2,6],mice_imputed_data.natCI[1,14],mice_imputed_data.natCI[2,14])
# c(mice_imputed_data.nat[2,7],mice_imputed_data.natCI[1,15],mice_imputed_data.natCI[2,15])
# c(mice_imputed_data.nat[2,8],mice_imputed_data.natCI[1,16],mice_imputed_data.natCI[2,16])
# 
# #2020
# c(mice_imputed_data.nat[3,1],mice_imputed_data.natCI[1,17],mice_imputed_data.natCI[2,17])
# c(mice_imputed_data.nat[3,2],mice_imputed_data.natCI[1,18],mice_imputed_data.natCI[2,18])
# c(mice_imputed_data.nat[3,3],mice_imputed_data.natCI[1,19],mice_imputed_data.natCI[2,19])
# c(mice_imputed_data.nat[3,4],mice_imputed_data.natCI[1,20],mice_imputed_data.natCI[2,20])
# c(mice_imputed_data.nat[3,5],mice_imputed_data.natCI[1,21],mice_imputed_data.natCI[2,21])
# c(mice_imputed_data.nat[3,6],mice_imputed_data.natCI[1,22],mice_imputed_data.natCI[2,22])
# c(mice_imputed_data.nat[3,7],mice_imputed_data.natCI[1,23],mice_imputed_data.natCI[2,23])
# c(mice_imputed_data.nat[3,8],mice_imputed_data.natCI[1,24],mice_imputed_data.natCI[2,24])

########ABOVE code is for calculating national *TOTALS*

##########################################
############### BELOW CODE IS FOR CALCULATING NATIONAL *AVERAGES*
#SET REFERENCE: 2018
# mice_imputed_data.nat$year <- relevel(as.factor(mice_imputed_data.nat$year), ref = 1) #2018
# 
# #loop through response columns, run model to produce estimates/CIs
# for (i in tablevals) {
#   fit <- with(temp_data,lm(mice_imputed_data[[i]]~year))
#   CI  <- summary(pool(fit),conf.int=TRUE) %>% mutate(
#     term = ifelse(term=="(Intercept)","year2018",
#                   ifelse(term=="year2019","year2019",
#                          ifelse(term=="year2020","year2020",NA)))
#     ) %>%
#     select(term,estimate,`2.5 %`,`97.5 %`)
#     CIt <- as.data.frame(t(as.matrix(CI)))
#     rownames(CIt) <- colnames(CI)
#     colnames(CIt) <- CI[,1]
#     CIt <- subset (CIt, select = -c(year2020,year2019))
#   assign(i,CIt,envir = .GlobalEnv)
# }
# 
# #rbind and finalize formatting
# national.est.CI2018 <- do.call(rbind,mget(tablevals)) %>% #append all data frames for table
#   filter(year2018!="year2018") %>% mutate(
#     across(where(is.character), as.numeric)
#   ) %>%
#   mutate(
#     across(where(is.numeric),round,1)
#   ) %>%
#   mutate(
#     across(where(is.numeric),formattable::comma,1)
#   )
# 

#CHANGE REFERENCE: 2019
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 2) #2019

#loop through response columns, run model to produce estimates/CIs
for (i in tablevals) {
  fit <- with(temp_data,lm(mice_imputed_data[[i]]~year + states + year:states))
  CI  <- summary(pool(fit),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2020,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- adm19$States
  assign(i,CIt,envir = .GlobalEnv)
}

#rbind and finalize formatting
national.est.CI2019            <- do.call(rbind,mget(tablevals))
national.est.CI2019n           <- as.data.frame(sapply(national.est.CI2019, as.numeric))
rownames(national.est.CI2019n) <- rownames(national.est.CI2019)
national.est.CI2019n <- national.est.CI2019n[,1:50] 
for (i in 2:51){
  national.est.CI2019n[,49+i]    <- national.est.CI2019n[,1] + national.est.CI2019n[,i]
}
national.est.CI2019n           <- national.est.CI2019n[,51:100]
colnames(national.est.CI2019n) <- adm19$States

national.est.CI2019t           <- transpose(national.est.CI2019n)
colnames(national.est.CI2019t) <- paste0(rownames(national.est.CI2019n),'2019')
rownames(national.est.CI2019t) <- colnames(national.est.CI2019n)

#CHANGE REFERENCE: 2020
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 3) #2020

#loop through response columns, run model to produce estimates/CIs
for (i in tablevals) {
  fit <- with(temp_data,lm(mice_imputed_data[[i]]~year + states + year:states))
  CI  <- summary(pool(fit),conf.int=TRUE) %>%
    select(term,estimate)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1]
  CIt           <- subset (CIt, select = -c(year2019,year2018))
  CIt           <- CIt[-c(1),]
  colnames(CIt) <- adm20$States
  assign(i,CIt,envir = .GlobalEnv)
}

#rbind and finalize formatting
national.est.CI2020            <- do.call(rbind,mget(tablevals))
national.est.CI2020n           <- as.data.frame(sapply(national.est.CI2020, as.numeric))
rownames(national.est.CI2020n) <- rownames(national.est.CI2020)
national.est.CI2020n <- national.est.CI2020n[,1:50] 
for (i in 2:51){
  national.est.CI2020n[,49+i]    <- national.est.CI2020n[,1] + national.est.CI2020n[,i]
}
national.est.CI2020n           <- national.est.CI2020n[,51:100]
colnames(national.est.CI2020n) <- adm20$States

national.est.CI2020t           <- transpose(national.est.CI2020n)
colnames(national.est.CI2020t) <- paste0(rownames(national.est.CI2020n),'2020')
rownames(national.est.CI2020t) <- colnames(national.est.CI2020n)

#cbind each data frame: 2019, 2020
national.est1920                                   <- cbind(national.est.CI2019t,national.est.CI2020t)
national.est1920$new_crime_violator_population2019 <- as.numeric(format(round(national.est1920$new_crime_violator_population2019,2), nsmall=0,scientific = F, digits = 3))

#just for state-level cost report
forreport.1920 <- national.est1920[,c("violator_population2019","violator_population2020")]
write.xlsx(forreport.1920, "shared_data/total_violators_2019_2020.xlsx")

#print for pasting in table
#2018
national.est.CI$year2018[1:3]
national.est.CI$year2018[4:6]
national.est.CI$year2018[7:9]
national.est.CI$year2018[10:12]
national.est.CI$year2018[13:15]
national.est.CI$year2018[16:18]
national.est.CI$year2018[19:21]
national.est.CI$year2018[22:24]
#2019
national.est.CI$year2019[1:3]
national.est.CI$year2019[4:6]
national.est.CI$year2019[7:9]
national.est.CI$year2019[10:12]
national.est.CI$year2019[13:15]
national.est.CI$year2019[16:18]
national.est.CI$year2019[19:21]
national.est.CI$year2019[22:24]
#2020
national.est.CI$year2020[1:3]
national.est.CI$year2020[4:6]
national.est.CI$year2020[7:9]
national.est.CI$year2020[10:12]
national.est.CI$year2020[13:15]
national.est.CI$year2020[16:18]
national.est.CI$year2020[19:21]
national.est.CI$year2020[22:24]

# inspect the distribution of original and imputed data
# want to see magenta points (imputed) match the shape of the blue ones (observed)
# plausible values
# xyplot(temp_data,total_admissions ~ total_population,pch=18,cex=1)
# densityplot(temp_data)

################################################################################
# IMPUTATION
# MEAN
# NOTE - NOT USING THIS - USING MULTIPLE IMPUTATION ABOVE INSTEAD
################################################################################

# Create mean if value is NA
# mean_imputed_data <-  adm_pop_analysis %>% 
#   group_by(year) %>% 
#   mutate(total_admissions_mean = ifelse(is.na(total_admissions), mean(total_admissions, na.rm=T), total_admissions),
#          total_violation_admissions_mean = ifelse(is.na(total_violation_admissions), mean(total_violation_admissions, na.rm=T), total_violation_admissions),      
#          total_probation_violation_admissions_mean = ifelse(is.na(total_probation_violation_admissions), mean(total_probation_violation_admissions, na.rm=T), total_probation_violation_admissions),      
#          new_offense_probation_violation_admissions_mean = ifelse(is.na(new_offense_probation_violation_admissions), mean(new_offense_probation_violation_admissions, na.rm=T), new_offense_probation_violation_admissions),  
#          technical_probation_violation_admissions_mean = ifelse(is.na(technical_probation_violation_admissions), mean(technical_probation_violation_admissions, na.rm=T), technical_probation_violation_admissions),  
#          total_parole_violation_admissions_mean = ifelse(is.na(total_parole_violation_admissions), mean(total_parole_violation_admissions, na.rm=T), total_parole_violation_admissions),           
#          new_offense_parole_violation_admissions_mean = ifelse(is.na(new_offense_parole_violation_admissions), mean(new_offense_parole_violation_admissions, na.rm=T), new_offense_parole_violation_admissions),  
#          technical_parole_violation_admissions_mean = ifelse(is.na(technical_parole_violation_admissions), mean(technical_parole_violation_admissions, na.rm=T), technical_parole_violation_admissions),       
#          
#          total_population_mean = ifelse(is.na(total_population), mean(total_population, na.rm=T), total_population),  
#          total_violation_population_mean = ifelse(is.na(total_violation_population), mean(total_violation_population, na.rm=T), total_violation_population),                  
#          total_probation_violation_population_mean = ifelse(is.na(total_probation_violation_population), mean(total_probation_violation_population, na.rm=T), total_probation_violation_population),  
#          new_offense_probation_violation_population_mean = ifelse(is.na(new_offense_probation_violation_population), mean(new_offense_probation_violation_population, na.rm=T), new_offense_probation_violation_population),  
#          technical_probation_violation_population_mean = ifelse(is.na(technical_probation_violation_population), mean(technical_probation_violation_population, na.rm=T), technical_probation_violation_population),  
#          total_parole_violation_population_mean = ifelse(is.na(total_parole_violation_population), mean(total_parole_violation_population, na.rm=T), total_parole_violation_population),           
#          new_offense_parole_violation_population_mean = ifelse(is.na(new_offense_parole_violation_population), mean(new_offense_parole_violation_population, na.rm=T), new_offense_parole_violation_population),  
#          technical_parole_violation_population_mean = ifelse(is.na(technical_parole_violation_population), mean(technical_parole_violation_population, na.rm=T), technical_parole_violation_population))       
#       

################################################################################
# CALCULATE CHANGES / NATIONAL ESTIMATES
################################################################################

# calculate percent change         
adm_pop_analysis <- mice_imputed_data.nat %>% group_by(states) %>% 
  mutate(overall_admissions_pct = (overall_admissions / dplyr::lag(overall_admissions) -1)*100,
         overall_population_pct = (overall_population / dplyr::lag(overall_population) -1)*100,
         admissions_for_violations_pct = (admissions_for_violations / dplyr::lag(admissions_for_violations) -1)*100,
         violator_population_pct = (violator_population / dplyr::lag(violator_population) -1)*100
  )

# rearrange data
# adm_pop_analysis <- adm_pop_analysis %>% 
#   select(sort(names(.)))
# adm_pop_analysis <- adm_pop_analysis %>% select(states, year, everything())

# change data types
adm_pop_analysis$year <- factor(adm_pop_analysis$year)
adm_pop_analysis$overall_admissions <- as.numeric(adm_pop_analysis$overall_admissions)
adm_pop_analysis$admissions_for_violations  <- as.numeric(adm_pop_analysis$admissions_for_violations)
adm_pop_analysis$admissions_for_technical_violations  <- as.numeric(adm_pop_analysis$admissions_for_technical_violations)
adm_pop_analysis$admissions_for_new_crime_violations  <- as.numeric(adm_pop_analysis$admissions_for_new_crime_violations)
adm_pop_analysis$overall_population  <- as.numeric(adm_pop_analysis$overall_population)
adm_pop_analysis$violator_population  <- as.numeric(adm_pop_analysis$violator_population)
adm_pop_analysis$technical_violator_population  <- as.numeric(adm_pop_analysis$technical_violator_population)
adm_pop_analysis$new_crime_violator_population  <- as.numeric(adm_pop_analysis$new_crime_violator_population)

# sum of variables
adm_pop_national <- adm_pop_analysis %>% group_by(year) %>% dplyr::summarise(
  # admissions
  overall_admissions = sum(overall_admissions),
  admissions_for_violations = sum(admissions_for_violations),
  admissions_for_technical_violations = sum(admissions_for_technical_violations),
  admissions_for_new_crime_violations = sum(admissions_for_new_crime_violations),
  # population
  overall_population = sum(overall_population),
  violator_population = sum(violator_population),
  technical_violator_population = sum(technical_violator_population),
  new_crime_violator_population = sum(new_crime_violator_population)
)

# transpose data
national_estimates = t(adm_pop_national)
national_estimates <- data.frame(national_estimates)
national_estimates <- tibble::rownames_to_column(national_estimates, "type")
national_estimates <- national_estimates %>% select(type, year2018 = X1, year2019 = X2, year2020 = X3)
national_estimates <- national_estimates %>% filter(type != "year")

# change data types
national_estimates$year2018 <- as.numeric(national_estimates$year2018)
national_estimates$year2019 <- as.numeric(national_estimates$year2019)
national_estimates$year2020 <- as.numeric(national_estimates$year2020)

# create changes variables
national_estimates <- national_estimates %>% mutate(change_18_19 = year2019-year2018,
                                                    change_19_20 = year2020-year2019,
                                                    change_18_20 = year2020-year2018,
                                                    pct_18_19 = ((year2019-year2018)/year2018),
                                                    pct_19_20 = ((year2020-year2019)/year2019),
                                                    pct_18_20 = ((year2020-year2018)/year2018)
)
# add labels to data
var.labels = c(type = "Metric", 
               year2018 = "2018",
               year2019 = "2019",
               year2020 = "2020",
               change_18_19 = "Change from 2018 to 2019",
               change_19_20 = "Change from 2019 to 2020",
               change_18_20 = "Change from 2018 to 2020",
               pct_18_19 = "Percent Change from 2018 to 2019",    
               pct_19_20 = "Percent Change from 2019 to 2020",
               pct_18_20 = "Percent Change from 2018 to 2020")
label(national_estimates) = as.list(var.labels[match(names(national_estimates), names(var.labels))])

# save data to send to comms
write.xlsx(national_estimates, "shared_data/National estimates for web team 2021.xlsx")

################################################################################
# Costs
################################################################################

# get cost data for 2021
costs <- read_xlsx("data/Cost and notes data 2021.xlsx", .name_repair = "universal")

# make variables lowercase and replace periods with underscore
costs <- costs %>% janitor::clean_names()

# remove states that are NA
costs <- costs %>% filter(states != "NA")

# select general cost info
costs <- costs %>% select(states, cost)

# remove dollar sign
costs$cost_2020 = as.numeric(gsub("\\$", "", costs$cost))
costs <- costs %>% select(-cost)

# get cost data from 2019 - using costs from 2019 if cost data is missing for 2020
costs2019 <- read_excel("data/Cost Per Day For Calculation.xlsx")

# rename variables
costs2019 <- costs2019 %>% select(states = States, cost_2019 = `State Reported CostPerDay`)

# replace NAs in 2020 cost data with data from 2019
setDT(costs); setDT(costs2019)
costs[is.na(cost_2020), cost_2020 := costs2019[.SD, on=.(states), x.cost_2019]]
costs <- merge(costs, costs2019, by = "states")

# rename variables and filter by 2019
costs_pop_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% 
  select(states,
         overall_admissions_2019 = overall_admissions,
         admissions_for_violations_2019 = admissions_for_violations,
         admissions_for_technical_violations_2019 = admissions_for_technical_violations,
         admissions_for_new_crime_violations_2019 = admissions_for_new_crime_violations,
         overall_population_2019 = overall_population,                  
         violator_population_2019 = violator_population,                 
         technical_violator_population_2019 = technical_violator_population,      
         new_crime_violator_population_2019 = new_crime_violator_population)

# rename variables and filter by 2020
costs_pop_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% 
  select(states,
         overall_admissions_2020 = overall_admissions,
         admissions_for_violations_2020 = admissions_for_violations,
         admissions_for_technical_violations_2020 = admissions_for_technical_violations,
         admissions_for_new_crime_violations_2020 = admissions_for_new_crime_violations,
         overall_population_2020 = overall_population,                  
         violator_population_2020 = violator_population,                 
         technical_violator_population_2020 = technical_violator_population,      
         new_crime_violator_population_2020 = new_crime_violator_population)

# merge costs and population numbers
costs_pop_df <- merge(costs_pop_2020, costs, by = c("states"), all.x = TRUE, all.y = TRUE)
costs_pop_df <- merge(costs_pop_df, costs_pop_2019, by = c("states"), all.x = TRUE, all.y = TRUE)

# prevent rounding
options(digits=20)

# calculate changes by year
costs_final <- costs_pop_df %>% 
  mutate(overall_population_change_19_20 = overall_population_2020 - overall_population_2019,
         violator_population_change_19_20 = violator_population_2020 - violator_population_2019,
         technical_violator_population_change_19_20 = technical_violator_population_2020 - technical_violator_population_2019)

# calculate yearly cost by type
costs_final <- costs_final %>%
  mutate(amount_saved_overall_population_change_19_20 = overall_population_change_19_20*cost_2019*365,
         amount_saved_total_violator_population_change_19_20 = violator_population_change_19_20*cost_2019*365, 
         amount_saved_technical_violator_change_19_20 = technical_violator_population_change_19_20*cost_2019*365)

# reorder variables
costs_final <- costs_final %>% select(states,
                                      overall_population_2019,
                                      violator_population_2019,
                                      technical_violator_population_2019,
                                      cost_2019,
                                      overall_population_2020,
                                      violator_population_2020,
                                      technical_violator_population_2020,
                                      cost_2020,
                                      everything())

var.labels = c(states = "State name",
               overall_population_2019 = "Total population in 2019",
               violator_population_2019 = "Total violation population in 2019",
               technical_violator_population_2019 = "Technical violation population in 2019",
               cost_2019 = "State reported cost per day in 2019" ,
               overall_population_2020 = "Total population in 2020",
               violator_population_2020 = "Total violation population in 2020",
               technical_violator_population_2020 = "Technical violation population in 2020",
               cost_2020 = "State reported cost per day in 2020",
               overall_population_change_19_20 = "Change in total population between 2019 and 2020",
               violator_population_change_19_20 = "Change in total violation population between 2019 and 2020",
               technical_violator_population_change_19_20 = "Change in technical violation population between 2019 and 2020",         
               amount_saved_overall_population_change_19_20 = "Amount saved due to change in total population between 2019 and 2020",    
               amount_saved_total_violator_population_change_19_20 = "Amount saved due to change in total violation population between 2019 and 2020",
               amount_saved_technical_violator_change_19_20 = "Amount saved due to change in technical violation population between 2019 and 2020")

label(costs_final) = as.list(var.labels[match(names(costs_final), names(var.labels))])

# save as xlsx
write.xlsx(costs_final, "shared_data/Costs for web team 2021.xlsx")