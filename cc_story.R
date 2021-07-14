#######################################
# Confined and Costly survey
# Impute values, costs, national estimates data for comms
# by Mari Roberts, JM
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

######################################################################################################################################################
# CHECK MISSINGNESS
######################################################################################################################################################

# rename variables to reflect the year and combine data
miss_2018 <- adm_pop_analysis %>% filter(year == 2018) %>% select(state = states, admissions_18 = total_admissions, population_18 = total_population)
miss_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% select(state = states, admissions_19 = total_admissions, population_19 = total_population)
miss_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% select(state = states, admissions_20 = total_admissions, population_20 = total_population)
miss_data <- merge(miss_2018, miss_2019, by = c("state"))
miss_data <- merge(miss_data, miss_2020, by = c("state"))

# factor state
miss_data$state <- factor(miss_data$state)

# examine missing data with ff_glimpse
explanatory = c("admissions_18","admissions_19", "admissions_20",
                "population_18", "population_19")
dependent =  "population_20"
miss_data %>%
  ff_glimpse(dependent, explanatory)

# visualize missing data and relationships
miss_data %>%
  missing_plot()

# pattern of missingness between variables
miss_data %>%
  missing_pattern(dependent, explanatory)

# 2019 population is missing the most (7)
aggr(miss_data, prop=FALSE, numbers=TRUE)

# pairs plots to show relationships between missing values and observed values in all variables
miss_data %>%
  missing_pairs(dependent, explanatory)

# remove state variable
miss_data1 <- miss_data %>% select(-state)

# using correlations to explore missing values
# extracting variables that have missing values
# see which variables tend to be missing together
x <- as_data_frame(abs(is.na(miss_data)))

# elements of x are 1 if a value in the data is missing and 0 if non-missing
head(miss_data)
head(x)

# extracting variables that have some missing values
y <- x[which(sapply(x, sd) > 0)]

# correlations among indicator variables
cor(y)

# look at the relationship between the presence of missing values in each variable and the observed values in other variables
# rows are observed variables, and the columns are indicator variables representing missingness
# ignore the warning message and NA values in the correlation matrix
cor(miss_data1, y, use="pairwise.complete.obs")

#####
# MCAR test
#####

# MCAR test
# not missing completely at random
# TestMCARNormality(data = miss_data1,del.lesscases = 1)

######################################################################################################################################################
# IMPUTATION
# MICE
#National Estimate Variables
######################################################################################################################################################

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


# add labels
var.labels = c(states                              = "State name", 
               year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crim violator population"
               )
label(m.imp) = as.list(var.labels[match(names(m.imp), names(var.labels))])

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(m.imp,2,pMiss)

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method, predictive mean matching
temp_data <- mice(m.imp,m=5,maxit=50,meth='pmm',seed=500)
summary(temp_data)

# check imputed data
# each observation (first column left) within each imputed dataset (first row at the top)
temp_data$imp$overall_admissions

# completed dataset
mice_imputed_data <- complete(temp_data,1)

######
#produce confidence intervals
#post multiple imputation t-test
#grab response columns for creating estimated counts/CIs
tablevals<- colnames(mice_imputed_data[3:10])

#SET REFERENCE: 2018
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 1) #2018

#loop through response columns, run model to produce estimates/CIs
for (i in tablevals) {
  fit <- with(temp_data,lm(mice_imputed_data[[i]]~year))
  CI  <- summary(pool(fit),conf.int=TRUE) %>% mutate(
    term = ifelse(term=="(Intercept)","year2018",
                  ifelse(term=="year2019","year2019",
                         ifelse(term=="year2020","year2020",NA)))
    ) %>%
    select(term,estimate,`2.5 %`,`97.5 %`)
    CIt <- as.data.frame(t(as.matrix(CI)))
    rownames(CIt) <- colnames(CI)
    colnames(CIt) <- CI[,1] 
    CIt <- subset (CIt, select = -c(year2020,year2019))
  assign(i,CIt,envir = .GlobalEnv)
}

#rbind and finalize formatting
national.est.CI2018 <- do.call(rbind,mget(tablevals)) %>% #append all data frames for table
  filter(year2018!="year2018") %>% mutate(
    across(where(is.character), as.numeric)
  ) %>%
  mutate(
    across(where(is.numeric),round,1)
  ) %>%
  mutate(
    across(where(is.numeric),formattable::comma,1)
  )

#CHANGE REFERENCE: 2019
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 2) #2019

#loop through response columns, run model to produce estimates/CIs
for (i in tablevals) {
  fit <- with(temp_data,lm(mice_imputed_data[[i]]~year))
  CI  <- summary(pool(fit),conf.int=TRUE) %>% mutate(
    term = ifelse(term=="(Intercept)","year2019",
                  ifelse(term=="year2018","year2018",
                         ifelse(term=="year2020","year2020",NA)))
  ) %>%
    select(term,estimate,`2.5 %`,`97.5 %`)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1] 
  CIt <- subset (CIt, select = -c(year2018,year2020))
  assign(i,CIt,envir = .GlobalEnv)
}

#rbind and finalize formatting
national.est.CI2019 <- do.call(rbind,mget(tablevals)) %>% #append all data frames for table
  filter(year2019!="year2019") %>% mutate(
    across(where(is.character), as.numeric)
  ) %>%
  mutate(
    across(where(is.numeric),round,1)
  ) %>%
  mutate(
    across(where(is.numeric),formattable::comma,1)
  )

#CHANGE REFERENCE: 2020
temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = 3) #2020

#loop through response columns, run model to produce estimates/CIs
for (i in tablevals) {
  fit <- with(temp_data,lm(mice_imputed_data[[i]]~year))
  CI  <- summary(pool(fit),conf.int=TRUE) %>% mutate(
    term = ifelse(term=="(Intercept)","year2020",
                  ifelse(term=="year2018","year2018",
                         ifelse(term=="year2019","year2019",NA)))
  ) %>%
    select(term,estimate,`2.5 %`,`97.5 %`)
  CIt <- as.data.frame(t(as.matrix(CI)))
  rownames(CIt) <- colnames(CI)
  colnames(CIt) <- CI[,1] 
  CIt <- subset (CIt, select = -c(year2018,year2019))
  assign(i,CIt,envir = .GlobalEnv)
}

#rbind and finalize formatting
national.est.CI2020 <- do.call(rbind,mget(tablevals)) %>% #append all data frames for table
  filter(year2020!="year2020") %>% mutate(
    across(where(is.character), as.numeric)
    ) %>%
  mutate(
    across(where(is.numeric),round,1)
  ) %>%
  mutate(
    across(where(is.numeric),formattable::comma,1)
  )

#rbind each data frame: 2018, 2019, 2020
national.est.CI <- cbind(national.est.CI2018,national.est.CI2019,national.est.CI2020)

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

######################################################################################################################################################
# IMPUTATION
# MEAN
# NOTE - NOT USING THIS - USING MULTIPLE IMPUTATION ABOVE INSTEAD
######################################################################################################################################################

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

######################################################################################################################################################
# CALCULATE CHANGES / NATIONAL ESTIMATES
######################################################################################################################################################

# calculate percent change         
adm_pop_analysis <- mean_imputed_data %>% group_by(states) %>% mutate(total_admissions_pct = (total_admissions_mean / dplyr::lag(total_admissions_mean) -1)*100,
                                                                      total_population_pct = (total_population_mean / dplyr::lag(total_population_mean) -1)*100,
                                                                      total_violation_admissions_pct = (total_violation_admissions_mean / dplyr::lag(total_violation_admissions_mean) -1)*100,
                                                                      total_violation_population_pct = (total_violation_population_mean / dplyr::lag(total_violation_population_mean) -1)*100
                                                                      )

# rearrange data
adm_pop_analysis <- adm_pop_analysis %>% 
  select(sort(names(.)))
adm_pop_analysis <- adm_pop_analysis %>% select(states, year, everything())

# change data types
adm_pop_analysis$year <- factor(adm_pop_analysis$year)
adm_pop_analysis$total_admissions_mean <- as.numeric(adm_pop_analysis$total_admissions_mean )
adm_pop_analysis$total_population_mean  <- as.numeric(adm_pop_analysis$total_population_mean )
adm_pop_analysis$total_violation_admissions_mean  <- as.numeric(adm_pop_analysis$total_violation_admissions_mean )
adm_pop_analysis$total_violation_population_mean  <- as.numeric(adm_pop_analysis$total_violation_population_mean )

# sum of variables
adm_pop_national <- adm_pop_analysis %>% group_by(year) %>% dplyr::summarise(# admissions
                                                                             total_admissions = sum(total_admissions_mean),
                                                                             total_violation_admissions = sum(total_violation_admissions_mean),
                                                                             prob_admissions = sum(total_probation_violation_admissions_mean),
                                                                             new_prob_admissions = sum(new_offense_probation_violation_admissions_mean),
                                                                             tech_prob_admissions = sum(technical_probation_violation_admissions_mean),
                                                                             parole_admissions = sum(total_parole_violation_admissions_mean),
                                                                             new_parole_admissions = sum(new_offense_parole_violation_admissions_mean),
                                                                             tech_parole_admissions = sum(technical_parole_violation_admissions_mean),
                                                                             # population
                                                                             total_population = sum(total_population_mean),
                                                                             total_violation_population = sum(total_violation_population_mean),
                                                                             prob_population = sum(total_probation_violation_population_mean),
                                                                             new_prob_population = sum(new_offense_probation_violation_population_mean),
                                                                             tech_prob_population = sum(technical_probation_violation_population_mean),
                                                                             parole_population = sum(total_parole_violation_population_mean),
                                                                             new_parole_population = sum(new_offense_parole_violation_population_mean),
                                                                             tech_parole_population = sum(technical_parole_violation_population_mean)
                                                                             )

# # transpose
# x = colnames(adm_pop_national[,-1])
# tmp <- melt(adm_pop_national, id_vars = "year", 
#             measure_vars = x, 
#             variable_name = "type", 
#             value_name = "value", 
#             na.rm = TRUE)

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

######################################################################################################################################################
# Costs
######################################################################################################################################################

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

# select variables and filter by year
costs_pop_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% select(states, 
                                                                       total_population_mean, 
                                                                       total_violation_population_mean, 
                                                                       technical_probation_violation_population_mean, 
                                                                       technical_parole_violation_population_mean)
costs_pop_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% select(states, 
                                                                       total_population_mean, 
                                                                       total_violation_population_mean, 
                                                                       technical_probation_violation_population_mean, 
                                                                       technical_parole_violation_population_mean)

# add technical prob and parole together to get total technical for 2019 and 2020
costs_pop_2019 <- costs_pop_2019 %>%  mutate(total_population_2019 = total_population_mean,
                                             total_viol_population_2019 = total_violation_population_mean,
                                             technical_population_2019 = technical_probation_violation_population_mean + technical_parole_violation_population_mean) %>%
  select(-technical_probation_violation_population_mean,
         -technical_parole_violation_population_mean,
         -total_violation_population_mean,
         -total_population_mean)
costs_pop_2020 <- costs_pop_2020 %>%  mutate(total_population_2020 = total_population_mean,
                                             total_viol_population_2020 = total_violation_population_mean,
                                             technical_population_2020 = technical_probation_violation_population_mean + technical_parole_violation_population_mean) %>%
  select(-technical_probation_violation_population_mean,
         -technical_parole_violation_population_mean,
         -total_violation_population_mean,
         -total_population_mean)

# merge costs and population numbers
costs_pop_df <- merge(costs_pop_2019, costs, by = "states", all.x = TRUE, all.y = TRUE)
costs_pop_df <- merge(costs_pop_df, costs_pop_2020, by = "states", all.x = TRUE, all.y = TRUE)

# prevent rounding
options(digits=20)

# sum columns
costs_final <- costs_pop_df %>% select(-cost_2020, -cost_2019)
costs_final <- costs_final %>%
  summarize_if(is_numeric, sum, na.rm=TRUE)
costs_final$total <- "total"

# transpose
costs_final <- recast(costs_final, variable~total, value.var='value')

# add avg costs per day
costs_final$avg_cost_per_day_19 <- mean(costs_pop_df$cost_2019)
costs_final$avg_cost_per_day_20 <- mean(costs_pop_df$cost_2020)

# add cost per day depending on year
costs_final <- costs_final %>% mutate(avg_cost_per_day = ifelse(variable == "total_population_2019"|
                                                                variable == "total_viol_population_2019"|
                                                                variable == "technical_population_2019", 
                                                                avg_cost_per_day_19, avg_cost_per_day_20))

# remove unwanted variables
costs_final <- costs_final %>% select(-avg_cost_per_day_19,-avg_cost_per_day_20) 

# calculate average yearly cost
costs_final <- costs_final %>% mutate(average_yearly_cost = total*avg_cost_per_day*365)

var.labels = c(variable = "Metric", 
               total = "Total",
               avg_cost_per_day = "Average cost per day",
               average_yearly_cost = "Average cost per year (total x average cost per day x 365 days)"
               )
label(costs_final) = as.list(var.labels[match(names(costs_final), names(var.labels))])

# save as csv
write.xlsx(costs_final, "shared_data/Costs for web team 2021.xlsx")
