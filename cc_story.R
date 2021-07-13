#######################################
# Confined and Costly survey
# Impute values, costs, national estimates data for comms
# by Mari Roberts
# 7/13/2021
#######################################

# read automated_clean to get data
source("automated_clean.R")

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
                     'VIM',
                     'finalfit',
                     'MissMech'
)
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# get working directory depending on login
getwd <- function(){
  thisLogin <- sys_info()['login']
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

# dup for cleaning
adm_analysis <- admissions
pop_analysis <- population

# merge together
adm_pop_analysis <- merge(admissions, population, by = c("states","year"))

# set up tables for change
adm_pop_analysis <- adm_pop_analysis %>% select(states, year, everything()) %>% arrange(desc(states))

# remove 2017
adm_pop_analysis <- adm_pop_analysis %>% filter(year != 2017)

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
######################################################################################################################################################

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(adm_pop_analysis,2,pMiss)

# Plots
# md_pattern(adm_pop_analysis)
# marginplot(adm_pop_analysis[c(1,2)])
# aggr_plot <- aggr(adm_pop_analysis, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(adm_pop_analysis), cex_axis=_7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method, predictive mean matching
temp_data <- mice(adm_pop_analysis,m=5,maxit=50,meth='pmm',seed=500)
summary(temp_data)

# check imputed data
# each observation (first column left) within each imputed dataset (first row at the top)
temp_data$imp$total_admissions

# completed dataset
mice_imputed_data <- complete(temp_data,1)

# inspect the distribution of original and imputed data
# want to see magenta points (imputed) match the shape of the blue ones (observed)
# plausible values
# xyplot(temp_data,total_admissions ~ total_population,pch=18,cex=1)
# densityplot(temp_data)

######################################################################################################################################################
# IMPUTATION
# MEAN
######################################################################################################################################################

# Create mean
mean_imputed_data <-  adm_pop_analysis %>% 
  group_by(year) %>% 
  mutate(total_admissions_mean = ifelse(is.na(total_admissions), mean(total_admissions, na.rm=T), total_admissions),
         total_violation_admissions_mean = ifelse(is.na(total_violation_admissions), mean(total_violation_admissions, na.rm=T), total_violation_admissions),      
         total_probation_violation_admissions_mean = ifelse(is.na(total_probation_violation_admissions), mean(total_probation_violation_admissions, na.rm=T), total_probation_violation_admissions),      
         New_offense_probation_violation_admissions_mean = ifelse(is.na(New_offense_probation_violation_admissions), mean(New_offense_probation_violation_admissions, na.rm=T), New_offense_probation_violation_admissions),  
         technical_probation_violation_admissions_mean = ifelse(is.na(technical_probation_violation_admissions), mean(technical_probation_violation_admissions, na.rm=T), technical_probation_violation_admissions),  
         total_parole_violation_admissions_mean = ifelse(is.na(total_parole_violation_admissions), mean(total_parole_violation_admissions, na.rm=T), total_parole_violation_admissions),           
         New_offense_parole_violation_admissions_mean = ifelse(is.na(New_offense_parole_violation_admissions), mean(New_offense_parole_violation_admissions, na.rm=T), New_offense_parole_violation_admissions),  
         technical_parole_violation_admissions_mean = ifelse(is.na(technical_parole_violation_admissions), mean(technical_parole_violation_admissions, na.rm=T), technical_parole_violation_admissions),       
         
         total_population_mean = ifelse(is.na(total_population), mean(total_population, na.rm=T), total_population),  
         total_violation_population_mean = ifelse(is.na(total_violation_population), mean(total_violation_population, na.rm=T), total_violation_population),                  
         total_probation_violation_population_mean = ifelse(is.na(total_probation_violation_population), mean(total_probation_violation_population, na.rm=T), total_probation_violation_population),  
         New_offense_probation_violation_population_mean = ifelse(is.na(New_offense_probation_violation_population), mean(New_offense_probation_violation_population, na.rm=T), New_offense_probation_violation_population),  
         technical_probation_violation_population_mean = ifelse(is.na(technical_probation_violation_population), mean(technical_probation_violation_population, na.rm=T), technical_probation_violation_population),  
         total_parole_violation_population_mean = ifelse(is.na(total_parole_violation_population), mean(total_parole_violation_population, na.rm=T), total_parole_violation_population),           
         New_offense_parole_violation_population_mean = ifelse(is.na(New_offense_parole_violation_population), mean(New_offense_parole_violation_population, na.rm=T), New_offense_parole_violation_population),  
         technical_parole_violation_population_mean = ifelse(is.na(technical_parole_violation_population), mean(technical_parole_violation_population, na.rm=T), technical_parole_violation_population))       
      

######################################################################################################################################################
# CALCULATE CHANGEs / NATIONAL ESTIMATES
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
adm_pop_analysis$total_admissions_mean <- as_numeric(adm_pop_analysis$total_admissions_mean )
adm_pop_analysis$total_population_mean  <- as_numeric(adm_pop_analysis$total_population_mean )
adm_pop_analysis$total_violation_admissions_mean  <- as_numeric(adm_pop_analysis$total_violation_admissions_mean )
adm_pop_analysis$total_violation_population_mean  <- as_numeric(adm_pop_analysis$total_violation_population_mean )

# sum of variables
adm_pop_national <- adm_pop_analysis %>% group_by(year) %>% dplyr::summarise(# admissions
                                                                             total_admissions = sum(total_admissions_mean),
                                                                             total_violation_admissions = sum(total_violation_admissions_mean),
                                                                             prob_admissions = sum(total_probation_violation_admissions_mean),
                                                                             new_prob_admissions = sum(New_offense_probation_violation_admissions_mean),
                                                                             tech_prob_admissions = sum(technical_probation_violation_admissions_mean),
                                                                             parole_admissions = sum(total_parole_violation_admissions_mean),
                                                                             new_parole_admissions = sum(New_offense_parole_violation_admissions_mean),
                                                                             tech_parole_admissions = sum(technical_parole_violation_admissions_mean),
                                                                             # population
                                                                             total_population = sum(total_population_mean),
                                                                             total_violation_population = sum(total_violation_population_mean),
                                                                             prob_population = sum(total_probation_violation_population_mean),
                                                                             new_prob_population = sum(New_offense_probation_violation_population_mean),
                                                                             tech_prob_population = sum(technical_probation_violation_population_mean),
                                                                             parole_population = sum(total_parole_violation_population_mean),
                                                                             new_parole_population = sum(New_offense_parole_violation_population_mean),
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
national_estimates <- data_frame(national_estimates)
national_estimates <- tibble::rownames_to_column(national_estimates, "type")
national_estimates <- national_estimates %>% select(type, year2018 = X1, year2019 = X2, year2020 = X3)
national_estimates <- national_estimates %>% filter(type != "year")

# change data types
national_estimates$year2018 <- as_numeric(national_estimates$year2018)
national_estimates$year2019 <- as_numeric(national_estimates$year2019)
national_estimates$year2020 <- as_numeric(national_estimates$year2020)

# create changes variables
national_estimates <- national_estimates %>% mutate(change_18_19 = year2019-year2018,
                                                    change_19_20 = year2020-year2019,
                                                    change_18_20 = year2020-year2018,
                                                    pct_18_19 = ((year2019-year2018)/year2018),
                                                    pct_19_20 = ((year2020-year2019)/year2019),
                                                    pct_18_20 = ((year2020-year2018)/year2018)
                                                    )

# save data to send to comms
write.xlsx(national_estimates, "shared_data/National estimates for web team 2021.xlsx")

######################################################################################################################################################
# Costs
######################################################################################################################################################

# get cost data for 2021
costs <- read_xlsx("data/Cost and notes data 2021.xlsx", .name_repair = "universal")
costs <- costs %>% filter(states != "NA")

# select general cost info
costs <- costs %>% select(states, Cost)

# remove dollar sign
costs$cost_2020 = as_numeric(gsub("\\$", "", costs$Cost))
costs <- costs %>% select(-Cost)

# get cost data from 2019 - using costs from 2019 if cost data is missing for 2020
costs2019 <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs2019 <- costs2019 %>% select(states, cost_2019 = `State Reported CostPerDay`)

# replace NAs in 2020 cost data with data from 2019
setDt(costs); setDt(costs2019)
costs[is.na(cost_2020), cost_2020 := costs2019[.sD, on=.(states), x_cost_2019]]
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

# add technical prob and parole together to get total technical 
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

# save as csv
write.xlsx(costs_final, "shared_data/Costs for web team 2021.xlsx")
