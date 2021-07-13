#######################################
# Confined and Costly Survey
# Impute values, plots, data for comms
# by Mari Roberts
# 3/29/2021
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
                     'finalfit'
)
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

# dup for cleaning
adm_analysis <- adm
pop_analysis <- population

# rename variables
adm <- adm %>% mutate(New.commitments.admissions = New.commitments) %>% select(-New.commitments)
population <- population %>% mutate(New.commitments.population = New.commitments) %>% select(-New.commitments)

# merge together
adm_pop_analysis <- merge(adm, population, by = c("States","year"))

# set up tables for change
adm_pop_analysis <- adm_pop_analysis %>% select(States, year, everything()) %>% arrange(desc(States))

# remove 2017
adm_pop_analysis <- adm_pop_analysis %>% filter(year != 2017)

# remove unwanted variables
adm_pop_analysis <- adm_pop_analysis %>% select(-New.commitments.admissions, -New.commitments.population)

######################################################################################################################################################
# CHECK MISSINGNESS
######################################################################################################################################################

# miss_2018 <- adm_pop_analysis %>% filter(year == 2018) %>% select(state = States, admissions_18 = Total.admissions, population_18 = Total.population) 
# miss_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% select(state = States, admissions_19 = Total.admissions, population_19 = Total.population) 
# miss_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% select(state = States, admissions_20 = Total.admissions, population_20 = Total.population) 
# miss_data <- merge(miss_2018, miss_2019, by = c("state"))
# miss_data <- merge(miss_data, miss_2020, by = c("state"))
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
# x <- as.data.frame(abs(is.na(miss_data)))
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

######################################################################################################################################################
# IMPUTATION
# MICE
######################################################################################################################################################

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(adm_pop_analysis,2,pMiss)

# Plots
# md.pattern(adm_pop_analysis)
# marginplot(adm_pop_analysis[c(1,2)])
# aggr_plot <- aggr(adm_pop_analysis, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(adm_pop_analysis), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method, predictive mean matching
temp_data <- mice(adm_pop_analysis,m=5,maxit=50,meth='pmm',seed=500)
summary(temp_data)

# check imputed data
# each observation (first column left) within each imputed dataset (first row at the top)
temp_data$imp$Total.admissions

# completed dataset
mice_imputed_data <- complete(temp_data,1)

# inspect the distribution of original and imputed data
# want to see magenta points (imputed) match the shape of the blue ones (observed)
# plausible values
# xyplot(temp_data,Total.admissions ~ Total.population,pch=18,cex=1)
# densityplot(temp_data)

######################################################################################################################################################
# IMPUTATION
# MEAN
######################################################################################################################################################

# Create mean
mean_imputed_data <-  adm_pop_analysis %>% 
  group_by(year) %>% 
  mutate(Total.admissions.mean = ifelse(is.na(Total.admissions), mean(Total.admissions, na.rm=T), Total.admissions),
         Total.violation.admissions.mean = ifelse(is.na(Total.violation.admissions), mean(Total.violation.admissions, na.rm=T), Total.violation.admissions),      
         Total.probation.violation.admissions.mean = ifelse(is.na(Total.probation.violation.admissions), mean(Total.probation.violation.admissions, na.rm=T), Total.probation.violation.admissions),      
         New.offense.probation.violation.admissions.mean = ifelse(is.na(New.offense.probation.violation.admissions), mean(New.offense.probation.violation.admissions, na.rm=T), New.offense.probation.violation.admissions),  
         Technical.probation.violation.admissions.mean = ifelse(is.na(Technical.probation.violation.admissions), mean(Technical.probation.violation.admissions, na.rm=T), Technical.probation.violation.admissions),  
         Total.parole.violation.admissions.mean = ifelse(is.na(Total.parole.violation.admissions), mean(Total.parole.violation.admissions, na.rm=T), Total.parole.violation.admissions),           
         New.offense.parole.violation.admissions.mean = ifelse(is.na(New.offense.parole.violation.admissions), mean(New.offense.parole.violation.admissions, na.rm=T), New.offense.parole.violation.admissions),  
         Technical.parole.violation.admissions.mean = ifelse(is.na(Technical.parole.violation.admissions), mean(Technical.parole.violation.admissions, na.rm=T), Technical.parole.violation.admissions),       
         
         Total.population.mean = ifelse(is.na(Total.population), mean(Total.population, na.rm=T), Total.population),  
         Total.violation.population.mean = ifelse(is.na(Total.violation.population), mean(Total.violation.population, na.rm=T), Total.violation.population),                  
         Total.probation.violation.population.mean = ifelse(is.na(Total.probation.violation.population), mean(Total.probation.violation.population, na.rm=T), Total.probation.violation.population),  
         New.offense.probation.violation.population.mean = ifelse(is.na(New.offense.probation.violation.population), mean(New.offense.probation.violation.population, na.rm=T), New.offense.probation.violation.population),  
         Technical.probation.violation.population.mean = ifelse(is.na(Technical.probation.violation.population), mean(Technical.probation.violation.population, na.rm=T), Technical.probation.violation.population),  
         Total.parole.violation.population.mean = ifelse(is.na(Total.parole.violation.population), mean(Total.parole.violation.population, na.rm=T), Total.parole.violation.population),           
         New.offense.parole.violation.population.mean = ifelse(is.na(New.offense.parole.violation.population), mean(New.offense.parole.violation.population, na.rm=T), New.offense.parole.violation.population),  
         Technical.parole.violation.population.mean = ifelse(is.na(Technical.parole.violation.population), mean(Technical.parole.violation.population, na.rm=T), Technical.parole.violation.population))       
      

######################################################################################################################################################
# CALCULATE CHANGES / NATUINAL ESTIMATES
######################################################################################################################################################

# calculate percent change         
adm_pop_analysis <- mean_imputed_data %>% group_by(States) %>% mutate(Total.admissions.pct = (Total.admissions.mean / dplyr::lag(Total.admissions.mean) -1)*100,
                                                                      Total.population.pct = (Total.population.mean / dplyr::lag(Total.population.mean) -1)*100,
                                                                      Total.violation.admissions.pct = (Total.violation.admissions.mean / dplyr::lag(Total.violation.admissions.mean) -1)*100,
                                                                      Total.violation.population.pct = (Total.violation.population.mean / dplyr::lag(Total.violation.population.mean) -1)*100
                                                                      )

# rearrange data
adm_pop_analysis <- adm_pop_analysis %>% 
  select(sort(names(.)))
adm_pop_analysis <- adm_pop_analysis %>% select(States, year, everything())

# change data types
adm_pop_analysis$year <- factor(adm_pop_analysis$year)
adm_pop_analysis$Total.admissions.mean <- as.numeric(adm_pop_analysis$Total.admissions.mean )
adm_pop_analysis$Total.population.mean  <- as.numeric(adm_pop_analysis$Total.population.mean )
adm_pop_analysis$Total.violation.admissions.mean  <- as.numeric(adm_pop_analysis$Total.violation.admissions.mean )
adm_pop_analysis$Total.violation.population.mean  <- as.numeric(adm_pop_analysis$Total.violation.population.mean )

# sum of variables
adm_pop_national <- adm_pop_analysis %>% group_by(year) %>% dplyr::summarise(# admissions
                                                                             total.admissions = sum(Total.admissions.mean),
                                                                             total.violation.admissions = sum(Total.violation.admissions.mean),
                                                                             prob.admissions = sum(Total.probation.violation.admissions.mean),
                                                                             new.prob.admissions = sum(New.offense.probation.violation.admissions.mean),
                                                                             tech.prob.admissions = sum(Technical.probation.violation.admissions.mean),
                                                                             parole.admissions = sum(Total.parole.violation.admissions.mean),
                                                                             new.parole.admissions = sum(New.offense.parole.violation.admissions.mean),
                                                                             tech.parole.admissions = sum(Technical.parole.violation.admissions.mean),
                                                                             # population
                                                                             total.population = sum(Total.population.mean),
                                                                             total.violation.population = sum(Total.violation.population.mean),
                                                                             prob.population = sum(Total.probation.violation.population.mean),
                                                                             new.prob.population = sum(New.offense.probation.violation.population.mean),
                                                                             tech.prob.population = sum(Technical.probation.violation.population.mean),
                                                                             parole.population = sum(Total.parole.violation.population.mean),
                                                                             new.parole.population = sum(New.offense.parole.violation.population.mean),
                                                                             tech.parole.population = sum(Technical.parole.violation.population.mean)
                                                                             )

# # transpose
# x = colnames(adm_pop_national[,-1])
# tmp <- melt(adm_pop_national, id.vars = "year", 
#             measure.vars = x, 
#             variable_name = "type", 
#             value.name = "value", 
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

# save data to send to comms
write.xlsx(national_estimates, "shared_data/National estimates for web team 2021.xlsx")

######################################################################################################################################################
# Costs
######################################################################################################################################################

# get cost data for 2021
costs <- read_xlsx("data/Cost and notes data 2021.xlsx", .name_repair = "universal")
costs <- costs %>% filter(States != "NA")

# select general cost info
costs <- costs %>% select(States, Cost)

# remove dollar sign
costs$cost_2020 = as.numeric(gsub("\\$", "", costs$Cost))
costs <- costs %>% select(-Cost)

# get cost data from 2019 - using costs from 2019 if cost data is missing for 2020
costs2019 <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs2019 <- costs2019 %>% select(States, cost_2019 = `State Reported CostPerDay`)

# replace NAs in 2020 cost data with data from 2019
setDT(costs); setDT(costs2019)
costs[is.na(cost_2020), cost_2020 := costs2019[.SD, on=.(States), x.cost_2019]]
costs <- merge(costs, costs2019, by = "States")

# select variables and filter by year
costs_pop_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% select(States, 
                                                                       Total.population.mean, 
                                                                       Total.violation.population.mean, 
                                                                       Technical.probation.violation.population.mean, 
                                                                       Technical.parole.violation.population.mean)
costs_pop_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% select(States, 
                                                                       Total.population.mean, 
                                                                       Total.violation.population.mean, 
                                                                       Technical.probation.violation.population.mean, 
                                                                       Technical.parole.violation.population.mean)

# add technical prob and parole together to get total technical 
costs_pop_2019 <- costs_pop_2019 %>%  mutate(total_population_2019 = Total.population.mean,
                                             total_viol_population_2019 = Total.violation.population.mean,
                                             technical_population_2019 = Technical.probation.violation.population.mean + Technical.parole.violation.population.mean) %>%
  select(-Technical.probation.violation.population.mean,
         -Technical.parole.violation.population.mean,
         -Total.violation.population.mean,
         -Total.population.mean)
costs_pop_2020 <- costs_pop_2020 %>%  mutate(total_population_2020 = Total.population.mean,
                                             total_viol_population_2020 = Total.violation.population.mean,
                                             technical_population_2020 = Technical.probation.violation.population.mean + Technical.parole.violation.population.mean) %>%
  select(-Technical.probation.violation.population.mean,
         -Technical.parole.violation.population.mean,
         -Total.violation.population.mean,
         -Total.population.mean)

# merge costs and population numbers
costs_pop_df <- merge(costs_pop_2019, costs, by = "States", all.x = TRUE, all.y = TRUE)
costs_pop_df <- merge(costs_pop_df, costs_pop_2020, by = "States", all.x = TRUE, all.y = TRUE)

# prevent rounding
options(digits=20)

# sum columns
costs_final <- costs_pop_df %>% select(-cost_2020, -cost_2019)
costs_final <- costs_final %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
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
