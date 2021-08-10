#######################################
# More Community, Less Confinement
# Impute missing values and calculate national estimates
# by Mari Roberts, Joshua Mallett
# Date: 7/14/2021 - finalize MI counts w/CI's
#######################################

# create vector of necessary packages
requiredPackages = c('rstudioapi',
                     'dplyr',
                     'openxlsx',
                     'readr',
                     'reshape',
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

# set wd based on your current program path (for collaboration)
# requires rstudioapi package
setwd(dirname(getActiveDocumentContext()$path))

# load data
source("clean.R")

# merge together
adm_pop <- merge(admissions, population, by = c("states","year"))

# reorder variables and sort states alphabetically
adm_pop <- adm_pop %>% select(states, year, everything()) %>% arrange(desc(states))

################################
# Impute missing values
# Create national estimate variables
################################

#New National Variables/columns
m.imp <- adm_pop %>% mutate(
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

# check imputed data
# each observation (first column left) within each imputed data set (first row at the top)
temp_data$imp$overall_admissions

# completed dataset
mice_imputed_data <- complete(temp_data,1)

################################
# Produce confidence intervals
# Post multiple imputation t-test
################################

# grab response columns for creating estimated counts/CIs
tablevals<- colnames(mice_imputed_data[3:10])

# aggregated data: national level
for (i in tablevals) {
  natmice <- setNames(aggregate(mice_imputed_data[[i]], by=list(Category=mice_imputed_data$year), FUN=sum)[2],i)
  assign(i,natmice,envir = .GlobalEnv)
}
mice_imputed_data.nat <- do.call(cbind,mget(tablevals)) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

# add year 
mice_imputed_data.nat$year <- ifelse(row.names(mice_imputed_data.nat)=="1",2018,
                                     ifelse(row.names(mice_imputed_data.nat)=="2",2019,
                                            ifelse(row.names(mice_imputed_data.nat)=="3",2020,NA)))
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
label(mice_imputed_data.nat) = as.list(var.labels[match(names(mice_imputed_data.nat), names(var.labels))])

#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (i in tablevals) {
  #j = 1,2,3 - 1=2018, 2=2019, 3=2020
  for (j in 1:3) { 
    test<-data.frame(CI95=poisson.test(mice_imputed_data.nat[j,i], conf.level = 0.95)$conf.int[1:2])
    colnames(test)[colnames(test)=="CI95"] <- paste0(i,j)
    assign(paste0(i,j),test,envir = .GlobalEnv)
  }
}
mice_imputed_data.natCI <- do.call(cbind,mget(c(paste0(tablevals,"1"),paste0(tablevals,"2"),paste0(tablevals,"3")))) %>%
  mutate(across(where(is.numeric),formattable::comma,1))

#USE mice_imputed_data.nat for National Totals
#USE mice_imputed_data.natCI for National 95% CIs
#2018
c(mice_imputed_data.nat[1,1],mice_imputed_data.natCI[1,1],mice_imputed_data.natCI[2,1])
c(mice_imputed_data.nat[1,2],mice_imputed_data.natCI[1,2],mice_imputed_data.natCI[2,2])
c(mice_imputed_data.nat[1,3],mice_imputed_data.natCI[1,3],mice_imputed_data.natCI[2,3])
c(mice_imputed_data.nat[1,4],mice_imputed_data.natCI[1,4],mice_imputed_data.natCI[2,4])
c(mice_imputed_data.nat[1,5],mice_imputed_data.natCI[1,5],mice_imputed_data.natCI[2,5])
c(mice_imputed_data.nat[1,6],mice_imputed_data.natCI[1,6],mice_imputed_data.natCI[2,6])
c(mice_imputed_data.nat[1,7],mice_imputed_data.natCI[1,7],mice_imputed_data.natCI[2,7])
c(mice_imputed_data.nat[1,8],mice_imputed_data.natCI[1,8],mice_imputed_data.natCI[2,8])

#2019
c(mice_imputed_data.nat[2,1],mice_imputed_data.natCI[1,9], mice_imputed_data.natCI[2,9])
c(mice_imputed_data.nat[2,2],mice_imputed_data.natCI[1,10],mice_imputed_data.natCI[2,10])
c(mice_imputed_data.nat[2,3],mice_imputed_data.natCI[1,11],mice_imputed_data.natCI[2,11])
c(mice_imputed_data.nat[2,4],mice_imputed_data.natCI[1,12],mice_imputed_data.natCI[2,12])
c(mice_imputed_data.nat[2,5],mice_imputed_data.natCI[1,13],mice_imputed_data.natCI[2,13])
c(mice_imputed_data.nat[2,6],mice_imputed_data.natCI[1,14],mice_imputed_data.natCI[2,14])
c(mice_imputed_data.nat[2,7],mice_imputed_data.natCI[1,15],mice_imputed_data.natCI[2,15])
c(mice_imputed_data.nat[2,8],mice_imputed_data.natCI[1,16],mice_imputed_data.natCI[2,16])

#2020
c(mice_imputed_data.nat[3,1],mice_imputed_data.natCI[1,17],mice_imputed_data.natCI[2,17])
c(mice_imputed_data.nat[3,2],mice_imputed_data.natCI[1,18],mice_imputed_data.natCI[2,18])
c(mice_imputed_data.nat[3,3],mice_imputed_data.natCI[1,19],mice_imputed_data.natCI[2,19])
c(mice_imputed_data.nat[3,4],mice_imputed_data.natCI[1,20],mice_imputed_data.natCI[2,20])
c(mice_imputed_data.nat[3,5],mice_imputed_data.natCI[1,21],mice_imputed_data.natCI[2,21])
c(mice_imputed_data.nat[3,6],mice_imputed_data.natCI[1,22],mice_imputed_data.natCI[2,22])
c(mice_imputed_data.nat[3,7],mice_imputed_data.natCI[1,23],mice_imputed_data.natCI[2,23])
c(mice_imputed_data.nat[3,8],mice_imputed_data.natCI[1,24],mice_imputed_data.natCI[2,24])

################################
# Calculate changes by year
################################

# calculate percent change         
adm_pop <- mice_imputed_data %>% group_by(states) %>% 
  mutate(overall_admissions_pct = (overall_admissions / dplyr::lag(overall_admissions) -1)*100,
         overall_population_pct = (overall_population / dplyr::lag(overall_population) -1)*100,
         admissions_for_violations_pct = (admissions_for_violations / dplyr::lag(admissions_for_violations) -1)*100,
         violator_population_pct = (violator_population / dplyr::lag(violator_population) -1)*100
  )

# change data types
adm_pop <- adm_pop %>% mutate_at(vars(-c("states", "year")), as.numeric)
adm_pop$year <- factor(adm_pop$year)

# sum of variables
adm_pop_national <- adm_pop %>% group_by(year) %>% dplyr::summarise(
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

# transpose data and clean columns
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

################################################################################
# Costs
################################################################################

# replace NAs in 2020 with 2019 cost
costs <- costs %>% mutate(cost = ifelse(is.na(cost_2020), cost_2019, cost_2020))

# select variables
costs <- costs %>% select(states, cost)

# rename variables and filter by 2019
costs_pop_2019 <- adm_pop %>% filter(year == 2019) %>% 
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
costs_pop_2020 <- adm_pop %>% filter(year == 2020) %>% 
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
costs_pop <- merge(costs_pop_2020, costs, by = c("states"), all.x = TRUE, all.y = TRUE)
costs_pop <- merge(costs_pop, costs_pop_2019, by = c("states"), all.x = TRUE, all.y = TRUE)

# prevent rounding
options(digits=20)

# calculate changes by year
cost_by_state <- costs_pop %>% 
  mutate(overall_population_change_19_20 = overall_population_2020 - overall_population_2019,
         violator_population_change_19_20 = violator_population_2020 - violator_population_2019,
         technical_violator_population_change_19_20 = technical_violator_population_2020 - technical_violator_population_2019,
         overall_admissions_change_19_20 = overall_admissions_2020 - overall_admissions_2019,
         admissions_for_violations_change_19_20 = admissions_for_violations_2020 - admissions_for_violations_2019,
         admissions_for_technical_violations_change_19_20 = admissions_for_technical_violations_2020 - admissions_for_technical_violations_2019)

# calculate yearly cost by type
cost_by_state <- cost_by_state %>%
  mutate(amount_saved_overall_population_change_19_20 = overall_population_change_19_20*cost*365,
         amount_saved_violator_population_change_19_20 = violator_population_change_19_20*cost*365, 
         amount_saved_technical_violator_change_19_20 = technical_violator_population_change_19_20*cost*365,
         amount_saved_overall_admissions_change_19_20 = overall_admissions_change_19_20*cost*365,
         amount_saved_admissions_for_violations_change_19_20 = admissions_for_violations_change_19_20*cost*365,
         amount_saved_admissions_for_technical_violations_change_19_20 = admissions_for_technical_violations_change_19_20*cost*365)

# reorder variables
cost_by_state <- cost_by_state %>% select(states,
                                          cost,
                                          overall_population_2019,
                                          violator_population_2019,
                                          technical_violator_population_2019,
                                          overall_population_2020,
                                          violator_population_2020,
                                          technical_violator_population_2020,
                                          everything())

# absolute value for costs
cost_by_state$amount_saved_overall_population_change_19_20 <- abs(cost_by_state$amount_saved_overall_population_change_19_20)
cost_by_state$amount_saved_violator_population_change_19_20 <- abs(cost_by_state$amount_saved_violator_population_change_19_20)
cost_by_state$amount_saved_technical_violator_change_19_20 <- abs(cost_by_state$amount_saved_technical_violator_change_19_20)
cost_by_state$amount_saved_overall_admissions_change_19_20 <- abs(cost_by_state$amount_saved_overall_admissions_change_19_20)
cost_by_state$amount_saved_admissions_for_violations_change_19_20 <- abs(cost_by_state$amount_saved_admissions_for_violations_change_19_20)
cost_by_state$amount_saved_admissions_for_technical_violations_change_19_20 <- abs(cost_by_state$amount_saved_admissions_for_technical_violations_change_19_20)

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

label(cost_by_state) = as.list(var.labels[match(names(cost_by_state), names(var.labels))])

# get national number
national_savings <- cost_by_state %>% 
  summarise(amount_saved_violator_population_change_19_20 = sum(amount_saved_violator_population_change_19_20),
            amount_saved_admissions_for_violations_change_19_20 = sum(amount_saved_admissions_for_violations_change_19_20))