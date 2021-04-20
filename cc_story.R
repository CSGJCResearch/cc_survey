#######################################
# Confined and Costly Survey
# Impute values, plots, data for comms
# by Mari Roberts
# 3/29/2021
#######################################

# read automated_clean to get data
# source("automated_clean.R")

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

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 10),size=10,face="bold"),
                     #axis.text.y = element_text(size=10),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size=10,face="bold"),
                     legend.position = "top",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))

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

# merge together
adm_pop_analysis <- merge(adm, population, by = c("States","year"))

# set up tables for change
adm_pop_analysis <- adm_pop_analysis %>% select(States, year, everything()) %>% arrange(desc(States))

# remove 2017
adm_pop_analysis <- adm_pop_analysis %>% filter(year != 2017)

# remove unwanted variables
adm_pop_analysis <- adm_pop_analysis %>% select(-Total.probation.violation.admissions, -Total.parole.violation.admissions, 
                                                -Technical.probation.violation.admissions,-Technical.parole.violation.admissions,
                                                -New.offense.parole.violation.admissions,-New.offense.probation.violation.admissions,
                                                -Total.probation.violation.population, -Total.parole.violation.population, 
                                                -Technical.probation.violation.population,-Technical.parole.violation.population,
                                                -New.offense.parole.violation.population,-New.offense.probation.violation.population,
                                                -Total.violation.admissions,-Total.violation.population)

######################################################################################################################################################
# MISSINGNESS
######################################################################################################################################################

miss_2018 <- adm_pop_analysis %>% filter(year == 2018) %>% select(state = States, admissions_18 = Total.admissions, population_18 = Total.population) 
miss_2019 <- adm_pop_analysis %>% filter(year == 2019) %>% select(state = States, admissions_19 = Total.admissions, population_19 = Total.population) 
miss_2020 <- adm_pop_analysis %>% filter(year == 2020) %>% select(state = States, admissions_20 = Total.admissions, population_20 = Total.population) 
miss_data <- merge(miss_2018, miss_2019, by = c("state"))
miss_data <- merge(miss_data, miss_2020, by = c("state"))
miss_data$state <- factor(miss_data$state)

# examine missing data with ff_glimpse
explanatory = c("admissions_19", "admissions_20", 
                "population_18", "population_19", "population_20")
dependent = "admissions_18"
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
x <- as.data.frame(abs(is.na(miss_data)))

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
TestMCARNormality(data = miss_data1,del.lesscases = 1)

######################################################################################################################################################
# IMPUTATION
# MICE
######################################################################################################################################################

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(adm_pop_analysis,2,pMiss)

# md.pattern(adm_pop_analysis)
# marginplot(adm_pop_analysis[c(1,2)])
# only 30% of data isnt missing info
aggr_plot <- aggr(adm_pop_analysis, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(adm_pop_analysis), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method
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
xyplot(temp_data,Total.admissions ~ Total.population,pch=18,cex=1)
densityplot(temp_data)

######################################################################################################################################################
# IMPUTATION
# MEAN
######################################################################################################################################################

# if the column data type is num, impute with mean
for (cols in colnames(adm_pop_analysis)) {
  if (cols %in% names(adm_pop_analysis[,sapply(adm_pop_analysis, is.numeric)])) {
    mean_imputed_data <- adm_pop_analysis %>% group_by(year) %>% mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    mean_imputed_data <- adm_pop_analysis %>% group_by(year) %>% mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
  }
}

######################################################################################################################################################
# CALCULATE CHANGES / NATUINAL ESTIMATES
######################################################################################################################################################

# calculate percent change         
adm_pop_analysis <- mean_imputed_data %>% group_by(States) %>% mutate(Total.admissions.pct = (Total.admissions / dplyr::lag(Total.admissions) -1)*100)
adm_pop_analysis <- adm_pop_analysis %>% group_by(States) %>% mutate(Total.population.pct = (Total.population / dplyr::lag(Total.population) -1)*100)

# create national estimate for admissions and pop change from 2018 to 2020 
##### fix bug
adm_pop_analysis$year <- factor(adm_pop_analysis$year)
adm_pop_analysis$Total.admissions <- as.numeric(adm_pop_analysis$Total.admissions)
adm_pop_analysis$Total.population <- as.numeric(adm_pop_analysis$Total.population)
adm_pop_national <- adm_pop_analysis %>% group_by(year) %>% dplyr::summarise(total.admissions = sum(Total.admissions),
                                                                             total.population = sum(Total.population))

adm_pop_national <- adm_pop_national %>%
  mutate(total.admissions.pct = (total.admissions - lag(total.admissions)) / lag(total.admissions),
         total.admissions.change = total.admissions - lag(total.admissions),
         total.population.pct = (total.population - lag(total.population)) / lag(total.population),
         total.population.change = total.population - lag(total.population))

######################################################################################################################################################
# Costs
######################################################################################################################################################

# get cost data for 2021
costs <- read.csv("data/Data for web team 2020 v6 CORRECTED/Survey 2021-Table 1.csv")

# select general cost info
costs <- costs %>% select(States, Cost)

# remove dollar sign
costs$cost_2020 = as.numeric(gsub("\\$", "", costs$Cost))
costs <- costs %>% select(-Cost)

# get cost data from 2019
costs2019 <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs2019 <- costs2019 %>% select(States, cost_2019 = `State Reported CostPerDay`)

# replace NAs in 2020 cost data with data from 2019
setDT(costs); setDT(costs2019)
costs[is.na(cost_2020), cost_2020 := costs2019[.SD, on=.(States), x.cost_2019]]
costs <- merge(costs, costs2019, by = "States")

# creat costs_pop df
costs_pop_2019 <- population %>% filter(year == 2019) %>% select(States, Total.violation.population, Technical.probation.violation.population, Technical.parole.violation.population)
costs_pop_2020 <- population %>% filter(year == 2020) %>% select(States, Total.violation.population, Technical.probation.violation.population, Technical.parole.violation.population)

# add technical prob and parole together to get tech number
costs_pop_2019 <- costs_pop_2019 %>%  mutate(total_population_2019 = Total.violation.population,
                                             technical_population_2019 = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-Technical.probation.violation.population,
         -Technical.parole.violation.population,
         -Total.violation.population)
costs_pop_2020 <- costs_pop_2020 %>%  mutate(total_population_2020 = Total.violation.population,
                                             technical_population_2020 = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-Technical.probation.violation.population,
         -Technical.parole.violation.population,
         -Total.violation.population)

# merge costs and population numbers
costs_pop_df <- merge(costs_pop_2019, costs, by = "States", all.x = TRUE, all.y = TRUE)
costs_pop_df <- merge(costs_pop_df, costs_pop_2020, by = "States", all.x = TRUE, all.y = TRUE)

# calc costs
costs_pop_df <- costs_pop_df %>% mutate(pop_sup_cost_2019 = total_population_2019*cost_2019*365,
                                        pop_tech_cost_2019 = technical_population_2019*cost_2019*365,
                                        pop_sup_cost_2020 = total_population_2020*cost_2020*365,
                                        pop_tech_cost_2020 = technical_population_2020*cost_2020*365) 

# rearrange data
costs_pop_df <- costs_pop_df %>% select(States, cost_2019,cost_2020,
                                        pop_sup_cost_2019,pop_sup_cost_2020,
                                        pop_tech_cost_2019,pop_tech_cost_2020,
                                        everything())

avg_sup_cost <- mean(costs_pop_df$pop_sup_cost_2020)
avg_sup_cost # $171,439,834

#########################################
# prison info
#########################################

prisons_data <- read_csv("data/CSG revocations model v0.3_010421.csv")
prisons_data <- prisons_data %>% select(State, `Data Year`, `Number of facilities [input]`,`State-wide capacity [input]`)

#########################################
# write csv files
#########################################

# write csv files to share and add to shared folder
write.csv(adm_decline, "shared_data/cc_admissions_changes.csv")
write.csv(pop_decline, "shared_data/cc_population_changes.csv")
write.csv(costs_pop_df, "shared_data/supervising_costs.csv")
write.csv(prisons_data, "shared_data/prisons_data.csv")

######################################################################################################################################################
# GRAPHS
# Changes in admissions by state
######################################################################################################################################################

# read in data
source("automated_clean.R")

# 1. Chart with states with largest drops overall admissions

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# arrange data by largest drops
largest_drops_adm <- df %>% arrange(Total.admissions.pct) %>% head(10)

# only look at states with largest drops
df <- df %>% filter(States %in% largest_drops_adm$States)

plot1 <- ggplot(df, aes(x=reorder(States, -Total.admissions), y=Total.admissions, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  ggtitle("States with the Largest Drops in Total Admissions") + 
  theme_bw() + theme_csgjc + 
  geom_text(aes(label = scales::comma(round(Total.admissions), accuracy=1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),vjust=-.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
  geom_label(data = df, vjust = 1,
             mapping = aes(label = ifelse(Total.admissions.pct>0,paste0("", round(Total.admissions.pct,1),"%"),
                                          paste0("",Total.admissions.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) 
plot1

# 2. Chart with states with largest drops in supervision violation admissions
# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

# arrange data by largest drops
largest_drops_viol_adm <- df %>% arrange(Total.violation.admissions.pct) %>% head(10)

# only look at states with largest drops
df <- df %>% filter(States %in% largest_drops_viol_adm$States)
df$Total.violation.admissions <- round(df$Total.violation.admissions,0)

plot1 <- ggplot(df, aes(x=reorder(States, -Total.violation.admissions), y=Total.violation.admissions, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  ggtitle("States with the Largest Drops in Violation Admissions") + 
  theme_bw() + theme_csgjc + 
  geom_text(aes(label = scales::comma(round(Total.violation.admissions), accuracy=1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),vjust=-.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
  geom_label(data = df, vjust = 1,
             mapping = aes(label = ifelse(Total.violation.admissions.pct>0,paste0("", round(Total.violation.admissions.pct,1),"%"),
                                          paste0("",Total.violation.admissions.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) 
plot1

# 3. Chart with states with largest drops in technical violation admissions
df_2020 <- adm_change %>% filter(year == 2020) 
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Technical.violations.pct <- round(df$Technical.violations.pct ,1)

# arrange data by largest drops
largest_drops_viol_adm <- df %>% arrange(Technical.violations.pct) %>% head(10)

# only look at states with largest drops
df <- df %>% filter(States %in% largest_drops_viol_adm$States)
df$Technical.violations <- round(df$Technical.violations,0)

plot1 <- ggplot(df, aes(x=reorder(States, -Technical.violations), y=Technical.violations, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  ggtitle("States with the Largest Drops in Technical Violation Admissions") + 
  theme_bw() + theme_csgjc + 
  geom_text(aes(label = scales::comma(round(Technical.violations), accuracy=1)), 
            position = position_dodge2(width = 0.9, preserve = "single"),vjust=-.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
  geom_label(data = df, vjust = 1,
             mapping = aes(label = ifelse(Technical.violations.pct>0,paste0("", round(Technical.violations.pct,1),"%"),
                                          paste0("",Technical.violations.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) 
plot1
