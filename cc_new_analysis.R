#######################################
# Confined and Costly Survey
# New Analysis Using UCR Data
# by Mari Roberts
# 3/29/2021
#######################################

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
                     'Hmisc',
                     'ggpubr',
                     'corrplot'
)
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# get working directory depending on login
getwd <- function(){
  thisLogin <- Sys.info()['login']
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

# read in cc survey data with imputed values
source("cc_story.R")

# import data
q1 <- read_excel("data/2020_Quarter_4_Quarterly Table 1.xlsx", skip = 4)
q2 <- read_excel("data/2020_Quarter_4_Quarterly Table 2.xlsx", skip = 4)
q3 <- read_excel("data/2020_Quarter_4_Quarterly Table 3.xls", skip = 4)
q4 <- read_excel("data/2020_Quarter_4_Quarterly Table 4.xlsx", skip = 4)
census.xlsx <- read_excel("data/nst-est2019-01.xlsx", skip = 3)
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

######
# ucr
######

# rows are missing state names, if NA, replace with state name in row above
ucr <- q4 %>% fill(State, City)

# remove unwanted rows
ucr <- ucr[-c(445:452),]

# remove commas from numbers and make numeric
ucr$Population1 <- as.character(gsub("\\,", "", ucr$Population1))
ucr$city_pop <- as.numeric(ucr$Population1)

# remove punctuation
ucr[] <- lapply(ucr, gsub, pattern=',', replacement='')
ucr <- data.frame(ucr)

# convert char to numeric (if its able to be numeric)
ucr[] <- lapply(ucr, function(x) type.convert(as.character(x), as.is = TRUE))

# factor state and year
ucr$state <- factor(ucr$State)
ucr$city <- factor(ucr$City)
ucr$year <- factor(ucr$Year)
ucr <- ucr %>% select(-State,-Year, -City)

# aggregate up to the state level
ucr_agg <- ucr %>% group_by(state, year) %>% summarise(ucr_pop = sum(city_pop),
                                                      state_violent_crime = sum(Violent.crime),
                                                      state_murder = sum(Murder),
                                                      state_rape = sum(Rape2),
                                                      state_roberry = sum(Robbery),
                                                      state_aggravated_assault = sum(Aggravated.Assault),
                                                      state_property_crime = sum(Property.crime),
                                                      state_burglary = sum(Burglary),
                                                      state_larceny_theft = sum(Larceny.theft),
                                                      state_motor_theft = sum(Motor.vehicle.theft),
                                                      state_arson = sum(Arson3))

# calculate change score for UCR crimes
ucr_agg <- ucr_agg %>% group_by(state) %>% 
  mutate(state_violent_crime_change = (state_violent_crime / lag(state_violent_crime)-1)*100,
         state_murder_change = (state_murder / lag(state_murder)-1)*100,
         state_rape_change = (state_rape / lag(state_rape)-1)*100,
         state_roberry_change = (state_roberry / lag(state_roberry)-1)*100,
         state_aggravated_assault_change = (state_aggravated_assault / lag(state_aggravated_assault)-1)*100,
         state_property_crime_change = (state_property_crime / lag(state_property_crime)-1)*100,
         state_burglary_change = (state_burglary / lag(state_burglary)-1)*100,
         state_larceny_theft_change = (state_larceny_theft / lag(state_larceny_theft)-1)*100,
         state_motor_theft_change = (state_motor_theft / lag(state_motor_theft)-1)*100,
         state_arson_change = (state_arson / lag(state_arson)-1)*100
         )

# subset to 2020 so that there is one row per state
ucr_agg_2020 <- ucr_agg %>% filter(year == 2020)

######
# census
######

# dup for cleaning and select variables (select 2019 for pop year)
census <- census.xlsx %>% select(state = `...1`, census_pop_2019 = `2019`)

# remove periods in state names
census$state <- as.character(gsub("\\.", "", census$state))

# remove unwanted rows
census <- census[-c(1:5,57:63),]

# factor state
census$state <- factor(census$state)

######
# merge with census data 
######

# merge
df <- merge(census, ucr_agg_2020, by = "state", all.y = TRUE)

# get % of pop that UCR data covers per state
df <- df %>% mutate(ucr_proportion = ucr_pop/census_pop_2019)

##################
# ANALYSIS
##################

# load cc data info
source("automated_clean.R")

# dup for cleaning
pop_change_anlysis <- pop_change
adm_change_anlysis <- adm_change

# select variables and filter to change in pop and admissions 2019-2020
pop_change_anlysis <- pop_change_anlysis %>% filter(year == 2020) %>%
  select(state = States, cc_population_change = Total.population.pct)
adm_change_anlysis <- adm_change_anlysis %>%  filter(year == 2020) %>% 
  select(state = States, cc_admissions_change = Total.admissions.pct)

########
# merge all data together
########

df_final <- merge(pop_change_anlysis, adm_change_anlysis, by = c("state"), all.x = TRUE, all.y = TRUE)
# df_final <- merge(df_final, df, by = "state", all.x = TRUE, all.y = TRUE) # keeps all data, NAs

df_final_noNAs <- merge(df_final, df, by = "state") # 30 UCR states (only 22 overlap)
df_final_noNAs <- df_final_noNAs %>% select(-census_pop_2019,-year,-ucr_pop,-ucr_proportion,
                                            -state_violent_crime,-state_murder,
                                            -state_rape,-state_roberry,-state_aggravated_assault,
                                            -state_property_crime,-state_burglary,-state_larceny_theft,            
                                            -state_motor_theft,-state_arson )
df_final_noNAs <- df_final_noNAs[complete.cases(df_final_noNAs), ]

df_final <- merge(df_final, df, by = "state", all.y = TRUE, all.x = TRUE) # 30 UCR states (only 22 overlap)

# rename and rearrange variables
df_final <- df_final %>% select(state, ucr_pop, census_pop_2019,ucr_proportion,
                    cc_population_change, cc_admissions_change,everything())


# write this csv
write.csv(df_final, "shared_data/ucr_final_data.csv")

########
# Correlate change in crime from 2019 to 2020 with change in total prison admissions and pops 2019-2020
########

# view data to find linear relationship
ggscatter(df_final, x = "cc_population_change", y = "state_violent_crime_change",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Population Change from 2019-2020", ylab = "Violent Crime Change")
ggscatter(df_final, x = "cc_population_change", y = "state_property_crime_change",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Population Change from 2019-2020", ylab = "Property Crime Change")
ggscatter(df_final, x = "cc_admissions_change", y = "state_violent_crime_change",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Admissions Change from 2019-2020", ylab = "Violent Crime Change")
ggscatter(df_final, x = "cc_admissions_change", y = "state_property_crime_change",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Admissions Change from 2019-2020", ylab = "Property Crime Change")

# check for normality
shapiro.test(df_final$state_violent_crime_change) # => p = 0.1889, normal
shapiro.test(df_final$state_property_crime_change) # => p = 0.2192, normal
shapiro.test(df_final$cc_admissions_change) # => p = 0.5726, normal
shapiro.test(df_final$cc_population_change) # => p = 0.3163, normal
ggqqplot(df_final$state_violent_crime_change, ylab = "state_violent_crime_change")
ggqqplot(df_final$state_property_crime_change, ylab = "state_property_crime_change")
# is normal

# subset data for analysis
df_sub <- df_final %>% select(-state,-ucr_pop, -census_pop_2019,-ucr_proportion,-year,
                              -state_violent_crime,-state_murder,
                              -state_rape,-state_roberry,-state_aggravated_assault,
                              -state_property_crime,-state_burglary,-state_larceny_theft,            
                              -state_motor_theft,-state_arson )

# compute correlation matrix
res <- cor(df_sub)
round(res, 2)

# compute correlation matrix
res2 <- rcorr(as.matrix(df_sub))
res2

# extract the correlation coefficients
res2$r

# extract p-values
res2$P

# custom function to format correlation matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# create correlation matrix
res2<-rcorr(as.matrix(df_sub[,1:12]))
flattenCorrMatrix(res2$r, res2$P)

# visualize significant and insignificant correlations
cor_view <- rcorr(as.matrix(df_sub))
M <- cor_view$r
p_mat <- cor_view$P
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p_mat, sig.level = 0.01)

# ##############
# # Include non-parametric correlation approach (Spearman)
# ##############
# 
# rcorr(as.matrix(df_sub), type = c("pearson","spearman"))
# cor_2 <- rcorr(as.matrix(df_sub))
# cor_2
# 
# # p-values
# cor_2$P
# 
# # correlation matrix
# cor_2$r
# 
# flat_cor_mat <- function(cor_r, cor_p){
#   #This function provides a simple formatting of a correlation matrix
#   #into a table with 4 columns containing :
#   # Column 1 : row names (variable 1 for the correlation test)
#   # Column 2 : column names (variable 2 for the correlation test)
#   # Column 3 : the correlation coefficients
#   # Column 4 : the p-values of the correlations
#   library(tidyr)
#   library(tibble)
#   cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
#   cor_r <- gather(cor_r, column, cor, -1)
#   cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
#   cor_p <- gather(cor_p, column, p, -1)
#   cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
#   cor_p_matrix
# }
# 
# cor_3 <- rcorr(as.matrix(df_sub[, 1:4]))
# 
# # formatting of the correlation matrix
# my_cor_matrix <- flat_cor_mat(cor_3$r, cor_3$P)
# head(my_cor_matrix)
