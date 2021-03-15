# load necessary packages
requiredPackages = c('dplyr',
                     'openxlsx',
                     'readr',
                     'reshape',
                     'ggplot2',
                     'readxl',
                     'tidyverse',
                     'knitr'
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

adm17 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2017-Table 1.csv")
adm18 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2018-Corrected-Table 1.csv")
adm19 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2019-Corrected-Table 1.csv")
adm20 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Admissions 2020-Corrected-Table 1.csv")

# fix adm 2020 for corrected states, this lets you know who has submitted adm data so far
statesadm20 <- adm20 %>% select(States, Corrected)
statesadm20 <- statesadm20 %>% arrange(States)
statesadm20 <- statesadm20 %>% filter(States != "NA" & States != "Count" & States != "Total")
statesadm20 <- statesadm20 %>%  filter(Corrected == "Yes")

# remove unwanted variables
adm17 <- adm17 %>% select(-X11)
adm18 <- adm18 %>% select(-Notes, -`Publicly Available Data`,-X14)
adm19 <- adm19 %>% select(-Notes, -`Publicly Available Data`,-X14)
adm20 <- adm20 %>% select(-`Admissions Year`,-`Reporting Year`,-`Months Reported`, -Notes, -X16, -X17)

# remove unwanted rows
# adm17 <- adm17[-c(51:52),] 
# adm18 <- adm18[-c(51:54),] 
# adm19 <- adm19[-c(51:54),] 
# adm20 <- adm20[-c(51:54),] 
adm17 <- adm17 %>% filter(`State Abbrev` != "NA")
adm18 <- adm18 %>% filter(`State Abbrev` != "NA")
adm19 <- adm19 %>% filter(`State Abbrev` != "NA")
adm20 <- adm20 %>% filter(`State Abbrev` != "NA")

######################
# data for Marshall
######################

# duplicate data
adm17_1 <- adm17 %>% arrange(States)
adm18_1 <- adm18 %>% arrange(States)
adm19_1 <- adm19 %>% arrange(States)
adm20_1 <- adm20 %>% arrange(States)
colnames(adm17_1) <- paste("2017", colnames(adm17), sep = " ")
colnames(adm18_1) <- paste("2018", colnames(adm18), sep = " ")
colnames(adm19_1) <- paste("2019", colnames(adm19), sep = " ")
colnames(adm20_1) <- paste("2020", colnames(adm20), sep = " ")

cc_adm <- cbind(adm17_1,adm18_1,adm19_1,adm20_1)

cc_adm <- cc_adm %>% select(State = `2017 States`,
                            `2018 Total admissions`,`2019 Total admissions`,`2020 Total admissions`,
                            `2018 Total violation admissions`,`2019 Total violation admissions`,`2020 Total violation admissions`,
                            `2018 Validated admissions` = `2018 Corrected`,`2019 Validated admissions`=`2019 Corrected`,`2020 Validated admissions`=`2020 Corrected`)

# read pop data
population17 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2017-Table 1.csv")
population18 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2018-Corrected-Table 1.csv")
population19 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2019-Corrected-Table 1.csv")
population20 <- read_csv("data/Data for web team 2020 v6 CORRECTED/Population 2020-Corrected-Table 1.csv")

# fix pop 2020 for corrected states, this lets you know who has submitted population data so far
statespop20 <- population20 %>% select(States, Corrected)
statespop20 <- statespop20 %>% arrange(States)
statespop20 <- statespop20[-c(4,13,22,29,35,48,51),] # remove duplicate rows
statespop20 <- statespop20 %>% filter(States != "NA" & States != "Count" & States != "Total")
statespop20 <- statespop20 %>% filter(Corrected == "Yes")

# remove unwanted variables
population17 <- population17 %>% select(-`Population Year`,-X12,-X13)
population18 <- population18 %>% select(-Notes,-X13, -X14)
population19 <- population19 %>% select(-Notes, -X13)
population20 <- population20 %>% select(-Notes, -X13, -X14)

# remove rows
# population17 <- population17[-c(51:52),]
# population18 <- population18[-c(51:54),] # remove empty rows
# population19 <- population19[-c(51:54),] # remove empty rows
# population20 <- population20[-c(51:52),] # remove empty rows
population17 <- population17 %>% filter(`State Abbrev` != "NA")
population18 <- population18 %>% filter(`State Abbrev` != "NA")
population19 <- population19 %>% filter(`State Abbrev` != "NA")
population20 <- population20 %>% filter(`State Abbrev` != "NA")

# issue with pop 20, some rows were duplicated with NAs - weird bug?
population20 <- population20 %>% arrange(States)
population20 <- population20[-c(4,12,21,28,35,47,50),] # remove duplicate rows

# duplicate data
population17_1 <- population17 %>% arrange(States)
population18_1 <- population18 %>% arrange(States)
population19_1 <- population19 %>% arrange(States)
population20_1 <- population20 %>% arrange(States)
colnames(population17_1) <- paste("2017", colnames(population17), sep = " ")
colnames(population18_1) <- paste("2018", colnames(population18), sep = " ")
colnames(population19_1) <- paste("2019", colnames(population19), sep = " ")
colnames(population20_1) <- paste("2020", colnames(population20), sep = " ")

cc_pop <- cbind(population17_1,population18_1,population19_1,population20_1)

cc_pop <- cc_pop %>% select(State = `2017 States`,
                                          `2018 Total population`,`2019 Total population`,`2020 Total population`,
                                          `2018 Total violation population`,`2019 Total violation population`,`2020 Total violation population`,
                                          `2018 Validated population` = `2018 Corrected`,`2019 Validated population`=`2019 Corrected`,`2020 Validated population`=`2020 Corrected`)

cc_adm_pop <- merge(cc_adm, cc_pop, by = "State")

write.csv(cc_adm_pop, "cc_adm_pop.csv")

##################
# Create CSVs
##################

# for marshall
# pop_long_marshall <- pop_long %>% filter(category == "Total.population" |
#                                          category == "Total.violation.population") %>% select(States, year, category, count)
# pop_long_marshall <- pop_long_marshall %>% mutate(type = "Population")
# 
# adm_long_marshall <- adm_long %>% filter(category == "Total.admissions" |
#                                          category == "Total.violation.admissions") %>% select(States, year, category, count)
# adm_long_marshall <- adm_long_marshall %>% mutate(type = "Admissions")
# 
# # combine adm and pop data
# adm_pop_cc_long <- rbind(pop_long_marshall,adm_long_marshall)
# 
# # write csvs and save to data folder
# write.csv(pop_long_marshall, "data/pop_long.csv")
# write.csv(adm_long_marshall, "data/adm_long.csv")
# write.csv(adm_pop_cc_long, "data/cc_adm_pop_long_all.csv")
