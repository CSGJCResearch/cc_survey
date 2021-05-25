#######################################
# Confined and Costly Survey
# by Mari Roberts
# 5/20/2021
#######################################

# load data
# source("automated_clean.R")

# read excel population data for 2018-2019
population18 <- read_xlsx("data/Data for web team 2021 v5.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v5.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v5.xlsx", sheet = "Population 2020", .name_repair = "universal")

# remove unwanted variables
population20 <- population20 %>% select(-Numbers.were.corrected.or.validated.in.the.2021.survey.)

# rename variables
pop18_a <- population18 %>% select(States, 
                                   parole_pop_18 = Total.parole.violation.population,
                                   prob_pop_18 = Total.probation.violation.population)
pop19_a <- population19 %>% select(States, 
                                   parole_pop_19 = Total.parole.violation.population,
                                   prob_pop_19 = Total.probation.violation.population)
pop20_a <- population20 %>% select(States, 
                                   parole_pop_20 = Total.parole.violation.population,
                                   prob_pop_20 = Total.probation.violation.population)

# merge pop data
rev_pop <- merge(pop18_a,pop19_a, by = "States")
rev_pop <- merge(rev_pop,pop20_a, by = "States")

# calculate changes
rev_pop <- rev_pop %>% mutate(# change_18_20 = parole_pop_18-parole_pop_20,
  parole_pop_18_19 = ((parole_pop_19-parole_pop_18)/parole_pop_18),
  parole_pop_19_20 = ((parole_pop_20-parole_pop_19)/parole_pop_19),
  prob_pop_18_19 = ((prob_pop_19-prob_pop_18)/prob_pop_18),
  prob_pop_19_20 = ((prob_pop_20-prob_pop_19)/prob_pop_19)
)

rev_pop <- rev_pop %>% filter(States == "Oklahoma" |
                                States == "Rhode Island" | 
                                States == "Missouri" | 
                                States == "Maine" | 
                                States == "Alabama" | 
                                States == "California" | 
                                States == "Connecticut" | 
                                States == "North Dakota" | 
                                States == "Illinois")


write.csv(rev_pop, "rev_pop.csv")

#######
# Admissions
#######

# read excel admissions data for 2018-2019
admissions18 <- read_xlsx("data/Data for web team 2021 v5.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
admissions19 <- read_xlsx("data/Data for web team 2021 v5.xlsx", sheet = "Admissions 2019", .name_repair = "universal")
admissions20 <- read_xlsx("data/Data for web team 2021 v5.xlsx", sheet = "Admissions 2020", .name_repair = "universal")

# remove unwanted variables
admissions20 <- admissions20 %>% select(-Numbers.were.corrected.or.validated.in.the.2021.survey.,
                                        -Admissions.Year,
                                        -Reporting.Year,
                                        -Months.Reported)

# rename variables
adm18_a <- admissions18 %>% select(States, 
                                   parole_adm_18 = Total.parole.violation.admissions,
                                   prob_adm_18 = Total.probation.violation.admissions)
adm19_a <- admissions19 %>% select(States, 
                                   parole_adm_19 = Total.parole.violation.admissions,
                                   prob_adm_19 = Total.probation.violation.admissions)
adm20_a <- admissions20 %>% select(States, 
                                   parole_adm_20 = Total.parole.violation.admissions,
                                   prob_adm_20 = Total.probation.violation.admissions)

# merge adm data
rev_adm <- merge(adm18_a,adm19_a, by = "States")
rev_adm <- merge(rev_adm,adm20_a, by = "States")

# calculate changes
rev_adm <- rev_adm %>% mutate(# change_18_20 = parole_adm_18-parole_adm_20,
  parole_adm_18_19 = ((parole_adm_19-parole_adm_18)/parole_adm_18),
  parole_adm_19_20 = ((parole_adm_20-parole_adm_19)/parole_adm_19),
  prob_adm_18_19 = ((prob_adm_19-prob_adm_18)/prob_adm_18),
  prob_adm_19_20 = ((prob_adm_20-prob_adm_19)/prob_adm_19)
)

rev_adm <- rev_adm %>% filter(States == "Oklahoma" |
                                                 States == "Rhode Island" | 
                                                 States == "Missouri" | 
                                                 States == "Maine" | 
                                                 States == "Alabama" | 
                                                 States == "California" | 
                                                 States == "Connecticut" | 
                                                 States == "North Dakota" | 
                                                 States == "Illinois")

write.csv(rev_adm, "rev_adm.csv")
