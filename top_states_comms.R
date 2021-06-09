#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 2/21/2020
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
pop18_a <- population18 %>% select(States, violation_population_18 = Total.violation.population)
pop19_a <- population19 %>% select(States, violation_population_19 = Total.violation.population)
pop20_a <- population20 %>% select(States, violation_population_20 = Total.violation.population)

# merge pop data
rev_pop <- merge(pop18_a,pop19_a, by = "States")
rev_pop <- merge(rev_pop,pop20_a, by = "States")

# calculate changes
rev_pop <- rev_pop %>% mutate(# change_18_20 = violation_population_18-violation_population_20,
                              pct_18_19 = ((violation_population_19-violation_population_18)/violation_population_18),
                              pct_19_20 = ((violation_population_20-violation_population_19)/violation_population_19)
                              # pct_18_20 = ((violation_population_20-violation_population_18)/violation_population_18)*100
)

# reorder variables
rev_pop <- rev_pop %>% select(States, pct_18_19, pct_19_20, everything())

# number of states over 10% from 2019 to 2020
pop_over10pct <- rev_pop %>% filter(pct_19_20 < -.10)
# number of states over 10% from 2018 to 2019
pop_over10priorpct <- rev_pop %>% filter(pct_18_19 < -.10)
pop_over5priorpct <- rev_pop %>% filter(pct_18_19 < -.05)
pop_over2priorpct <- rev_pop %>% filter(pct_18_19 < -.02)

# top 5 states in 2019
top_5_2019 <- rev_pop %>% arrange(pct_18_19) %>% head(5)
top_5_2019 <- top_5_2019 %>% select(States, pct_18_19)

top_5_2020 <- rev_pop %>% arrange(pct_19_20) %>% head(5)
top_5_2020 <- top_5_2020 %>% select(States, pct_19_20)

# combine data
top_10_states <- cbind(top_5_2019, top_5_2020)

write.csv(top_10_states, "shared_data/top_5_pop_drops.csv")

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
adm18_a <- admissions18 %>% select(States, violation_admissions_18 = Total.violation.admissions)
adm19_a <- admissions19 %>% select(States, violation_admissions_19 = Total.violation.admissions)
adm20_a <- admissions20 %>% select(States, violation_admissions_20 = Total.violation.admissions)

# merge adm data
rev_adm <- merge(adm18_a,adm19_a, by = "States")
rev_adm <- merge(rev_adm,adm20_a, by = "States")

# calculate changes
rev_adm <- rev_adm %>% mutate(# change_18_20 = violation_admissions_18-violation_admissions_20,
  pct_18_19 = ((violation_admissions_19-violation_admissions_18)/violation_admissions_18),
  pct_19_20 = ((violation_admissions_20-violation_admissions_19)/violation_admissions_19)
  # pct_18_20 = ((violation_admissions_20-violation_admissions_18)/violation_admissions_18)*100
)

# decline
adm_decline_states <- rev_adm %>% filter(pct_19_20 < 0)
View(adm_decline_states)

# number of states over 10% from 2019 to 2020
adm_over10pct <- rev_adm %>% filter(pct_19_20 < -.10)
adm_over50pct <- rev_adm %>% filter(pct_19_20 < -.50)

# number of states over 10% from 2018 to 2019
adm_over10priorpct <- rev_adm %>% filter(pct_18_19 <= -.10)
adm_over5priorpct <- rev_adm %>% filter(pct_18_19 <= -.05)
adm_over2priorpct <- rev_adm %>% filter(pct_18_19 < -.02)

# top 5 states in 2019
top_5_2019 <- rev_adm %>% arrange(pct_18_19) %>% head(5)
top_5_2019 <- top_5_2019 %>% select(States, pct_18_19)

top_5_2020 <- rev_adm %>% arrange(pct_19_20) %>% head(5)
top_5_2020 <- top_5_2020 %>% select(States, pct_19_20)

# combine data
top_10_states <- cbind(top_5_2019, top_5_2020)

# combine state drops numbers
rev_adm <- rev_adm %>% mutate(adm_pct_18_19 = pct_18_19,
                              adm_pct_19_20 = pct_19_20) %>% select(-pct_18_19,-pct_19_20) 
rev_pop <- rev_pop %>% mutate(pop_pct_18_19 = pct_18_19,
                              pop_pct_19_20 = pct_19_20) %>% select(-pct_18_19,-pct_19_20)
rev_changes <- merge(rev_adm, rev_pop, by = "States")

# save csvs
write.csv(top_10_states, "shared_data/top_5_adm_drops.csv")
write.csv(rev_changes, "shared_data/state_rev_changes.csv")


