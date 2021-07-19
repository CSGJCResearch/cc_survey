#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load data
# source("automated_clean.R")

# read excel population data for 2018-2020
population18 <- read_xlsx("data/Data for web team 2021 v9.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v9.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v9.xlsx", sheet = "Population 2020", .name_repair = "universal")

# rename variables
pop18_a <- population18 %>% select(states, violation_population_18 = total_violation_population)
pop19_a <- population19 %>% select(states, violation_population_19 = total_violation_population)
pop20_a <- population20 %>% select(states, violation_population_20 = total_violation_population)

# merge pop data
rev_pop <- merge(pop18_a,pop19_a, by = "states")
rev_pop <- merge(rev_pop,pop20_a, by = "states")

# change data types
rev_pop$violation_population_18 <- as.numeric(rev_pop$violation_population_18)
rev_pop$violation_population_19 <- as.numeric(rev_pop$violation_population_19)
rev_pop$violation_population_20 <- as.numeric(rev_pop$violation_population_20)

# calculate changes
rev_pop <- rev_pop %>% mutate(# change_18_20 = violation_population_18-violation_population_20,
                              pct_18_19 = ((violation_population_19-violation_population_18)/violation_population_18),
                              pct_19_20 = ((violation_population_20-violation_population_19)/violation_population_19)
                              # pct_18_20 = ((violation_population_20-violation_population_18)/violation_population_18)*100
)

# reorder variables
rev_pop <- rev_pop %>% select(states, pct_18_19, pct_19_20, everything())

# pct change 2018-2019
rev_pop <- rev_pop %>% arrange(pct_18_19)
rev_pop <- rev_pop %>% mutate(pct_18_19_1 = pct_18_19*100)
rev_pop$pct_18_19_1 <- round(rev_pop$pct_18_19_1,1)
rev_pop <- rev_pop %>% arrange(pct_18_19_1)

# pct change 2019-2020
rev_pop <- rev_pop %>% mutate(pct_19_20_1 = pct_19_20*100)
rev_pop$pct_19_20_1 <- round(rev_pop$pct_19_20_1,1)
rev_pop <- rev_pop %>% arrange(pct_19_20_1)

# save data
rev_pop1 <- rev_pop %>% select(states, `Violation Pop Change from 2018 to 2019`=pct_18_19_1, `Violation Pop Change from 2019 to 2020`=pct_19_20_1)
# write.csv(rev_pop1, "shared_data/rev_pop.csv")

# From 2019 to 2020, ? states saw more than a 35 percent decline 
pop_over35pct <- rev_pop %>% filter(pct_19_20 < -.35)
dim(pop_over35pct)

# ? states saw more than a 10 percent decline before the pandemic
pop_over10priorpct <- rev_pop %>% filter(pct_18_19 < -.10)
dim(pop_over10priorpct)

# top 5 states in 2019
top_5_2019 <- rev_pop %>% arrange(pct_18_19) %>% head(5)
top_5_2019 <- top_5_2019 %>% select(states, pct_18_19)

# top 5 states in 2020
top_5_2020 <- rev_pop %>% arrange(pct_19_20) %>% head(5)
top_5_2020 <- top_5_2020 %>% select(states, pct_19_20)

# combine data
top_10_states <- cbind(top_5_2019, top_5_2020)

# save data
write.csv(top_10_states, "shared_data/Top 5 states population drops.csv")

#######
# Admissions
#######

# read excel admissions data for 2018-2020
admissions18 <- read_xlsx("data/Data for web team 2021 v9.xlsx", sheet = "Admissions 2018", .name_repair = "universal")
admissions19 <- read_xlsx("data/Data for web team 2021 v9.xlsx", sheet = "Admissions 2019", .name_repair = "universal")
admissions20 <- read_xlsx("data/Data for web team 2021 v9.xlsx", sheet = "Admissions 2020", .name_repair = "universal")

# rename variables
adm18_a <- admissions18 %>% select(states, violation_admissions_18 = total_violation_admissions)
adm19_a <- admissions19 %>% select(states, violation_admissions_19 = total_violation_admissions)
adm20_a <- admissions20 %>% select(states, violation_admissions_20 = total_violation_admissions)

# merge adm data
rev_adm <- merge(adm18_a,adm19_a, by = "states")
rev_adm <- merge(rev_adm,adm20_a, by = "states")

# change data types
rev_adm$violation_admissions_18 <- as.numeric(rev_adm$violation_admissions_18)
rev_adm$violation_admissions_19 <- as.numeric(rev_adm$violation_admissions_19)
rev_adm$violation_admissions_20 <- as.numeric(rev_adm$violation_admissions_20)

# calculate changes
rev_adm <- rev_adm %>% mutate(# change_18_20 = violation_admissions_18-violation_admissions_20,
  pct_18_19 = ((violation_admissions_19-violation_admissions_18)/violation_admissions_18),
  pct_19_20 = ((violation_admissions_20-violation_admissions_19)/violation_admissions_19)
  # pct_18_20 = ((violation_admissions_20-violation_admissions_18)/violation_admissions_18)*100
)

# pct change 2018-2019
rev_adm <- rev_adm %>% arrange(pct_18_19)
rev_adm <- rev_adm %>% mutate(pct_18_19_1 = pct_18_19*100)
rev_adm$pct_18_19_1 <- round(rev_adm$pct_18_19_1,1)
rev_adm <- rev_adm %>% arrange(pct_18_19_1)

# pct change 2019-2020
rev_adm <- rev_adm %>% mutate(pct_19_20_1 = pct_19_20*100)
rev_adm$pct_19_20_1 <- round(rev_adm$pct_19_20_1,1)
rev_adm <- rev_adm %>% arrange(pct_19_20_1)

# save data
rev_adm1 <- rev_adm %>% select(states, 
                               `Violation Adm Change from 2018 to 2019`=pct_18_19_1, 
                               `Violation Adm Change from 2019 to 2020`=pct_19_20_1)
# write.csv(rev_adm1, "shared_data/rev_adm.csv")

# all states with a decline
adm_decline_states <- rev_adm %>% filter(pct_19_20 < 0)

# ? states saw more than a 25 percent decline in state prison populations
adm_over25pct <- rev_adm %>% filter(pct_19_20 < -.25)
dim(adm_over10pct)

# top 5 states in 2019
top_5_2019 <- rev_adm %>% arrange(pct_18_19) %>% head(5)
top_5_2019 <- top_5_2019 %>% select(states, pct_18_19)

# top 5 states in 2020
top_5_2020 <- rev_adm %>% arrange(pct_19_20) %>% head(5)
top_5_2020 <- top_5_2020 %>% select(states, pct_19_20)

# combine data
top_10_states <- cbind(top_5_2019, top_5_2020)

# combine state drops numbers
rev_adm <- rev_adm %>% mutate(adm_pct_18_19 = pct_18_19,
                              adm_pct_19_20 = pct_19_20) %>% select(-pct_18_19,-pct_19_20) 
rev_pop <- rev_pop %>% mutate(pop_pct_18_19 = pct_18_19,
                              pop_pct_19_20 = pct_19_20) %>% select(-pct_18_19,-pct_19_20)
rev_changes <- merge(rev_adm, rev_pop, by = "states")

# save csvs
write.csv(top_10_states, "shared_data/Top 5 states admissions drops.csv")
write.csv(rev_changes, "shared_data/State violation changes by year.csv")
