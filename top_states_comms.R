#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load data
source("automated_clean.R")

# read excel population data for 2018-2020
population18 <- population %>% filter(year == "2018")
population19 <- population %>% filter(year == "2019")
population20 <- population %>% filter(year == "2020")

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

# From 2019 to 2020, ____ states saw more than a 30 percent decline 
pop_over30pct <- rev_pop %>% filter(pct_19_20 < -.30)

#######
# Admissions
#######

# read excel admissions data for 2018-2020
admissions18 <- admissions %>% filter(year == "2018")
admissions19 <- admissions %>% filter(year == "2019")
admissions20 <- admissions %>% filter(year == "2020")

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

# From 2019 to 2020, ____ states saw more than a 30 percent decline 
adm_over30pct <- rev_adm %>% filter(pct_19_20 < -.30)

# From 2018 to 2019, ____ states saw more than a 10 percent decline 
adm_over10pct_18_19 <- rev_adm %>% filter(pct_18_19 < -.10)

#########
# save data for comms
#########

# rename variables for merging
rev_adm <- rev_adm %>% select(states,
                              violation_admissions_18, 
                              violation_admissions_19, 
                              violation_admissions_20,
                              adm_pct_18_19 = pct_18_19,
                              adm_pct_19_20 = pct_19_20)
rev_pop <- rev_pop %>% select(states,
                              violation_population_18, 
                              violation_population_19, 
                              violation_population_20,
                              pop_pct_18_19 = pct_18_19,
                              pop_pct_19_20 = pct_19_20)

# merge adm and pop data
adm_pop_changes <- merge(rev_adm, rev_pop, by = "states")

# add labels
var.labels = c(states = "States",
               violation_admissions_18 = "Admissions due to supervision violations in 2018",
               violation_admissions_19 = "Admissions due to supervision violations in 2019",
               violation_admissions_20 = "Admissions due to supervision violations in 2020",
               adm_pct_18_19 = "Change in admissions due to supervision violations from 2018 to 2019",
               adm_pct_19_20 = "Change in admissions due to supervision violations from 2019 to 2020",
               violation_population_18 = "Population of supervision violators in 2018",
               violation_population_19 = "Population of supervision violators in 2019",
               violation_population_20 = "Population of supervision violators in 2020",
               pop_pct_18_19 = "Change in population due to supervision violators from 2018 to 2019",
               pop_pct_19_20 = "Change in population due to supervision violators from 2019 to 2020")
label(adm_pop_changes) = as.list(var.labels[match(names(adm_pop_changes), names(var.labels))])

# save data
write.xlsx(adm_pop_changes, "shared_data/State violation changes by year 2021.xlsx")