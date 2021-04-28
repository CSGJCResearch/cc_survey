#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load data
# source("automated_clean.R")

# read excel population data for 2018-2019
population18 <- read_xlsx("data/Data for web team 2021 v3.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v3.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v3.xlsx", sheet = "Population 2020", .name_repair = "universal")

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

# top 5 states in 2019
top_5_2019 <- rev_pop %>% arrange(pct_18_19) %>% head(5)
top_5_2019 <- top_5_2019 %>% select(States, pct_18_19)

top_5_2020 <- rev_pop %>% arrange(pct_19_20) %>% head(5)
top_5_2020 <- top_5_2020 %>% select(States, pct_19_20)

# combine data
top_10_states <- cbind(top_5_2019, top_5_2020)

write.csv(top_10_states, "test.csv")
