
# read in data
source("automated_clean.R")
source("automated_costs.R")

# read in survey data about length of stay and costs
survey <- read_xlsx("data/Data for web team 2021 v1.xlsx", sheet = "Length of Stay and Costs 2021", .name_repair = "universal")

# dup for cleaning
length_of_stay <- survey %>% select(states = States,
                                    length_new_commits = Average.length.of.stay.for.new.commits,
                                    length_prob = Average.length.of.stay.for.probation,
                                    length_parole = Average.length.of.stay.for.parole,
                                    measure = Measure,
                                    #notes = `Length of Stay Notes`
                                    )

# convert unit of measurement to days
length_of_stay <- length_of_stay %>% 
                  mutate(length_new_commits_days = case_when(measure == "Days" ~ length_new_commits,
                                                             measure == "Months" ~ length_new_commits*30.42, # avg days in month
                                                             measure == "Years" ~ length_new_commits*365),
                         length_prob_days = case_when(measure == "Days" ~ length_prob,
                                                      measure == "Months" ~ length_prob*30.42, 
                                                      measure == "Years" ~ length_prob*365),
                         length_parole_days = case_when(measure == "Days" ~ length_parole,
                                                        measure == "Months" ~ length_parole*30.42,
                                                        measure == "Years" ~ length_parole*365))

# select variables
length_of_stay <- length_of_stay %>% select(states, length_new_commits_days, length_prob_days, length_parole_days)

# remove florida
length_of_stay <- length_of_stay %>% filter(states != "Florida")

# average length of stay
length_new_commits_days <- length_of_stay %>% select(states,length_new_commits_days) 
length_new_commits_days <- na.omit(length_new_commits_days)
avg_length_new_commits_days <- mean(length_new_commits_days$length_new_commits_days)

length_prob_days <- length_of_stay %>% select(states,length_prob_days) 
length_prob_days <- na.omit(length_prob_days)
avg_length_prob_days <- mean(length_prob_days$length_prob_days)

length_parole_days <- length_of_stay %>% select(states,length_parole_days) 
length_parole_days <- na.omit(length_parole_days)
avg_length_parole_days <- mean(length_parole_days$length_parole_days)

# calculate costs
costs_length_of_stay <- costs_pop %>% select(states, cost_2020, year)
costs_length_of_stay <- merge(length_of_stay, costs_length_of_stay, by = "states", all.y = TRUE)

costs_length_of_stay <- costs_length_of_stay %>% mutate(new_commits_cost = length_new_commits_days*cost_2020,
                                                        prob_cost = length_prob_days*cost_2020,
                                                        parole_cost = length_parole_days*cost_2020)

# cost length of stay
new_commits_cost <- costs_length_of_stay %>% select(states,new_commits_cost) 
new_commits_cost <- na.omit(new_commits_cost)
cost_new_commits_cost <- mean(new_commits_cost$new_commits_cost)

prob_cost <- costs_length_of_stay %>% select(states,prob_cost) 
prob_cost <- na.omit(prob_cost)
cost_prob_cost <- mean(prob_cost$prob_cost)

parole_cost <- costs_length_of_stay %>% select(states,parole_cost) 
parole_cost <- na.omit(parole_cost)
cost_parole_cost <- mean(parole_cost$parole_cost)
