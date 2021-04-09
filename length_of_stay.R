
# read in data
source("automated_clean.R")

# read in survey data about length of stay and costs
survey <- read_csv("data/Data for web team 2020 v6 CORRECTED/Survey 2021-Table 1.csv")

# dup for cleaning
length_of_stay <- survey %>% select(states = States,
                                    length_new_commits = `Average length of stay for new commits`,
                                    length_prob = `Average length of stay for probation`,
                                    length_parole = `Average length of stay for parole`,
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

# 