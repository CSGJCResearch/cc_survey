##################
# Budgets
##################

# get 2019 budget data
budget <- read.csv("data/NASBO - State Corrections Expenditures - 1991 to current.csv")
budget <- budget %>% filter(Year == "2019" | Year == "2020") %>% select(States = State, Year, Budget = `Corrections....Total.Funds`)
budget <- budget %>% filter(States != "Puerto Rico")

# get 2020 budget data
budget_2020 <- read.csv("data/budget_2020.csv")
budget_2020$Year <- "2020"
budget_2020 <- budget_2020 %>% select(States, Year, Budget = budget)

# combine data
budget <- rbind(budget, budget_2020) 

##################
# 2021 Reported Costs
##################

# get cost data for 2021
costs <- read.csv("data/Data for web team 2020 v6 CORRECTED/Survey 2021-Table 1.csv")

# select general cost info
costs <- costs %>% select(States, Cost)

# remove dollar sign
costs$Cost = as.numeric(gsub("\\$", "", costs$Cost))

# remove NAs
costs <- costs[complete.cases(costs), ]

# add year 
costs$year = "2020"

##################
# 2019 Reported Costs
##################

# get cost data
costs2020 <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs2020 <- costs2020 %>% select(`State Abbrev`, States, Cost = `State Reported CostPerDay`)

# costs_adm <- adm_long %>%
#   filter(category == "Total.violation.admissions"|
#          category == "Technical.probation.violation.admissions"|
#          category == "Technical.parole.violation.admissions") %>% select(States, year, category, count)

# creat costs_pop df
costs_pop <- population %>% select(States, year, Total.violation.population, Technical.probation.violation.population, Technical.parole.violation.population)

# add technical prob and parole together to get tech number
costs_pop <- costs_pop %>% mutate(total_population = Total.violation.population,
                                  technical_population = Technical.probation.violation.population + Technical.parole.violation.population) %>%
  select(-Technical.probation.violation.population,
         -Technical.parole.violation.population,
         -Total.violation.population)

