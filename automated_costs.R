##################
# Budgets
##################

# get 2019 budget data
budget <- read.csv("data/NASBO - State Corrections Expenditures - 1991 to current.csv")
budget <- budget %>% filter(Year == "2019" | Year == "2020") %>% select(States = State, budget_2019 = `Corrections....Total.Funds`)
budget <- budget %>% filter(States != "Puerto Rico") 

# get 2020 budget data
budget_2020 <- read.csv("data/budget_2020.csv")
budget_2020$Year <- "2020"
budget_2020 <- budget_2020 %>% select(States, budget_2020 = budget)

# combine data
budget <- merge(budget, budget_2020, by = c("States"))

# change from factor to char
budget$budget_2019 <- as.character(budget$budget_2019)
budget$budget_2020 <- as.character(budget$budget_2020)

# change by millions to actual number
budget$budget_2019 = as.character(gsub("\\$", "", budget$budget_2019)) # remove $ sign
budget$budget_2020 = as.character(gsub("\\$", "", budget$budget_2020)) 
budget$budget_2019 = as.character(gsub("\\M", "", budget$budget_2019)) # remove M
budget$budget_2020 = as.character(gsub("\\M", "", budget$budget_2020)) 
budget$budget_2019 = as.character(gsub("\\,", "", budget$budget_2019)) # remove comma
budget$budget_2020 = as.character(gsub("\\,", "", budget$budget_2020)) 
budget$budget_2019 <- as.numeric(budget$budget_2019)
budget$budget_2020 <- as.numeric(budget$budget_2020)
budget$budget_2019 <- budget$budget_2019*1000000
budget$budget_2020 <- budget$budget_2020*1000000

# add commas
budget$budget_2019 <- comma(budget$budget_2019,digits = 0)
budget$budget_2020 <- comma(budget$budget_2020,digits = 0)

##################
# 2021 Reported Costs
##################

# get cost data for 2021
costs <- read.csv("data/Data for web team 2020 v6 CORRECTED/Survey 2021-Table 1.csv")

# marginal costs
marginal_costs <- costs %>% select(states = States, marginal_cost_2020 = Marginal)

# select general cost info
costs <- costs %>% select(States, Cost)

# remove dollar sign
costs$cost_2020 = as.numeric(gsub("\\$", "", costs$Cost))
costs <- costs %>% select(-Cost)
marginal_costs$marginal_cost_2020 = as.numeric(gsub("\\$", "", marginal_costs$marginal_cost_2020))

# add year for costs 2019 if NA and 2020 if not NA
costs <- costs %>% mutate(year = ifelse(is.na(cost_2020),"2019","2020"))

# get cost data from 2019
costs2019 <- read_excel("data/Cost Per Day For Calculation.xlsx")
costs2019 <- costs2019 %>% select(States, cost_2019 = `State Reported CostPerDay`)

# replace NAs in 2020 cost data with data from 2019
setDT(costs); setDT(costs2019)
costs[is.na(cost_2020), cost_2020 := costs2019[.SD, on=.(States), x.cost_2019]]
costs <- merge(costs, costs2019, by = "States")
costs$states <- costs$States
costs <- costs %>% select(-States)

# creat costs_pop df
costs_pop_2019 <- population %>% filter(year == 2019) %>% select(states = States, 
                                                                 total_pop_2019 = Total.population,
                                                                 total_viol_pop_2019 = Total.violation.population, 
                                                                 tech_prob_pop_2019 = Technical.probation.violation.population, 
                                                                 tech_parole_pop_2019 = Technical.parole.violation.population)
costs_pop_2020 <- population %>% filter(year == 2020) %>% select(states = States, 
                                                                 total_pop_2020 = Total.population,
                                                                 total_viol_pop_2020 = Total.violation.population, 
                                                                 tech_prob_pop_2020 = Technical.probation.violation.population, 
                                                                 tech_parole_pop_2020 = Technical.parole.violation.population)

# add technical prob and parole together to get tech number
costs_pop_2019 <- costs_pop_2019 %>%  mutate(tech_pop_2019 = tech_prob_pop_2019 + tech_parole_pop_2019) 
costs_pop_2020 <- costs_pop_2020 %>%  mutate(tech_pop_2020 = tech_prob_pop_2020 + tech_parole_pop_2020) 

# merge costs and population numbers
costs_pop_df <- merge(costs_pop_2019, costs, by = "states", all.x = TRUE, all.y = TRUE)
costs_pop_df <- merge(costs_pop_df, costs_pop_2020, by = "states", all.x = TRUE, all.y = TRUE)

# calc costs
costs_pop_df <- costs_pop_df %>% mutate(pop_sup_cost_2019 = total_pop_2019*cost_2019*365,
                                        pop_tech_cost_2019 = tech_pop_2019*cost_2019*365,
                                        pop_sup_cost_2020 = total_pop_2020*cost_2020*365,
                                        pop_tech_cost_2020 = tech_pop_2020*cost_2020*365) 

# select variables
costs_pop <- costs_pop_df %>% select(states,
                                     total_pop_2019,
                                     total_viol_pop_2019,
                                     tech_pop_2019,
                                     cost_2019,
                                     pop_sup_cost_2019,
                                     pop_tech_cost_2019,
                                     total_pop_2020,
                                     total_viol_pop_2020,
                                     tech_pop_2020,
                                     cost_2020,
                                     year,
                                     pop_sup_cost_2020,
                                     pop_tech_cost_2020)

# add marginal costs
costs_pop <- merge(costs_pop, marginal_costs, by = "states", all.x = TRUE, all.y = TRUE)

# calculate marginal costs 
costs_pop <- costs_pop %>% mutate(pop_sup_marginal_cost_2020 = total_pop_2020*marginal_cost_2020*365,
                                  pop_tech_marginal_cost_2020 = tech_pop_2020*marginal_cost_2020*365) 

# remove NAs for plotting the differences between cost and marginal cost
costs_pop_noNAs <- costs_pop %>% select(states,
                                        year,
                                        total_pop_2020,
                                        tech_pop_2020,
                                        cost_2020,
                                        pop_sup_cost_2020,
                                        pop_tech_cost_2020,
                                        marginal_cost_2020,
                                        pop_sup_marginal_cost_2020,
                                        pop_tech_marginal_cost_2020) 
costs_pop_noNAs <- costs_pop_noNAs %>% filter(year != 2019) # remove costs where we had to use 2019 costs when 2020 was missing
costs_pop_noNAs <- costs_pop_noNAs[complete.cases(costs_pop_noNAs),]

###########
# Plots
###########

# change data to long form for graphing
costs_long <- costs_pop_noNAs %>% a

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     #axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 10),size=10,face="bold"),
                     axis.text.y = element_text(size=10),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size=10,face="bold"),
                     legend.position = "right",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))

# custom function for total population (save lines of code)
plot1 <- ggplot(costs_pop_noNAs, aes(x=reorder(states, -pop_sup_cost_2020), y=Total.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = scales::comma(Total.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#0db4e4", "#007392")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
plot1 <- plot1 + theme_csgjc 

# # calc by millions
# costs_pop_df$pop_sup_cost <- (costs_pop_df$pop_sup_cost)/1000000
# costs_pop_df$pop_tech_cost <- (costs_pop_df$pop_tech_cost)/1000000

####################################################
# AUTOMATED TABLES
####################################################

# add comma
library(formattable)
costs_pop_df$pop_sup_cost_2019 <- scales::comma(costs_pop_df$pop_sup_cost_2019)
costs_pop_df$pop_tech_cost_2019 <- scales::comma(costs_pop_df$pop_tech_cost_2019)
costs_pop_df$pop_sup_cost_2020 <- scales::comma(costs_pop_df$pop_sup_cost_2020)
costs_pop_df$pop_tech_cost_2020 <- scales::comma(costs_pop_df$pop_tech_cost_2020)
costs_pop_df$total_pop_2019 <- comma(costs_pop_df$total_pop_2019,digits = 0)
costs_pop_df$total_pop_2020 <- comma(costs_pop_df$total_pop_2020,digits = 0)
costs_pop_df$tech_pop_2019 <- comma(costs_pop_df$tech_pop_2019,digits = 0)
costs_pop_df$tech_pop_2020 <- comma(costs_pop_df$tech_pop_2020,digits = 0)

# # round - add $ and M
# costs_pop_df$pop_sup_cost <- paste("$", round(costs_pop_df$pop_sup_cost, 1), "M", sep="")
# costs_pop_df$pop_tech_cost <- paste("$", round(costs_pop_df$pop_tech_cost, 1), "M", sep="")

# add costs and budgets together
expenditures <- merge(budget, costs_pop_df, by = c("States"))

# replace NAs with "No Data"
expenditures$pop_sup_cost_2019[is.na(expenditures$pop_sup_cost_2019)] = "Data Needed to Calculate"
expenditures$pop_tech_cost_2019[is.na(expenditures$pop_tech_cost_2019)] = "Data Needed to Calculate"
expenditures$pop_sup_cost_2020[is.na(expenditures$pop_sup_cost_2020)] = "Data Needed to Calculate"
expenditures$pop_tech_cost_2020[is.na(expenditures$pop_tech_cost_2020)] = "Data Needed to Calculate"

# rename categories
expenditures$`DOC Budget 2019` <- expenditures$budget_2019
expenditures$`DOC Budget 2020` <- expenditures$budget_2020
expenditures$`DOC Cost to Incarcerate Supervision Violators 2019` <- expenditures$pop_sup_cost_2019
expenditures$`DOC Cost to Incarcerate Technical Supervision Violators 2019` <- expenditures$pop_tech_cost_2019
expenditures$`DOC Cost to Incarcerate Supervision Violators 2020` <- expenditures$pop_sup_cost_2020
expenditures$`DOC Cost to Incarcerate Technical Supervision Violators 2020` <- expenditures$pop_tech_cost_2020

# # select variables
numbers <- expenditures %>% select(States, total_pop_2019,total_pop_2020,tech_pop_2019,tech_pop_2020)

# rename variables
numbers$`Total Population 2019` <- numbers$total_pop_2019
numbers$`Total Population 2020` <- numbers$total_pop_2020
numbers$`Technical Population 2019` <- numbers$tech_pop_2019
numbers$`Technical Population 2020` <- numbers$tech_pop_2020

# remove variables
numbers <- numbers %>% select(-tech_pop_2020,-tech_pop_2019,-total_pop_2020, -total_pop_2019)

# change from char to numeric, remove commas, round numbers
numbers$`Total Population 2019` <- as.character(gsub("\\,", "", numbers$`Total Population 2019`))
numbers$`Total Population 2020` <- as.character(gsub("\\,", "", numbers$`Total Population 2020`))
numbers$`Technical Population 2019` <- as.character(gsub("\\,", "", numbers$`Technical Population 2019`))
numbers$`Technical Population 2020` <- as.character(gsub("\\,", "", numbers$`Technical Population 2020`))

numbers$`Total Population 2019` <- as.numeric(numbers$`Total Population 2019`)
numbers$`Total Population 2020` <- as.numeric(numbers$`Total Population 2020`)
numbers$`Technical Population 2019` <- as.numeric(numbers$`Technical Population 2019`)
numbers$`Technical Population 2020` <- as.numeric(numbers$`Technical Population 2020`)

numbers$`Total Population 2019` <- round(numbers$`Total Population 2019`,0)
numbers$`Total Population 2020` <- round(numbers$`Total Population 2020`,0)
numbers$`Technical Population 2019` <- round(numbers$`Technical Population 2019`,0)
numbers$`Technical Population 2020` <- round(numbers$`Technical Population 2020`,0)

# remove variables
expenditures <- expenditures %>% select(-budget_2019, -budget_2020, -cost_2019, -cost_2020,
                                        -pop_sup_cost_2019, -pop_sup_cost_2020, -pop_tech_cost_2019, -pop_tech_cost_2020,
                                        -tech_pop_2020,-tech_pop_2019,
                                        -total_pop_2020, -total_pop_2019)

# Add expenditures to state name for automation later
expenditures$States <- paste(expenditures$States, "_Expenditures", sep="")
