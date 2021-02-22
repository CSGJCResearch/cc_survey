#######################################
# Confined and Costly Survey
# Tables for Automated CC Study Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load R file which cleans data
source("automated_clean.R")

# ADMISSIONS

# Overall Admissions
# Admissions for supervision violation
# Admissions for technical violation

# DOC Costs to incarcerate violators

# POPULATION

# Overall population
# Population of supervision violators
# Population of technical violator

# reformat pop change table
pop_table <- pop_change %>% 
  select(States, year, Overall.population, Population.supervision.violators, Population.technical.violators) %>%
  dplyr::rename("Overall Population" = Overall.population, 
         "Population of supervision violators" = Population.supervision.violators, 
         "Population of technical violator" = Population.technical.violators)

# custom generate pop table function
generate_pop_table <- function(df, myvar){
  df1 <- df %>% filter(States == myvar)
  # kable(df1)
}

# loop through states var, create plots & store them in a list
pop_table_list <- unique(pop_table$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ generate_pop_table(pop_table, .x))

list2env(pop_table_list, envir = .GlobalEnv)

# https://stackoverflow.com/questions/59169631/split-a-list-into-separate-data-frame-in-r
# save all tables to PNG files - doesn't work
# purrr::iwalk(pop_table_list,
#              ~ kableExtra::save_kable(filename = paste0("pop_change_", .y, ".png"))
# )

# # change to list from tibble
# pop_table_list <- lapply(pop_table_list, as.list)
# 
# # create dataframe for each table
# imap(pop_table_list, ~ set_names(tibble(.x), .y)) %>%
#   set_names(str_c("pop_table_", 1:50)) %>% 
#   list2env(.GlobalEnv)

