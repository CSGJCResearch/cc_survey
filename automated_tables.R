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
  select(States, Overall.population, Population.supervision.violators, Population.technical.violators) %>%
  dplyr::rename("Overall Population" = Overall.population, 
         "Population of supervision violators" = Population.supervision.violators, 
         "Population of technical violator" = Population.technical.violators)

# custom generate pop table function
generate_pop_table <- function(df, myvar){
  df1 <- df %>% filter(States == myvar)
  # kable(df1)
  df1
}

# loop through states var, create plots & store them in a list
pop_table_list <- unique(pop_table$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ generate_pop_table(pop_table, .x))

# save all tables to PNG files
# purrr::iwalk(pop_table_list,
#              ~ kableExtra::save_kable(filename = paste0("pop_change_", .y, ".png"))
# )

# my_list = split(pop_table, f = pop_table$States)
# my_list_df=lapply(my_list,function(x)as.data.frame(x))

list_a <- purrr::map(pop_table_list, tibble::as_tibble)
list2env(list_a, envir = .GlobalEnv)
