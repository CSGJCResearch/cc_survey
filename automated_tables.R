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
         "Population of technical violators" = Population.technical.violators) %>% arrange(desc(States)) %>% select(-States,-year)

# reformat adm change table
adm_table <- adm_change %>% 
  select(States, year, Overall.admissions, Admissions.supervision.violators, Admissions.technical.violators) %>%
  dplyr::rename("Overall Admissions" = Overall.admissions, 
                "Admissions of supervision violators" = Admissions.supervision.violators, 
                "Admissions of technical violators" = Admissions.technical.violators)  %>% arrange(desc(States)) 

# add adm and pop together
adm_pop_table <- cbind(adm_table, pop_table)

# reorder variables
adm_pop_table <- adm_pop_table %>% select(States,
                                          Year = year,
                                          `Overall Admissions`,
                                          `Admissions of supervision violators`,
                                          `Admissions of technical violators`,
                                          `Overall Population`,
                                          `Population of supervision violators`,
                                          `Population of technical violators`)

# reformat numbers
adm_pop_table$`Overall Admissions` = paste(round(adm_pop_table$`Overall Admissions`, 2), "%", sep="")
adm_pop_table$`Admissions of supervision violators` = paste(round(adm_pop_table$`Admissions of supervision violators`, 2), "%", sep="")
adm_pop_table$`Admissions of technical violators` = paste(round(adm_pop_table$`Admissions of technical violators`, 2), "%", sep="")
adm_pop_table$`Overall Population` = paste(round(adm_pop_table$`Overall Population`, 2), "%", sep="")
adm_pop_table$`Population of supervision violators` = paste(round(adm_pop_table$`Population of supervision violators`,2), "%", sep="")
adm_pop_table$`Population of technical violators` = paste(round(adm_pop_table$`Population of technical violators`, 2), "%", sep="")

adm_pop_table[adm_pop_table == "NA%"] <- "No Data"  

# custom generate pop table function
generate_table <- function(df, myvar){
  df1 <- df %>% filter(States == myvar)
  # kable(df1)
}

# loop through states var, create plots & store them in a list
table_list <- unique(adm_pop_table$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ generate_table(adm_pop_table, .x))

list2env(table_list, envir = .GlobalEnv)

# remove state names
Alabama <- Alabama %>% select(-States)
Alaska <- Alaska %>% select(-States)
Arizona <- Arizona %>% select(-States)
Arkansas <- Arkansas %>% select(-States)
California <- California %>% select(-States)











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



