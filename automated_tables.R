#######################################
# Confined and Costly Survey
# Tables for Automated CC Study Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load R file which cleans data
source("automated_clean.R")

##################
# POP AND ADM
##################

# reformat adm change table
adm_change$Violation.admissions = paste(round(adm_change$Violation.admissions, 2), "%", sep="")
adm_table <- adm_change %>%
  select(States, year, Violation.admissions) %>%
  dplyr::rename("Admissions for Supervision Violation" = Violation.admissions, 
                Year = yeary) %>% arrange(desc(States))
adm_table[adm_table == "NA%"] <- "No Data"  

##################################### ADD IN   select(States, `Change from Previous Year`,`2019`,`2020`)

# reformat pop change table
pop_change$Violation.population = paste(round(pop_change$Violation.population, 2), "%", sep="")
pop_table <- pop_change %>%
  select(States, year, Violation.population) %>%
  dplyr::rename("Population of Supervision Violators" = Violation.population,
                Year = year) %>% arrange(desc(States)) 
pop_table[pop_table == "NA%"] <- "No Data"  

# add adm and pop together
adm_pop_table <- cbind(adm_table, pop_table)
adm_pop_table <- adm_pop_table[-c(4:5)] # remove columns

# custom generate pop table function
generate_table <- function(df, myvar){
  df_myvar <- df %>% filter(States == myvar)
  df_long <- df_myvar %>% pivot_longer(cols = -c(States,Year), 
                                       names_to = "category", values_to = "count")
  df_long <- cast(df_long, States+category~Year)
  df_long <- df_long %>% select(-`2018`) # remove 2018 year
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
Colorado <- Colorado %>% select(-States)
Connecticut <- Connecticut %>% select(-States)
Delaware <- Delaware %>% select(-States)
Florida <- Florida %>% select(-States)
Georgia <- Georgia %>% select(-States)
Hawaii <- Hawaii %>% select(-States)
Idaho	 <- Idaho %>% select(-States)
Illinois <- Illinois %>% select(-States)
Indiana <- Indiana %>% select(-States)
Iowa <- Iowa %>% select(-States)
Kansas <- Kansas %>% select(-States)
Kentucky <- Kentucky %>% select(-States)
Louisiana <- Louisiana %>% select(-States)
Maine <- Maine %>% select(-States)
Maryland <- Maryland %>% select(-States)
Massachusetts <- Massachusetts %>% select(-States)
Michigan <- Michigan %>% select(-States)
Minnesota <- Minnesota %>% select(-States)
Mississippi <- Mississippi %>% select(-States)
Missouri <- Missouri %>% select(-States)
Montana <- Montana %>% select(-States)
Nebraska <- Nebraska %>% select(-States)
Nevada <- Nevada %>% select(-States)
`New Hampshire` <- `New Hampshire` %>% select(-States)
`New Jersey` <- `New Jersey` %>% select(-States)
`New Mexico` <- `New Mexico` %>% select(-States)
`New York` <- `New York` %>% select(-States)
`North Carolina` <- `North Carolina` %>% select(-States)
`North Dakota` <- `North Dakota` %>% select(-States)
Ohio <- Ohio %>% select(-States)
Oklahoma <- Oklahoma %>% select(-States)
Oregon <- Oregon %>% select(-States)
Pennsylvania <- Pennsylvania %>% select(-States)
`Rhode Island` <- `Rhode Island` %>% select(-States)
`South Carolina` <- `South Carolina` %>% select(-States)
`South Dakota` <- `South Dakota` %>% select(-States)
Tennessee <- Tennessee %>% select(-States)
Texas <- Texas %>% select(-States)
Utah <- Utah %>% select(-States)
Vermont <- Vermont %>% select(-States)
Virginia <- Virginia %>% select(-States)
Washington <- Washington %>% select(-States)
`West Virginia` <- `West Virginia` %>% select(-States)
Wisconsin <- Wisconsin %>% select(-States)
Wyoming <- Wyoming %>% select(-States)

##################
# Costs
##################











###################################
# Old Code
###################################
# # reformat pop change table
# pop_table <- pop_change %>% 
#   select(States, year, Overall.population, Population.supervision.violators, Population.technical.violators) %>%
#   dplyr::rename("Overall Population" = Overall.population, 
#          "Population of supervision violators" = Population.supervision.violators, 
#          "Population of technical violators" = Population.technical.violators) %>% arrange(desc(States)) %>% select(-States,-year)
# 
# # reformat adm change table
# adm_table <- adm_change %>% 
#   select(States, year, Overall.admissions, Admissions.supervision.violators, Admissions.technical.violators) %>%
#   dplyr::rename("Overall Admissions" = Overall.admissions, 
#                 "Admissions of supervision violators" = Admissions.supervision.violators, 
#                 "Admissions of technical violators" = Admissions.technical.violators)  %>% arrange(desc(States)) 

# # add adm and pop together
# adm_pop_table <- cbind(adm_table, pop_table)
# 
# # reorder variables
# adm_pop_table <- adm_pop_table %>% select(States,
#                                           Year = year,
#                                           `Overall Admissions`,
#                                           `Admissions of supervision violators`,
#                                           `Admissions of technical violators`,
#                                           `Overall Population`,
#                                           `Population of supervision violators`,
#                                           `Population of technical violators`)
# 
# # reformat numbers
# adm_pop_table$`Overall Admissions` = paste(round(adm_pop_table$`Overall Admissions`, 2), "%", sep="")
# adm_pop_table$`Admissions of supervision violators` = paste(round(adm_pop_table$`Admissions of supervision violators`, 2), "%", sep="")
# adm_pop_table$`Admissions of technical violators` = paste(round(adm_pop_table$`Admissions of technical violators`, 2), "%", sep="")
# adm_pop_table$`Overall Population` = paste(round(adm_pop_table$`Overall Population`, 2), "%", sep="")
# adm_pop_table$`Population of supervision violators` = paste(round(adm_pop_table$`Population of supervision violators`,2), "%", sep="")
# adm_pop_table$`Population of technical violators` = paste(round(adm_pop_table$`Population of technical violators`, 2), "%", sep="")
# 
# # change NA to "No Data"
# adm_pop_table[adm_pop_table == "NA%"] <- "No Data"  

# NOTES
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



