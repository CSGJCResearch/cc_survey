#######################################
# Confined and Costly Survey
# Tables for Automated CC Study Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load R file which cleans data
source("automated_clean.R")

# notes
# https://stackoverflow.com/questions/17717323/align-two-data-frames-next-to-each-other-with-knitr

##################
# POP AND ADM
##################

# reformat adm change table
adm_change$Total.violation.admissions.pct = paste(round(adm_change$Total.violation.admissions.pct, 2), "%", sep="")
adm_table <- adm_change %>%
  select(States, year, Total.violation.admissions.pct) %>%
  dplyr::rename("Admissions for Supervision Violation" = Total.violation.admissions.pct, 
                Year = year) %>% arrange(desc(States))
adm_table[adm_table == "NA%"] <- "No Data"  

# reformat pop change table
pop_change$Total.violation.population.pct <- as.numeric(pop_change$Total.violation.population.pct)
pop_change$Total.violation.population.pct = paste(round(pop_change$Total.violation.population.pct, 2), "%", sep="")

pop_table <- pop_change %>%
  select(States, year, Total.violation.population.pct) %>%
  dplyr::rename("Population of Supervision Violators" = Total.violation.population.pct,
                Year = year) %>% arrange(desc(States)) 
pop_table[pop_table == "NA%"] <- "No Data"  

# add adm and pop together
adm_pop_table <- cbind(adm_table, pop_table)
adm_pop_table <- adm_pop_table[-c(4:5)] # remove extra State and Year columns
adm_pop_table <- adm_pop_table %>% select(States = `States...1`,
                                          Year = `Year...2`,
                                          `Admissions for Supervision Violation`,
                                          `Population of Supervision Violators`)

# custom generate pop table function
generate_table <- function(df, myvar){
  df_myvar <- df %>% filter(States == myvar)
  df_long <- df_myvar %>% pivot_longer(cols = -c(States,Year), 
                                       names_to = "category", values_to = "count")
  df_long <- cast(df_long, States+category~Year)
  df_long <- df_long %>% select(-`2018`) # remove 2018 year
  df_long <- df_long %>% mutate(`Change from Previous Year` = category) %>% select(-category)
  df_long <- df_long %>% select(States,`Change from Previous Year`, everything())
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

expenditures$`DOC Budget` <- as.character(expenditures$`DOC Budget`)
expenditures$Year <- factor(expenditures$Year)
expenditures$States <- factor(expenditures$States)
str(expenditures)

generate_cost_table <- function(df, myvar){
  df_myvar <- df %>% filter(States == myvar)
  df_long <- df_myvar %>% pivot_longer(cols = -c(States,Year), 
                                       names_to = "category", values_to = "count")
  df_long <- cast(df_long, States+category~Year)
  df_long$Expenditures <- df_long$category
  df_long <- df_long %>% select(-category)
  df_long <- df_long %>% select(States, Expenditures, everything())
}

# # custom generate cost table function
# generate_cost_table <- function(df, myvar){
#   df_myvar <- df %>% filter(States == myvar)
#   df_long <- df_myvar %>% pivot_longer(cols = -c(States,Year), 
#                                        names_to = "category", values_to = "count")
#   df_long <- cast(df_long, States+category~Year)
#   df_long$Expenditures <- df_long$category
#   df_long <- df_long %>% select(-category)
#   # df_long <- df_long %>% select(States, Expenditures, `2019`, `2020`)
# }

# loop through states var, create plots & store them in a list
table_list1 <- unique(expenditures$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ generate_cost_table(expenditures, .x))

list2env(table_list1, envir = .GlobalEnv)

# remove state names
Alabama_Expenditures <- Alabama_Expenditures %>% select(-States)
Alaska_Expenditures <- Alaska_Expenditures %>% select(-States)
Arizona_Expenditures <- Arizona_Expenditures %>% select(-States)
Arkansas_Expenditures <- Arkansas_Expenditures %>% select(-States)
California_Expenditures <- California_Expenditures %>% select(-States)
Colorado_Expenditures <- Colorado_Expenditures %>% select(-States)
Connecticut_Expenditures <- Connecticut_Expenditures %>% select(-States)
Delaware_Expenditures <- Delaware_Expenditures %>% select(-States)
Florida_Expenditures <- Florida_Expenditures %>% select(-States)
Georgia_Expenditures <- Georgia_Expenditures %>% select(-States)
Hawaii_Expenditures <- Hawaii_Expenditures %>% select(-States)
Idaho_Expenditures	 <- Idaho_Expenditures %>% select(-States)
Illinois_Expenditures <- Illinois_Expenditures %>% select(-States)
Indiana_Expenditures <- Indiana_Expenditures %>% select(-States)
Iowa_Expenditures <- Iowa_Expenditures %>% select(-States)
Kansas_Expenditures <- Kansas_Expenditures %>% select(-States)
Kentucky_Expenditures <- Kentucky_Expenditures %>% select(-States)
Louisiana_Expenditures <- Louisiana_Expenditures %>% select(-States)
Maine_Expenditures <- Maine_Expenditures %>% select(-States)
Maryland_Expenditures <- Maryland_Expenditures %>% select(-States)
Massachusetts_Expenditures <- Massachusetts_Expenditures %>% select(-States)
Michigan_Expenditures <- Michigan_Expenditures %>% select(-States)
Minnesota_Expenditures <- Minnesota_Expenditures %>% select(-States)
Mississippi_Expenditures <- Mississippi_Expenditures %>% select(-States)
Missouri_Expenditures <- Missouri_Expenditures %>% select(-States)
Montana_Expenditures <- Montana_Expenditures %>% select(-States)
Nebraska_Expenditures <- Nebraska_Expenditures %>% select(-States)
Nevada_Expenditures <- Nevada_Expenditures %>% select(-States)
`New Hampshire_Expenditures` <- `New Hampshire_Expenditures` %>% select(-States)
`New Jersey_Expenditures` <- `New Jersey_Expenditures` %>% select(-States)
`New Mexico_Expenditures` <- `New Mexico_Expenditures` %>% select(-States)
`New York_Expenditures` <- `New York_Expenditures` %>% select(-States)
`North Carolina_Expenditures` <- `North Carolina_Expenditures` %>% select(-States)
`North Dakota_Expenditures` <- `North Dakota_Expenditures` %>% select(-States)
Ohio_Expenditures <- Ohio_Expenditures %>% select(-States)
Oklahoma_Expenditures <- Oklahoma_Expenditures %>% select(-States)
Oregon_Expenditures <- Oregon_Expenditures %>% select(-States)
Pennsylvania_Expenditures <- Pennsylvania_Expenditures %>% select(-States)
`Rhode Island_Expenditures` <- `Rhode Island_Expenditures` %>% select(-States)
`South Carolina_Expenditures` <- `South Carolina_Expenditures` %>% select(-States)
`South Dakota_Expenditures` <- `South Dakota_Expenditures` %>% select(-States)
Tennessee_Expenditures <- Tennessee_Expenditures %>% select(-States)
Texas_Expenditures <- Texas_Expenditures %>% select(-States)
Utah_Expenditures <- Utah_Expenditures %>% select(-States)
Vermont_Expenditures <- Vermont_Expenditures %>% select(-States)
Virginia_Expenditures <- Virginia_Expenditures %>% select(-States)
Washington_Expenditures <- Washington_Expenditures %>% select(-States)
`West Virginia_Expenditures` <- `West Virginia_Expenditures` %>% select(-States)
Wisconsin_Expenditures <- Wisconsin_Expenditures %>% select(-States)
Wyoming_Expenditures <- Wyoming_Expenditures %>% select(-States)


###################################
# Save tables as images
###################################

# library(kableExtra)
# t1 <- knitr::kables(list(kable(Alabama) %>% kable_styling(),kable(Alabama_Expenditures) %>% kable_styling())) %>% kable_styling()
# save_kable(t1, file = "Alabama.png")







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



