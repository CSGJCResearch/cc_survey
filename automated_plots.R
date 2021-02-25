#######################################
# Confined and Costly Survey
# Plots for Automated CC Study Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load R file which cleans data
source("automated_clean.R")

# notes
# https://stackoverflow.com/questions/41904737/label-na-values-in-geom-line
# https://stackoverflow.com/questions/26869141/conditionally-hiding-data-labels-in-ggplot2-graph

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     #axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 6),size=8,face="italic"),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(), 
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(), 
                     #panel.grid.major.y = element_blank(), 
                     #panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.position = "top",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))

# custom theme -y axis on right 
theme_csgjc2 <- theme(axis.ticks = element_blank(),
                     #axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 6),size=8,face="italic"),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(), 
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(), 
                     #panel.grid.major.y = element_blank(), 
                     #panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.position = "top",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))



##################
# Prison Admissions by Year
##################

# subset data
adm_by_year <- adm_long %>% filter(category=="Total.probation.violation.admissions"|
                                   category=="Total.parole.violation.admissions"| 
                                   category=="New.commitments") %>% select(-Totals,-Probation,-Parole)
adm_by_year <- adm_by_year %>%
  mutate(category = case_when(category == "New.commitments" ~ "New Commitments",
                              category == "Total.parole.violation.admissions" ~ "Parole",
                              category == "Total.probation.violation.admissions" ~ "Probation"))

# custom plot function
adm_by_year_plot <- function(df, myvar) {      
   ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
         geom_bar(stat = "identity", position = "stack", width = 0.65) +
         scale_y_continuous(labels = scales::comma) +
         #xlab("Year") + 
         #ylab("Count") +
         labs(subtitle = "Prison Admissions by Year") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("#6baed6", "#3182bd", "#08519c")) + 
    theme_bw() + #no_grid + 
    theme_csgjc
}

# loop through states var, create plots & store them in a list
adm_by_year_plot_list <- unique(adm_by_year$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ adm_by_year_plot(adm_by_year, .x))

# save all plots to PNG files
purrr::iwalk(adm_by_year_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("adm_by_year_", .y, ".png"),
                      width = 6, height = 4, dpi = 150)
)

##################
# Probation Violations Resulting in DOC Incarceration
##################

# subset data
adm_prob <- adm_long %>% filter(category=="New.offense.probation.violation.admissions"|
                                  category=="Technical.probation.violation.admissions") %>% 
                           select(-Totals,-Probation,-Parole)
adm_prob <- adm_prob %>%
  mutate(category = case_when(category == "New.offense.probation.violation.admissions" ~ "Non-Technical",
                              category == "Technical.probation.violation.admissions" ~ "Technical"))

# custom plot function
adm_prob_plot <- function(df, myvar) {      
  ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.65) +
    scale_y_continuous(labels = scales::comma) +
    #xlab("Year") + 
    #ylab("Count") +
    labs(subtitle = "Probation Violations Resulting in DOC Incarceration") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("#1c9099", "#67a9cf")) + 
    theme_bw() + #no_grid + 
    theme_csgjc
}

# loop through states var, create plots & store them in a list
adm_prob_plot_list <- unique(adm_prob$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ adm_prob_plot(adm_prob, .x))

# save all plots to PNG files
purrr::iwalk(adm_prob_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("adm_prob_", .y, ".png"),
                      width = 6, height = 4, dpi = 150)
)

##################
# Parole Violations Resulting in DOC Incarceration
##################

# subset data
adm_parole <- adm_long %>% filter(category=="New.offense.parole.violation.admissions"|
                                      category=="Technical.parole.violation.admissions") %>% 
  select(-Totals,-Probation,-Parole)
adm_parole <- adm_parole %>%
  mutate(category = case_when(category == "New.offense.parole.violation.admissions" ~ "Non-Technical",
                              category == "Technical.parole.violation.admissions" ~ "Technical"))

# custom plot function
adm_parole_plot <- function(df, myvar) {      
  ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.65) +
    scale_y_continuous(labels = scales::comma) +
    #xlab("Year") + 
    #ylab("Count") +
    labs(subtitle = "Parole Violations Resulting in DOC Incarceration") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("#1c9099", "#67a9cf")) + 
    theme_bw() + #no_grid + 
    theme_csgjc
}

# loop through states var, create plots & store them in a list
adm_parole_plot_list <- unique(adm_parole$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ adm_parole_plot(adm_parole, .x))

# save all plots to PNG files
purrr::iwalk(adm_parole_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("adm_parole_", .y, ".png"),
                      width = 6, height = 4, dpi = 150)
)

##################
# POP
# Prison Population by Year
##################

# subset data
pop_by_year <- pop_long %>% filter(category=="Total.probation.violation.population"|
                                     category=="Total.parole.violation.population"| 
                                     category=="New.commitments") %>% select(-Totals,-Probation,-Parole)
pop_by_year <- pop_by_year %>%
  mutate(category = case_when(category == "New.commitments" ~ "New Commitments",
                              category == "Total.parole.violation.population" ~ "Parole",
                              category == "Total.probation.violation.population" ~ "Probation"))


# custom plot function
pop_by_year_plot <- function(df, myvar) {      
  ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.65) +
    scale_y_continuous(labels = scales::comma, position = "right") +
    #xlab("Year") + 
    #ylab("Count") +
    labs(subtitle = "Prison Population by Year") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("#6baed6", "#3182bd", "#08519c")) + 
    theme_bw() + #no_grid + 
    theme_csgjc
}

# loop through states var, create plots & store them in a list
pop_by_year_plot_list <- unique(pop_by_year$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ pop_by_year_plot(pop_by_year, .x))

# save all plots to PNG files
purrr::iwalk(pop_by_year_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("pop_by_year_", .y, ".png"),
                      width = 6, height = 4, dpi = 150)
)

##################
# POP
# Probation Violations Resulting in DOC Incarceration
##################

# subset data
pop_prob <- pop_long %>% filter(category=="New.offense.probation.violation.population"|
                                    category=="Technical.probation.violation.population") %>% 
  select(-Totals,-Probation,-Parole)
pop_prob <- pop_prob %>%
  mutate(category = case_when(category == "New.offense.probation.violation.population" ~ "Non-Technical",
                              category == "Technical.probation.violation.population" ~ "Technical"))

# custom plot function
pop_prob_plot <- function(df, myvar) {      
  ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.65) +
    scale_y_continuous(labels = scales::comma, position = "right") +
    #xlab("Year") + 
    #ylab("Count") +
    labs(subtitle = "Probation Violations Resulting in DOC Incarceration") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("#1c9099", "#67a9cf")) + 
    theme_bw() + #no_grid + 
    theme_csgjc
}

# loop through states var, create plots & store them in a list
pop_prob_plot_list <- unique(pop_prob$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ pop_prob_plot(pop_prob, .x))

# save all plots to PNG files
purrr::iwalk(pop_prob_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("pop_prob_", .y, ".png"),
                      width = 6, height = 4, dpi = 150)
)

##################
# POP
# Parole Violations Resulting in DOC Incarceration
##################

# subset data
pop_parole <- pop_long %>% filter(category=="New.offense.parole.violation.population"|
                                      category=="Technical.parole.violation.population") %>% select(-Totals,-Parole,-Probation)
pop_parole <- pop_parole %>%
  mutate(category = case_when(category == "New.offense.parole.violation.population" ~ "Non-Technical",
                              category == "Technical.parole.violation.population" ~ "Technical"))

# custom plot function
pop_parole_plot <- function(df, myvar) {      
  ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.65) +
    scale_y_continuous(labels = scales::comma, position = "right") +
    #xlab("Year") + 
    #ylab("Count") +
    labs(subtitle = "Parole Violations Resulting in DOC Incarceration") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("#1c9099", "#67a9cf")) + 
    theme_bw() + #no_grid + 
    theme_csgjc
}

# loop through states var, create plots & store them in a list
pop_parole_plot_list <- unique(pop_parole$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ pop_parole_plot(pop_parole, .x))

# save all plots to PNG files
purrr::iwalk(pop_parole_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("pop_parole_", .y, ".png"),
                      width = 6, height = 4, dpi = 150)
)
