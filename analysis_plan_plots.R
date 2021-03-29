#######################################
# Confined and Costly Survey
# Plots for CC Study Proposed by JS
# by Mari Roberts
# 2/21/2020
#######################################

# read in data
# source("automated_clean.R")

# tables for review
adm_change_comms_2019 <- adm_change %>% select(States, year, Total.admissions, Total.violation.admissions, Total.admissions.pct, Total.violation.admissions.pct) %>% filter(year == 2019)
adm_change_comms_2020 <- adm_change %>% select(States, year, Total.admissions, Total.violation.admissions, Total.admissions.pct, Total.violation.admissions.pct) %>% filter(year == 2020)
adm_change_comms <- rbind(adm_change_comms_2019, adm_change_comms_2020)

# tables for review
pop_change_comms_2019 <- pop_change %>% select(States, year, Total.population, Total.violation.population, Total.population.pct, Total.violation.population.pct) %>% filter(year == 2019)
pop_change_comms_2020 <- pop_change %>% select(States, year, Total.population, Total.violation.population, Total.population.pct, Total.violation.population.pct) %>% filter(year == 2020)
pop_change_comms <- rbind(pop_change_comms_2019, pop_change_comms_2020)

adm_pop_change_comms <- merge(adm_change_comms, pop_change_comms, by = c("States","year"),all.x = TRUE, all.y = TRUE)

write.csv(adm_pop_change_comms, "cc_study_adm_pop_change_comms.csv")
  
##########################################################################
##########################################################################
# POPULATION ANALYSIS
##########################################################################
##########################################################################

#####################################
# Create 4 graphs with changes in overall population, grouped by size
#####################################

# 1. states experienced increases in population over 2%
# 2. states experienced no changes in population between -2% and +2%
# 3. states experienced decreases in population between 2% and 10%
# 4. states experienced decreases in population over 10%

#####################################
# 1. states experienced increases in population over 2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.population.pct > 2)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.population.pct <- round(df$Total.population.pct ,1)

# call custom functon for plot
plot1 <- total_pop_plot_2019(df)
plot1

# 2019 - 2020
# NO STATES FIT THIS CATEGORY

# # filter 
# df_2020 <- pop_change %>% filter(year == 2020)
# df_2020 <- df_2020 %>% filter(Total.population.pct > 2)
# # get 2019 pop info for states with increases over 2% in 2020
# df_2019 <- pop_change %>% filter(year == 2019)
# # subset to states in df_2020
# df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
# df <- rbind(df_2019, df_2020)
# df$Total.population.pct <- round(df$Total.population.pct ,1)
# 
# # call custom functon for plot
# plot1 <- total_pop_plot_2020(df)
# plot1

#####################################
# 2. states experienced no changes in population between -2% and +2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.population.pct <= 2 & Total.population.pct >= -2)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.population.pct <- round(df$Total.population.pct ,1)

# split data in half because graph is too large, create two graphs
high_values <- df_2019 %>% arrange(-Total.population) %>% head(9)
df_high <- df %>% filter(States %in% high_values$States)
low_values <- df_2019 %>% arrange(Total.population) %>% head(9)
df_low <- df %>% filter(States %in% low_values$States)

plot1 <- total_pop_plot_2019(df_high)
plot1
plot1 <- total_pop_plot_2019(df_low)
plot1 

# 2019 - 2020

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.population.pct <= 2 & Total.population.pct >= -2)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.population.pct <- round(df$Total.population.pct ,1)

# split data in half because graph is too large, create two graphs
# high_values <- df_2020 %>% arrange(-Total.population) %>% head(9)
# df_high <- df %>% filter(States %in% high_values$States)
# low_values <- df_2020 %>% arrange(Total.population) %>% head(9)
# df_low <- df %>% filter(States %in% low_values$States)

plot1 <- total_pop_plot_2020(df)
plot1

#####################################
# 3. states experienced decreases in population between 2% and 10%
#####################################

# 2018 - 2019

# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.population.pct <= -2 & Total.population.pct >= -10)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.population.pct <- round(df$Total.population.pct ,1)

# split data in half because graph is too large, create two graphs
high_values <- df_2019 %>% arrange(-Total.population) %>% head(8)
df_high <- df %>% filter(States %in% high_values$States)
low_values <- df_2019 %>% arrange(Total.population) %>% head(7)
df_low <- df %>% filter(States %in% low_values$States)

# just Alaska
plot1 <- total_pop_plot_2019(df_high)
plot1
plot1 <- total_pop_plot_2019(df_low)
plot1 

# 2019 - 2020

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.population.pct <= -2 & Total.population.pct >= -10)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.population.pct <- round(df$Total.population.pct ,1)

plot1 <- total_pop_plot_2020(df)
plot1

#####################################
# 4. states experienced decreases in population over 10%
#####################################

# 2018 - 2019
# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.population.pct <= -10)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.population.pct <- round(df$Total.population.pct ,1)

plot1 <- total_pop_plot_2019(df)
plot1

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.population.pct <= -10)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.population.pct <- round(df$Total.population.pct ,1)

# 
temp <- df %>% filter(year == 2019) %>% arrange(-Total.population)

df$Total.population <- round(df$Total.population ,0)

# split data because graph is too large, create three graphs
df_high <- df %>% filter(States == "Texas" | States == "California" | States == "Florida" | 
                           States == "Pennsylvania" | States == "Arizona" | States == "Illinois" | 
                           States == "Louisiana" | States == "North Carolina" | States == "Missouri")
df_med <- df %>% filter(States == "Oklahoma" | States == "Wisconsin" | States == "Colorado" | 
                          States == "Washington" | States == "South Carolina" | States == "Arkansas" | 
                          States == "Oregon" | States == "Nevada" | States == "Connecticut" | States == "Kansas")
df_low <- df %>% filter(States == "Hawaii" | States == "Idaho" | States == "West Virginia" | 
                          States == "Delaware" | States == "South Dakota" | States == "Montana" | 
                          States == "New Hampshire" | States == "Maine" | States == "Rhode Island" |
                          States == "North Dakota" | States == "Vermont")


  
plot1 <- total_pop_plot_2020(df_high)
plot1
plot1 <- total_pop_plot_2020(df_med)
plot1 
plot1 <- total_pop_plot_2020(df_low)
plot1 

# issue with df_med - is rounding for some reason - bug
df_med$Total.population <- round(df_med$Total.population,0)

plot1 <- ggplot(df_med, aes(x=reorder(States, -Total.population), y=Total.population, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = round(Total.population,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  # ggtitle("States With Increases in Population Over 2%") + 
  theme_bw()
plot1 <- plot1 + theme_csgjc + 
  geom_label(data = df_med, vjust = 1,
             mapping = aes(label = ifelse(Total.population.pct>0,paste0("", round(Total.population.pct,1),"%"),
                                          paste0("",Total.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
  geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
plot1

#####################################
# Create 4 graphs with changes in violation population, grouped by size
#####################################

# Create 4 graphs with changes in violation population, grouped by size of the change using the suggested cut offs and graph titles (or something similar):

# 1. states experienced increases in violation population over 2%
# 2. states experienced no changes in violation population between -2% and +2%
# 3. states experienced decreases in violation population between 2% and 10%
# 4. states experienced decreases in violation population over 10%

#####################################
# 1. states experienced increases in violation population over 2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.population.pct > 2)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)
df$Total.violation.population <- round(df$Total.violation.population, 1)

# call custom functon for plot
plot1 <- total_viol_pop_plot_2019(df)
plot1

# 2019 - 2020

# filter
df_2020 <- pop_change %>% filter(year == 2020)
df_2020 <- df_2020 %>% filter(Total.violation.population.pct > 2)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

# call custom functon for plot
plot1 <- total_viol_pop_plot_2020(df)
plot1

#####################################
# 2. states experienced no changes in violation population between -2% and +2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.population.pct <= 2 & Total.violation.population.pct >= -2)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

plot1 <- total_viol_pop_plot_2019(df)
plot1

# 2019 - 2020

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.violation.population.pct <= 2 & Total.violation.population.pct >= -2)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

plot1 <- total_viol_pop_plot_2020(df)
plot1

#####################################
# 3. states experienced decreases in violation population between 2% and 10%
#####################################

# 2018 - 2019

# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.population.pct <= -2 & Total.violation.population.pct >= -10)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

# split data in half because graph is too large, create two graphs
high_values <- df_2019 %>% arrange(-Total.violation.population) %>% head(8)
df_high <- df %>% filter(States %in% high_values$States)
low_values <- df_2019 %>% arrange(Total.violation.population) %>% head(7)
df_low <- df %>% filter(States %in% low_values$States)

plot1 <- total_viol_pop_plot_2019(df_high)
plot1
plot1 <- total_viol_pop_plot_2019(df_low)
plot1 

# 2019 - 2020

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.violation.population.pct <= -2 & Total.violation.population.pct >= -10)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

plot1 <- total_viol_pop_plot_2020(df)
plot1

#####################################
# 4. states experienced decreases in violation population over 10%
#####################################

# 2018 - 2019
# filter 
df_2019 <- pop_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.population.pct <= -10)
# get 2018 pop info for states with increases over 2% in 2019
df_2018 <- pop_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

plot1 <- total_viol_pop_plot_2019(df)
plot1

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.violation.population.pct <= -10)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.population.pct <- round(df$Total.violation.population.pct ,1)

# arrange data for graphs
temp <- df %>% filter(year==2019) %>% arrange(-Total.violation.population)

# split data in half because graph is too large, create three graphs
df_high <- df %>% filter(States == "Texas"|States == "California" | States == "Missouri" | States == "Florida" | 
                           States == "Wisconsin"| States == "North Carolina" |States == "Arizona"| 
                           States == "Louisiana"| States == "Washington"| States == "Arkansas")
df_med <- df %>% filter(States == "Pennnsylvania"| States == "Mississippi"| States == "Illinois"| 
                          States == "Colorado"| States == "Kansas"| States == "South Carolina"| 
                          States == "Nevada"| States == "Oklahoma"| States == "South Dakota")
df_low <- df %>% filter(States == "Oregon"|States == "West Virginia"| States == "Montana"| States == "Wyoming"| 
                          States == "Rhode Island"| States == "North Dakota"| 
                          States == "Delaware"| States == "Alabama"| States == "New Hampshire" | States == "Hawaii")

plot1 <- total_viol_pop_plot_2020(df_high)
plot1
plot1 <- total_viol_pop_plot_2020(df_med)
plot1 
plot1 <- total_viol_pop_plot_2020(df_low)
plot1 

# issue with df_med - is rounding for some reason - bug
df_med$Total.violation.population <- round(df_med$Total.violation.population,0)

plot1 <- ggplot(df_med, aes(x=reorder(States, -Total.violation.population), y=Total.violation.population, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = scales::comma(Total.violation.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  # ggtitle("States With Increases in Population Over 2%") + 
  theme_bw()
plot1 <- plot1 + theme_csgjc + 
  geom_label(data = df_med, vjust = 1,
             mapping = aes(label = ifelse(Total.violation.population.pct>0,paste0("", round(Total.violation.population.pct,1),"%"),
                                          paste0("",Total.violation.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
  geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
plot1

##########################################################################
##########################################################################
# ADMISSIONS ANALYSIS
##########################################################################
##########################################################################

#####################################
# Create 4 graphs with changes in overall admissions, grouped by size
#####################################

# 1. states experienced increases in admissions over 2%
# 2. states experienced no changes in admissions between -2% and +2%
# 3. states experienced decreases in admissions between 2% and 10%
# 4. states experienced decreases in admissions over 10%

#####################################
# 1. states experienced increases in admissions over 2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.admissions.pct > 2)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# call custom functon for plot
plot1 <- total_adm_plot_2019(df)
plot1

# 2019 - 2020

# # # filter 
# df_2020 <- adm_change %>% filter(year == 2020)
# df_2020 <- df_2020 %>% filter(Total.admissions.pct > 2)
# # get 2019 adm info for states with increases over 2% in 2020
# df_2019 <- adm_change %>% filter(year == 2019)
# # subset to states in df_2020
# df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
# df <- rbind(df_2019, df_2020)
# df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)
# 
# # call custom functon for plot
# plot1 <- total_adm_plot_2020(df)
# plot1

#####################################
# 2. states experienced no changes in admissions between -2% and +2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.admissions.pct <= 2 & Total.admissions.pct >= -2)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# split data in half because graph is too large, create two graphs
# high_values <- df_2019 %>% arrange(-Total.admissions) %>% head(9)
# df_high <- df %>% filter(States %in% high_values$States)
# low_values <- df_2019 %>% arrange(Total.admissions) %>% head(9)
# df_low <- df %>% filter(States %in% low_values$States)

plot1 <- total_adm_plot_2019(df)
plot1

# 2019 - 2020

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.admissions.pct <= 2 & Total.admissions.pct >= -2)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# split data in half because graph is too large, create two graphs
# high_values <- df_2020 %>% arrange(-Total.admissions) %>% head(9)
# df_high <- df %>% filter(States %in% high_values$States)
# low_values <- df_2020 %>% arrange(Total.admissions) %>% head(9)
# df_low <- df %>% filter(States %in% low_values$States)

plot1 <- total_adm_plot_2020(df)
plot1

#####################################
# 3. states experienced decreases in admissions between 2% and 10%
#####################################

# 2018 - 2019

# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.admissions.pct <= -2 & Total.admissions.pct >= -10)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# split data in half because graph is too large, create two graphs
high_values <- df_2019 %>% arrange(-Total.admissions) %>% head(11)
df_high <- df %>% filter(States %in% high_values$States)
low_values <- df_2019 %>% arrange(Total.admissions) %>% head(11)
df_low <- df %>% filter(States %in% low_values$States)

# just Alaska
plot1 <- total_adm_plot_2019(df_high)
plot1
plot1 <- total_adm_plot_2019(df_low)
plot1 

# 2019 - 2020

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.admissions.pct <= -2 & Total.admissions.pct >= -10)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

plot1 <- total_adm_plot_2020(df)
plot1

#####################################
# 4. states experienced decreases in admissions over 10%
#####################################

# 2018 - 2019
# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.admissions.pct <= -10)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

plot1 <- total_adm_plot_2019(df)
plot1

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.admissions.pct <= -10)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.admissions.pct <- round(df$Total.admissions.pct ,1)

# 
temp <- df %>% filter(year == 2019) %>% arrange(-Total.admissions)

df$Total.admissions <- round(df$Total.admissions ,0)

# split data because graph is too large, create three graphs
df_1 <- df %>% filter(States == "Texas" | States == "Alaska" | States == "California" | 
                        States == "Florida" | States == "North Carolina" | States == "Illinois" | 
                        States == "Connecticut" | States == "Georgia" | States == "Pennsylvania" | States == "Arizona")
df_2 <- df %>% filter(States == "Louisiana" | States == "Missouri" | States == "Alabama" | 
                        States == "Delaware" | States == "Colorado" | States == "Oklahoma" | 
                        States == "Arkansas" | States == "Wisconsin" | States == "Oregon" | States == "Hawaii"|States == "Mississippi")
df_3 <- df %>% filter(States == "Washington" | States == "Vermont" | States == "New Jersey" | 
                        States == "Maryland" | States == "South Carolina" | 
                        States ==  "Idaho" | States == "Kansas" |States ==  "Nevada")
df_4 <- df %>% filter(States == "South Dakota" |States ==  "Utah" |States ==  "West Virginia" | 
                        States ==  "Rhode Island" |States ==  "North Dakota" | States == "Montana" | 
                        States ==  "New Hampshire"  | States == "Wyoming"  | States == "Maine")

plot1 <- total_adm_plot_2020(df_1)
plot1
plot1 <- total_adm_plot_2020(df_2)
plot1
plot1 <- total_adm_plot_2020(df_3)
plot1
plot1 <- total_adm_plot_2020(df_4)
plot1

#####################################
# Create 4 graphs with changes in violation admissions, grouped by size
#####################################

# Create 4 graphs with changes in violation admissions, grouped by size of the change using the suggested cut offs and graph titles (or something similar):

# 1. states experienced increases in violation admissions over 2%
# 2. states experienced no changes in violation admissions between -2% and +2%
# 3. states experienced decreases in violation admissions between 2% and 10%
# 4. states experienced decreases in violation admissions over 10%

#####################################
# 1. states experienced increases in violation admissions over 2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.admissions.pct > 2)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)
df$Total.violation.admissions <- round(df$Total.violation.admissions, 1)

# call custom functon for plot
plot1 <- total_viol_adm_plot_2019(df)
plot1

# 2019 - 2020

# filter
df_2020 <- adm_change %>% filter(year == 2020)
df_2020 <- df_2020 %>% filter(Total.violation.admissions.pct > 2)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

# call custom functon for plot
plot1 <- total_viol_adm_plot_2020(df)
plot1

#####################################
# 2. states experienced no changes in violation admissions between -2% and +2%
#####################################

# 2018 - 2019 

# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.admissions.pct <= 2 & Total.violation.admissions.pct >= -2)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

plot1 <- total_viol_adm_plot_2019(df)
plot1

# 2019 - 2020

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.violation.admissions.pct <= 2 & Total.violation.admissions.pct >= -2)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

plot1 <- total_viol_adm_plot_2020(df)
plot1

#####################################
# 3. states experienced decreases in violation admissions between 2% and 10%
#####################################

# 2018 - 2019

# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.admissions.pct <= -2 & Total.violation.admissions.pct >= -10)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

# split data in half because graph is too large, create two graphs
high_values <- df_2019 %>% arrange(-Total.violation.admissions) %>% head(10)
df_high <- df %>% filter(States %in% high_values$States)
low_values <- df_2019 %>% arrange(Total.violation.admissions) %>% head(10)
df_low <- df %>% filter(States %in% low_values$States)

plot1 <- total_viol_adm_plot_2019(df_high)
plot1
plot1 <- total_viol_adm_plot_2019(df_low)
plot1 

# 2019 - 2020

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.violation.admissions.pct <= -2 & Total.violation.admissions.pct >= -10)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

plot1 <- total_viol_adm_plot_2020(df)
plot1

#####################################
# 4. states experienced decreases in violation admissions over 10%
#####################################

# 2018 - 2019
# filter 
df_2019 <- adm_change %>% filter(year == 2019) 
df_2019 <- df_2019 %>% filter(Total.violation.admissions.pct <= -10)
# get 2018 adm info for states with increases over 2% in 2019
df_2018 <- adm_change %>% filter(year == 2018)
# subset to states in df_2019
df_2018 <- df_2018 %>% filter(States %in% df_2019$States)
df <- rbind(df_2018, df_2019)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

plot1 <- total_viol_adm_plot_2019(df)
plot1

# filter 
df_2020 <- adm_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.violation.admissions.pct <= -10)
# get 2019 adm info for states with increases over 2% in 2020
df_2019 <- adm_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.violation.admissions.pct <- round(df$Total.violation.admissions.pct ,1)

# arrange data for graphs
temp <- df %>% filter(year==2019) %>% arrange(-Total.violation.admissions)

# split data in half because graph is too large, create three graphs
df_high <- df %>% filter(States == "Texas" | States == "North Carolina" | States == "California" | States == "Florida" |        
                           States == "Pennsylvania" | States == "Louisiana" | States == "Illinois" | States == "Arizona" |        
                           States == "Alabama" | States == "Wisconsin" | States == "Georgia")
df_med <- df %>% filter(States == "Idaho" | States == "Kansas" | States == "Oregon" | States == "Alaska" | States == "Mississippi" | 
                          States == "Colorado" | States == "South Dakota" | States == "Utah" | States == "Washington" | 
                          States == "Nevada" | States == "Maryland")
df_low <- df %>% filter(States == "Connecticut" | States == "Oklahoma" | States == "South Carolina" | 
                          States == "Rhode Island" | States == "New Hampshire" | States == "North Dakota" | 
                          States == "New Jersey" | States == "Wyoming" | States == "Montana" | States == "Maine")

plot1 <- total_viol_adm_plot_2020(df_high)
plot1
plot1 <- total_viol_adm_plot_2020(df_med)
plot1 
plot1 <- total_viol_adm_plot_2020(df_low)
plot1 
