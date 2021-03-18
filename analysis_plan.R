#######################################
# Confined and Costly Survey
# Plots for CC Study Proposed by JS
# by Mari Roberts
# 2/21/2020
#######################################

# read in data
source("automated_clean.R")

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

# custom function for total population
total_pop_plot_2019 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.population), y=Total.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = scales::comma(Total.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#0db4e4", "#007392")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.population.pct>0,paste0("", round(Total.population.pct,1),"%"),
                                            paste0("",Total.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
  return(plot1)
}

# same as above but with different color for 2020
total_pop_plot_2020 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.population), y=Total.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = scales::comma(Total.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#007392", "#00475d")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.population.pct>0,paste0("", round(Total.population.pct,1),"%"),
                                            paste0("",Total.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
  return(plot1)
}


# Create 4 graphs with changes in overall population, grouped by size of the change using the suggested cut offs and graph titles (or something similar):

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
# plot2 <- total_pop_plot_2020(df)
# plot2

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

plot_3 <- total_pop_plot_2019(df_high)
plot_3
plot_4 <- total_pop_plot_2019(df_low)
plot_4 

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
high_values <- df_2020 %>% arrange(-Total.population) %>% head(9)
df_high <- df %>% filter(States %in% high_values$States)
low_values <- df_2020 %>% arrange(Total.population) %>% head(9)
df_low <- df %>% filter(States %in% low_values$States)

# just Alaska
plot_5 <- total_pop_plot_2020(df_high)
plot_5
plot_6 <- total_pop_plot_2020(df_low)
plot_6 

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
plot_7 <- total_pop_plot_2019(df_high)
plot_7
plot_8 <- total_pop_plot_2019(df_low)
plot_8 

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

plot_9 <- total_pop_plot_2020(df)
plot_9

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

plot_10 <- total_pop_plot_2019(df)
plot_10

# filter 
df_2020 <- pop_change %>% filter(year == 2020) 
df_2020 <- df_2020 %>% filter(Total.population.pct <= -10)
# get 2019 pop info for states with increases over 2% in 2020
df_2019 <- pop_change %>% filter(year == 2019)
# subset to states in df_2020
df_2019 <- df_2019 %>% filter(States %in% df_2020$States)
df <- rbind(df_2019, df_2020)
df$Total.population.pct <- round(df$Total.population.pct ,1)

# split data in half because graph is too large, create two graphs
df_high <- df %>% filter(States == "Texas" | States == "California" | States == "Florida" | 
                           States == "Pennsylvania" | States == "Arizona" | States == "Illinois" | 
                           States == "Louisiana" | States == "North Carolina" | States == "Missouri")
df_med <- df %>% filter(States == "Oklahoma" | States == "Wisconsin" | States == "Colorado" | 
                          States == "Washington" | States == "South Carolina" | States == "Arkansas" | 
                          States == "Oregon" | States == "Nevada" | States == "Connecticut")
df_low <- df %>% filter(States == "Kansas" | States == "Idaho" | States == "West Virginia" | 
                          States == "Delaware" | States == "South Dakota" | States == "Montana" | 
                          States == "New Hampshire" | States == "Maine" | States == "Rhode Island" |
                          States == "North Dakota" | States == "Vermont")

plot_11 <- total_pop_plot_2020(df_high)
plot_11
plot_12 <- total_pop_plot_2020(df_med)
plot_12 
plot_13 <- total_pop_plot_2020(df_low)
plot_13 

# issue with df_med - is rounding for some reason - bug
df_med$Total.population <- round(df_med$Total.population,0)

plot_12 <- ggplot(df_med, aes(x=reorder(States, -Total.population), y=Total.population, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = scales::comma(Total.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#007392", "#00475d")) +
  # ggtitle("States With Increases in Population Over 2%") + 
  theme_bw()
plot_12 <- plot_12 + theme_csgjc + 
  geom_label(data = df_med, vjust = 1,
             mapping = aes(label = ifelse(Total.population.pct>0,paste0("", round(Total.population.pct,1),"%"),
                                          paste0("",Total.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
  geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
plot_12









