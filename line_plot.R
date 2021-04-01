
# read in data
source("automated_clean.R")

# import fonts
font_import()
loadfonts(quiet = TRUE)

# custom theme
theme_black = function(base_size = 12,base_family = "") {
  
  theme_grey() %+replace%
    
    theme(
      # specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "gray", lineheight = 0.9,face="bold"),  
      axis.text.y = element_text(size = base_size*0.8, color = "gray", lineheight = 0.9,face="bold"),  
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(colour = "gray", size = 0.3, linetype = "solid"),
      #axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      #axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      # specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "black"),  
      panel.grid.major = element_line(color = "black"),  
      panel.grid.minor = element_line(color = "black"),  
      #panel.margin = unit(0.5, "lines"),   
      # specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "#c8c8c8",face="bold"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

############################
# data cleaning and prep for plotting
############################

# factor, change to numeric, etc.
line_data <- pop_change
line_data <- line_data %>% select(states = States, year, total_population = Total.population, 
                                  change_17_18 = Total.population.pct)
line_data$states <- factor(line_data$states)
line_data$year <- factor(line_data$year)
line_data$total_population <- as.numeric(line_data$total_population)
line_data$change_17_18 <- as.numeric(line_data$change_17_18)

# remove states with NA in total pop
df <- line_data %>%
  group_by(states) %>%
  mutate(ind = sum(is.na(total_population))) %>% 
  group_by(states) %>%
  filter(!any(ind >=1)) %>%
  select(-ind)

# add 100% starting point, replace NAs with 100 for 2018
df$change_17_18[is.na(df$change_17_18)] <- 100

# calculate change from 100 baseline
df <- df %>% group_by(states) %>% 
  mutate(change_18_19 = (change_17_18 + lag(change_17_18)))
  # mutate(total_pop_line_change = (change_17_18 + dplyr::lag(change_17_18)))
# df$change_18_19[is.na(df$change_18_19)] <- 100

# sep data (only way to do this that i can figure out)
df_18 <- df %>% filter(year == 2018)
df_19_20 <- df %>% filter(year != 2018)
df_19_20 <- df_19_20 %>% group_by(states) %>% 
  mutate(change_19_20 = (change_18_19 + lag(change_18_19)))

# select variables to prepare for merging
df_20 <- df_19_20 %>% filter(year == 2020) %>% select(states, year, total_population, pop_change = change_19_20)
df_19 <- df_19_20 %>% filter(year == 2019) %>% select(states, year, total_population, pop_change = change_18_19)
df_18 <- df_18 %>% select(states, year, total_population, pop_change = change_17_18)

# add data back together
df_final <- rbind(df_20,df_19,df_18)

# line_plot <- ggplot(data=df_final) +
#   geom_line(aes(x=year, y=pop_change, group=states),alpha = 0.25) + theme_bw() 
# line_plot

# change year categories for plotting purposes
df_final <- df_final %>% mutate(year_plot = ifelse(year==2018,0,ifelse(year==2019,1,2)))



############################
# plot
############################

line_plot <- ggplot(data=df_final, aes(x=year_plot, y=pop_change, group=states)) +
  # lines
  geom_line(alpha = .6,color="#696969",size=.8) + theme_dark() + 
  stat_summary(aes(y = pop_change,group=1), fun=mean, 
               colour="#f89c1b", geom="line",group=1,size=.8) + 
  # axis
  scale_x_continuous(breaks=seq(0,2,1),labels = c("2018","2019","2020")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  # titles
  ggtitle("State Prison Populations Are Declining")

# add custom theme
line_plot + theme_black() + theme(text = element_text(family = "HK Grotesk"))

