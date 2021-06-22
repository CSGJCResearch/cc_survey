
# read in data
source("automated_clean.R")
# 
# # import fonts
# font_import()
# loadfonts(quiet = TRUE)

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
      legend.background = element_rect(color = NA, fill = "#383838"),  
      legend.key = element_rect(color = "white",  fill = "#383838"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "none",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # specify panel options
      panel.background = element_rect(fill = "#383838", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "#383838"),  
      panel.grid.major = element_line(color = "#383838"),  
      panel.grid.minor = element_line(color = "#383838"),  
      #panel.margin = unit(0.5, "lines"),   
      # specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # specify plot options
      plot.background = element_rect(color = "#383838", fill = "#383838"),  
      plot.title = element_text(size = base_size*1.2, color = "#c8c8c8",face="bold"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

theme_white = function(base_size = 12,base_family = "") {
  
  theme_grey() %+replace%
    
    theme(
      # specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "636363", lineheight = 0.9,face="bold"),  
      axis.text.y = element_text(size = base_size*0.8, color = "636363", lineheight = 0.9,face="bold"),  
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(colour = "636363", size = 0.3, linetype = "solid"),
      #axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      #axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      # specify legend options
      legend.background = element_rect(color = NA, fill = "white"),  
      legend.key = element_rect(color = "white",  fill = "white"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "none",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # specify panel options
      panel.background = element_rect(fill = "white", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "white"),  
      panel.grid.minor = element_line(color = "white"),  
      #panel.margin = unit(0.5, "lines"),   
      # specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # specify plot options
      plot.background = element_rect(color = "white", fill = "white"),  
      plot.title = element_text(size = base_size*1.2, color = "#c8c8c8",face="bold"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

############################
# data cleaning and prep for plotting
############################

# dup for cleaning
line_data <- pop_change

# factor, change to numeric, etc.
line_data <- line_data %>% select(states = States, year, total_population = Total.population, 
                                  change_all = Total.population.pct)
line_data$states <- factor(line_data$states)
# line_data$year <- factor(line_data$year)
line_data$total_population <- as.numeric(line_data$total_population)
line_data$change_all <- as.numeric(line_data$change_all)

# select western states
line_data <- line_data %>% filter(states == "Washington"|
                                    states == "Oregon"|  
                                    states == "California"|  
                                    states == "Idaho"|
                                    states == "Nevada"|
                                    states == "Montana"|
                                    states == "Utah"|
                                    states == "Arizona"|
                                    states == "Wyoming"|
                                    states == "Colorado"|
                                    states == "New Mexico"|
                                    states == "Alaska" |
                                    states == "Hawaii")

# remove states with NA in total pop
df <- line_data %>%
  group_by(states) %>%
  mutate(ind = sum(is.na(total_population))) %>% 
  group_by(states) %>%
  filter(!any(ind >=1)) %>%
  select(-ind)

# if state doesn't have 2020 data, remove
states_missing <- df %>%
  group_by(states) %>%
  filter(!any(year == "2020"))

# subset to states that have 2018 and 2020 data for plot
'%!in%' <- function(x,y)!('%in%'(x,y))
df <- df %>% filter(states %!in% states_missing$states)

# calculate change from 2018 to 2020
df_18_20_change <- df %>% filter(year != 2019) %>% select(-change_all)
df_18_20_change <- df_18_20_change %>% group_by(states) %>% mutate(change_18_20 = (total_population / dplyr::lag(total_population)-1)*100)

# add 100% starting point, replace NAs with 100 for 2018
# df$pop_change[is.na(df$pop_change)] <- 100

# df_final <- df %>%
#   group_by(states) %>%
#   arrange(year, .by_group = TRUE) %>%
#   mutate(diff1 = total_population / lag(total_population-1)*100) %>% 
#   mutate(diff2 = total_population / lag(total_population, default = first(diff1)-1)*100)

# calculate pct change between 100% baseline and 2019 data
df$year <- as.numeric(df$year)
df_final <- df %>%
  group_by(states) %>%
  mutate(change_19 = total_population / lag(total_population, default = first(total_population)-1)*100)

df_final$change_19_use <- ifelse(df_final$year==2020,df_final$change_all,df_final$change_19)

df_19_20 <- df_final %>% filter(year != 2018)
df_19_20 <- df_19_20 %>%
  group_by(states) %>%
  mutate(change_20 = change_19_use + lag(change_19_use))

df_19 <- df_19_20 %>% filter(year == 2019) %>% select(states, year, total_population, change_all, pop_change = change_19)
df_20 <- df_19_20 %>% filter(year == 2020) %>% select(states, year, total_population, change_all, pop_change = change_20)
df_18 <- df_final %>% filter(year == 2018) 
df_18 <- df_18 %>% select(states,year,total_population,change_all,pop_change=change_19)
df_18$pop_change <- 100

# combine dfs back together
df_final <- rbind(df_18,df_19, df_20)

# change year categories for plotting purposes
df_final <- df_final %>% mutate(year_plot = ifelse(year==2018,0,ifelse(year==2019,1,2)))

# add state abb
df_final <- merge(df_final,state_abb, by = "states")

# test nc
# north_carolina <- df_final %>% filter(states=="North Carolina") %>% distinct()
# View(north_carolina)

############################
# line plot
############################


line_plot <- ggplot(data=df_final, aes(x=year_plot, y=pop_change, group=states)) +
  # lines
  geom_line(color="darkgray",size=.7) + theme_dark() + 
  geom_line(data = subset(df_final, states == 'Colorado'), 
            size = .8, color = blue3) +
  stat_summary(aes(y = pop_change,group=1), fun=mean, 
               colour="orange", geom="line",linetype = 'dashed',group=1,size=.8) + 
  # axis
  scale_x_continuous(breaks=seq(0,2,1),labels = c("2018","2019","2020")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) 
  # titles
  # ggtitle("State Prison Populations Are Declining") +
  # labels
  # geom_text(data = subset(df_final, year == "2020"),aes(label = state_abb),
  #           color="#c8c8c8", size=3, fontface = "bold")


line_plot + theme_csgjc_labels()

# add custom theme
# line_plot + theme_black() + theme(text = element_text(family = "HK Grotesk")) 

####
# find labels for 2018-2020 change
####

# read excel population data for 2018-2019
population18 <- read_xlsx("data/Data for web team 2021 v6.xlsx", sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx("data/Data for web team 2021 v6.xlsx", sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx("data/Data for web team 2021 v6.xlsx", sheet = "Population 2020", .name_repair = "universal")

# remove unwanted variables
population20 <- population20 %>% select(-Numbers.were.corrected.or.validated.in.the.2021.survey.)

# rename variables
pop18_a <- population18 %>% select(States, population_18 = Total.population)
pop19_a <- population19 %>% select(States, population_19 = Total.population)
pop20_a <- population20 %>% select(States, population_20 = Total.population)

# merge pop data
pop <- merge(pop18_a,pop19_a, by = "States")
pop <- merge(pop,pop20_a, by = "States")

# calculate changes
pop <- pop %>% mutate(# change_18_20 = population_18-population_20,
  pct_18_19 = ((population_19-population_18)/population_18),
  pct_19_20 = ((population_20-population_19)/population_19),
  pct_18_20 = ((population_20-population_18)/population_18)
)

# reorder variables
pop <- pop %>% select(States, pct_18_19, pct_19_20, everything())

# select western states
western_pop <- pop %>% filter(States == "Washington"|
                                States == "Oregon"|  
                                States == "California"|  
                                States == "Idaho"|
                                States == "Nevada"|
                                States == "Montana"|
                                States == "Utah"|
                                States == "Arizona"|
                                States == "Wyoming"|
                                States == "Colorado"|
                                #States == "New Mexico"|
                                States == "Alaska" |
                                States == "Hawaii")
western_pop$pct_label_18_20 <- western_pop$pct_18_20*100
western_pop$pct_label_18_20 <- round(western_pop$pct_label_18_20,1)
