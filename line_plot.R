
# read in data
source("automated_clean.R")

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

# plot
line_plot <- ggplot(data=df_final, aes(x=year, y=pop_change, group=states)) +
  geom_line() + theme_bw() + 
  scale_color_manual(values = c(rep("gray", 43)))
line_plot + stat_summary(aes(y = pop_change,group=1), fun.y=mean, colour="red", geom="line",group=1)



