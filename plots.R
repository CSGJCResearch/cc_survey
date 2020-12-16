#######################################
# Confined and Costly Survey
# Creates plots
# by Mari Roberts
# 12/1/2020
#######################################

# read clean code
source("clean.R")

# needed for bbc style
install.packages('devtools')
devtools::install_github('bbc/bbplot')
if(!require(pacman))install.packages("pacman")
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

##########################
# grouped bar charts
##########################

# sort data
df <- with(df, df[order(year, type, region),])
population <- df %>% filter(type == "population")
south <- population %>% filter(region == "South")

# arrange population in decreasing order
sel_order <- 
  south %>% 
  arrange(desc(total)) %>% 
  mutate(state.abb = factor(state.abb))

g <- ggplot(south, aes(fill=year, x=state.abb, y = violations)) + geom_bar(position="dodge", stat="identity")
  
g +
  bbc_style()+
  labs(title = "Violation Populations by year", 
       subtitle = "Southern States from 2017 to 2020") +
  #geom_text(aes(label = total), vjust= 1.6, color= "white", size = 2)+
  # Add addition theme arguments after bbc_style()
  theme(plot.subtitle = element_text(size = rel(1.5)))
