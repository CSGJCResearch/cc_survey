#######################################
# Confined and Costly Survey
# Creates visualizations
# by Mari Roberts
# 12/1/2020
#######################################

# read libraries and data
# source("clean.R")

install.packages('devtools')
devtools::install_github('bbc/bbplot')

if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

# Install
install.packages("wesanderson")
# Load
library(wesanderson)

##########################
# grouped bar charts
##########################

# sort data
df <- with(df, df[order(year, type, region),])
population <- df %>% filter(type == "population")
south <- population %>% filter(region == "Southern")

# stacked barplot
# ggplot(data=south, aes(x=state, y=violation, fill =year)) + 
#   geom_bar(stat="identity") 

# order within groups
# object.data1 = south %>% 
#   group_by(state) %>% 
#   mutate(position = rank(-violation))
# 
# ggplot(object.data1, 
#        aes(x=state, y=violation, fill=year, group = position)) +
#   geom_bar(stat="identity", position="dodge")

# arrange violations in decreasing order
sel_order <- 
  south %>% 
  filter(year == "2017") %>% 
  arrange(desc(violation)) %>% 
  mutate(state_abb = factor(state_abb))

# grouped barplot of violations by state (2017-2019)
g <- south %>% 
  mutate(state_abb = factor(state_abb, levels = sel_order$state_abb, ordered = TRUE)) %>% 
  ggplot(aes(x = state_abb, y = violation, fill = year), group = state_abb) +
  geom_bar(stat="identity", width=.75, position = "dodge")  +
  geom_text(aes(label = violation), 
            position = position_dodge(0.9),
            color="black",vjust = 0.5, hjust = -.2,
            size=2.7, family="sans",angle=90) +
  # theme(axis.text.x = element_text(hjust = 1)) + 
  scale_fill_manual(values = c("#FCAC4C","#47B5C4","#044C7C")) +  
  theme_bw() 

g + theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.title = element_blank(), 
          #axis.text = element_blank(),
          axis.ticks = element_blank()) +
          labs(title="Southern Region", 
               subtitle="Violations by State (2017-2019)",
               fill = "Year") 

##########################
# radial heat map - not using
##########################

pop.df <- data.frame(population)

# transform pop.df
pop.df.m <- melt(pop.df)
library(plyr)
pop.df.m <- ddply(pop.df.m, .(variable), transform, value = scale(value))

# convert the factor levels to numeric + quanity to determine size of hole
pop.df.m$var2 = as.numeric(pop.df.m$variable) + 2

# create dummy variables and values for other half of circle
pop.df.m2 <- pop.df.m
pop.df.m2$state <- paste0("Z", pop.df.m2$state)
pop.df.m2$value <- NA
pop.df.m <- rbind(pop.df.m, pop.df.m2)

# labels and breaks need to be added with scale_y_discrete
y_labels = levels(pop.df.m$variable)
y_breaks = seq_along(y_labels) + 15

# create radial heat map
pop.df.labs <- subset(pop.df.m, variable==levels(pop.df.m$variable)[nlevels(pop.df.m$variable)])
pop.df.labs <- pop.df.labs[order(pop.df.labs$state),]
pop.df.labs$ang <- seq(from=(360/nrow(pop.df.labs))/1.5, 
                   to=(1.5*(360/nrow(pop.df.labs)))-360, 
                   length.out=nrow(pop.df.labs))+80
pop.df.labs$hjust <- 0
pop.df.labs$hjust[which(pop.df.labs$ang < -90)] <- 1
pop.df.labs$ang[which(pop.df.labs$ang < -90)] <- (180+pop.df.labs$ang)[which(pop.df.labs$ang < -90)]

# plot
p2 = ggplot(pop.df.m, aes(x=state, y=var2, fill=value)) +
  geom_tile(colour="white") +
  geom_text(data=pop.df.labs, aes(x=state, y=var2+1.5,
                              label=state, angle=ang, hjust=hjust), size=3) +
  scale_fill_gradient(low = "white", high = "#FF8300") + # color of gradient
  ylim(c(0, max(pop.df.m$var2) + 1.5)) +
  #scale_y_discrete(breaks=y_breaks, labels=y_labels) +
  coord_polar(theta="x") +
  theme(panel.background=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_text(size=5))
p2
