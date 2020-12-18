#######################################
# Confined and Costly Survey
# Creates plots
# by Mari Roberts
# 12/1/2020
#######################################

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

# subset to population data in south
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

# subset to south populations in 2019
south <- south %>%
  filter(year == 2019) %>%
  arrange(desc(violations)) 

# remove missing values
south <- south %>% filter(state.abb != "TN" &
                          state.abb != "MS" &  
                          state.abb != "GA")

######################
#Make plot
# Southern = Orange
# Northeast = Blues
# Midwest = Reds
# West = Greens

######################
# Prison Admissions
######################

admissions <- df %>% filter(type == "admissions")
south <- admissions %>% filter(region == "South")
midwest <- admissions %>% filter(region == "Midwest")
northeast <- admissions %>% filter(region == "Northeast")
west <- admissions %>% filter(region == "West")

# sort data
south <- with(south, south[order(year, type, region),])

# south violations in 2019
g2 <- ggplot(subset(south, year == "2019" & state.abb != "VA"), aes(x = reorder(state.abb, violations), y = violations)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#7CACE4",
           width = 0.75) +
  geom_hline(yintercept = 0, size = 0.4, colour="grey20") +
  bbc_style() +
  labs(title="Violation Populations by Year",
       subtitle = "Southern Region, 2019",
       family="Helvetica")
g2 + 
  coord_flip() + 
  geom_text(aes(label = total), vjust= 0.5, hjust =-0.2, color= "grey20", size = 3, family="Helvetica") +
  theme(panel.border = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, vjust = .4, hjust = 0, face = "plain", family="Helvetica"),  
        #axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        #axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain")
        plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey20", vjust=-1,family="Helvetica"),
        plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="grey20",family="Helvetica")  
        )

# SOUTH grouped plot 
g_south <- ggplot(subset(south, year != "2020"), aes(x = reorder(state.abb, -violations), 
                                                     y=violations, fill=as.factor(year))) + 
  geom_bar(stat="identity", position = "dodge",width = 0.75) +
  scale_fill_brewer(palette = "Oranges") +
  xlab("") + 
  ylab("") +
  scale_y_continuous(breaks=seq(0, 30000, 5000))+
  theme_bw()
g_south + theme_csgjc

# West grouped plot
g_west <- ggplot(subset(west, year != "2020"), aes(x = reorder(state.abb, -violations), 
                                                   y=violations, fill=as.factor(year))) + 
  geom_bar(stat="identity", position = "dodge",width = 0.75) +
  scale_fill_brewer(palette = "Greens") +
  xlab("") + 
  ylab("") +
  #scale_y_continuous(breaks=seq(0, 30000, 2500))+
  theme_bw()
g_west + theme_csgjc

# Midwest grouped plot 
g_midwest <- ggplot(subset(midwest, year != "2020"), aes(x = reorder(state.abb, -violations), 
                                                         y=violations, fill=as.factor(year))) + 
  geom_bar(stat="identity", position = "dodge",width = 0.75) +
  scale_fill_brewer(palette = "Reds") +
  xlab("") + 
  ylab("") +
  scale_y_continuous(breaks=seq(0, 15000, 2500))+
  theme_bw()
g_midwest + theme_csgjc

# Northeast grouped plot 
g_northeast <- ggplot(subset(northeast, year != "2020" & state.abb != "DE"), aes(x = reorder(state.abb, -violations), 
                                                             y=violations, fill=as.factor(year))) + 
  geom_bar(stat="identity", position = "dodge",width = 0.75) +
  scale_fill_brewer(palette = "Blues") +
  xlab("") + 
  ylab("") +
  scale_y_continuous(breaks=seq(0, 12000, 1000))+
  theme_bw()
g_northeast + theme_csgjc

######################
# Costs
######################

# south violations in 2019
g2 <- ggplot(cost_data, aes(x = reorder(state.abb, -costperday), y = costperday)) +
  geom_bar(stat="identity", 
           position="identity", 
           width = 0.75) + theme_bw()

g2 + theme_csgjc











