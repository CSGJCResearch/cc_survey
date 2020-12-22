#######################################
# Confined and Costly Survey
# Creates plots
# by Mari Roberts
# 12/1/2020
#######################################

# libraries
require(scales)

# sort data
df <- with(df, df[order(year, type, region),])

# subset to population data in south
population <- df %>% filter(type == "population")

# pct tech supervision
pct_tech <- population %>% select(state.abb, pct.tech)
pct_tech <- pct_tech %>% mutate(pct.tech.100 = pct.tech*100)

##########################
# grouped bar charts
##########################

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

#Make plot
# Southern = Orange
# Northeast = Blues
# Midwest = Reds
# West = Greens

# subset to population data in south
population <- df %>% filter(type == "population", year == "2019")
cost_per_year <- population %>% select(state.abb,
                                       cost.per.year,
                                       cost.per.year.tech)
cost_melt <- melt(cost_per_year, id.vars="state.abb")
cost_melt <-merge(regions, cost_melt, by = "state.abb")

# get pct tech
pct_tech <- population %>% select(state.abb, pct.tech)

# costs
g2 <- ggplot(cost_data, aes(x = reorder(state.abb, -cost.per.day), y = cost.per.day)) +
  geom_bar(stat="identity", 
           position="identity", 
           width = 0.75) + 
  theme_bw()

g2 + theme_csgjc

#########
# south
#########

south_cost <- cost_melt %>% filter(region == "South")

# stacked barchart of technical vs nontechnical costs
g_south <- ggplot(data = south_cost, aes(x = reorder(state.abb, -value), y = value, fill = variable)) + 
      geom_bar(stat="identity", position="stack") + 
      scale_fill_manual(values = c("#fdbe85","#e6550d"),
                        labels = c("Total Cost Per Year", "Technical Cost Per Year")) +
      scale_y_continuous(#labels = comma,
                         labels = dollar_format(),
                         breaks=seq(0,800000000,100000000)) +
      theme_csgjc 
g_south                            
                            
#########
# west
#########

west_cost <- cost_melt %>% filter(region == "West")

# stacked barchart of technical vs nontechnical costs
g_west <- ggplot(data = west_cost, aes(x = reorder(state.abb, -value), y = value, fill = variable)) + 
  geom_bar(stat="identity", position="stack",0.75) + 
  scale_fill_manual(values = c("#bae4b3","#31a354"),
                    labels = c("Total Cost Per Year", "Technical Cost Per Year")) +
  scale_y_continuous(#labels = comma,
    labels = dollar_format(),
    breaks=seq(0,3000000000,500000000)
    ) +
  theme_csgjc 

#########
# west with pct labels
#########

# add pct supervsion to data
west_cost1 <- merge(west_cost, pct_tech, by = "state.abb")
g_west + geom_label(data = west_cost1, 
                    vjust = 1.2,
         mapping = aes(label = ifelse(pct.tech.100>0,paste0("", round(pct.tech.100,1),"%"),
                                      paste0(pct.tech.100,"%")),y = 0), color = "darkgreen", fill = "white", size = 3) + 
  geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "darkgreen", fill = "white") +
  annotate("text", x = 7.4, y = 3099900000, label= "Percent of Costs Used for Technical Revocations", size = 3) +theme_csgjc 

#########
# top 20 with pct labels
#########                            

top_20 <- cost_melt %>% filter(variable=="cost.per.year") %>% group_by(state.abb) %>% arrange(desc(value)) %>% head(20) %>% select(state.abb)                 
top_20 <- merge(top_20, cost_melt, by = "state.abb")
top_20 <- merge(top_20, pct_tech, by = "state.abb")

g_top_20 <- ggplot(data = top_20, aes(x = reorder(state.abb, -value), y = value, fill = variable)) + 
  geom_bar(stat="identity", position="stack", width = 0.75) + 
  scale_fill_manual(values = c("#bae4b3","#31a354"),
                    labels = c("Total Cost Per Year", "Technical Cost Per Year")) +
  scale_y_continuous(#labels = comma,
    labels = dollar_format(),
    breaks=seq(0,3000000000,500000000)
  ) + 
  theme_csgjc

g_top_20 + geom_label(data = top_20, 
                      vjust = 1.2,
                      mapping = aes(label = ifelse(pct.tech.100>0,paste0("", round(pct.tech.100,1),"%"),
                                                   paste0(pct.tech.100,"%")),y = 0), 
                                    color = "darkgreen", fill = "white", size = 3) + 
  geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "darkgreen", fill = "white") +
  annotate("text", x = 7.4, y = 3099900000, label= "Percent of Costs Used for Technical Revocations", size = 3) 
  
#########
# top 20 pct with pct labels
######### 

top_20_pct <- pct_tech %>% group_by(state.abb) %>% arrange(desc(pct.tech)) %>% head(20) %>% select(state.abb)               
top_20_pct <- merge(top_20_pct, cost_melt, by = "state.abb")
top_20_pct <- merge(top_20_pct, pct_tech, by = "state.abb")

g_top_20 <- ggplot(data = top_20_pct, aes(x = reorder(state.abb, -value), y = value, fill = variable)) + 
  geom_bar(stat="identity", position="stack", width = 0.75) + 
  scale_fill_manual(values = c("#bae4b3","#31a354"),
                    labels = c("Total Cost Per Year", "Technical Cost Per Year")) +
  scale_y_continuous(#labels = comma,
    labels = dollar_format(),
    breaks=seq(0,1500000000,250000000)
  ) + 
  theme_csgjc

g_top_20 + geom_label(data = top_20_pct, 
                      vjust = 1.2,
                      mapping = aes(label = ifelse(pct.tech.100>0,paste0("", round(pct.tech.100,1),"%"),
                                                   paste0(pct.tech.100,"%")),y = 0), 
                      color = "darkgreen", fill = "white", size = 3) + 
  geom_point(x = 4.65, y = 1800000000, shape = 22, size = 8, color = "darkgreen", fill = "white") +
  annotate("text", x = 7.4, y = 1800000000, label= "Percent of Costs Used for Technical Revocations", size = 3) 

#########
# more admissions than pop graph
######### 

ggplot(admin_pop_100, aes(x=Population, xend=Admissions, y=`State Abb`, group=`State Abb`)) + 
  geom_dumbbell(color="darkgray", 
                size=0.5, 
                colour_x="#31a354",
                colour_xend="#253494",
                size_x=2.5,
                size_xend=2.5,) + 
  scale_x_continuous(labels = comma,breaks=seq(0,50000,5000)) +
  
  geom_text(aes(label = Population, hjust = 1, vjust = 2), size = 3) +
  geom_text(aes(label = Admissions, hjust = -3, vjust = 2), size = 3) +
  
  theme(#axis.line = element_line(colour = "grey20", size =.2),
    axis.ticks = element_blank(),
    axis.text.x = element_text(vjust = 9,margin = margin(t = 6)),
    #axis.title.y = element_text(color = "black", vjust = 2, size = 10),
    panel.border = element_blank(), 
    #panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    legend.position="top",
    legend.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm")
  ) 

# grouped barchart for admissions vs population

# get pct
pct <- admin_pop_100 %>% select(state.abb = `State Abb`, 
                                pct.100)
admin_pop_100_v2 <- merge(admin_pop_100_v2, pct, by = "state.abb")

ggplot(admin_pop_100_v2,
       aes(x = reorder(state,value), y = value, 
           fill = variable,
           label = value)) +
  geom_col(position = "dodge") +
  # add values
  geom_text(aes(label = scales::comma(value)),
            position = position_dodge(width = 0.9),
            hjust = -0.1, size = 3) +
  
  scale_fill_manual(values = c("#2b8cbe","#bdc9e1"),
                    labels = c("Admissions", "Population")) +
  coord_flip() + 
  # add percent difference
  geom_label(data = admin_pop_100_v2,
             vjust = 0.5,
             mapping = aes(label = ifelse(pct.100>0,paste0("", round(pct.100,1),"%"),
                                          paste0(pct.100,"%")),y = 0),
             color = "#2b8cbe", fill = "white", size = 3) +
  theme(#axis.line = element_line(colour = "grey20", size =.2),
    axis.ticks = element_blank(),
    #axis.text.x = element_text(vjust = 9,margin = margin(t = 6)),
    #axis.title.y = element_text(color = "black", vjust = 2, size = 10),
    panel.border = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    legend.position="top",
    legend.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm")
  ) 


