# Stacked area line graph with new court, prob,parole, 2017-2019
# only states that included data in 2019

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     #axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 6),size=14,face="bold"),
                     axis.text.y = element_text(size=14),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(), 
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(), 
                     #panel.grid.major.y = element_blank(), 
                     #panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size=14,face="bold"),
                     legend.position = "top",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))


adm_by_year_17_19 <- adm_by_year %>% filter(year != 2020)
# adm_by_year_17_19$category <- factor(adm_by_year_17_19$category)
# adm_by_year_17_19$year <- as.character(adm_by_year_17_19$year)
# adm_by_year_17_19$count <- as.numeric(adm_by_year_17_19$count)

# adm_group <- adm_by_year_17_19 %>% group_by(category, year) %>% summarise(total=sum(count))
x.2017 <- adm_by_year_17_19 %>% filter(year == "2017")
x.2017.NewCommits <- x.2017 %>% filter(category == "New Commitments")







write.csv(adm_by_year_17_19, "adm_by_year_17_19.csv")
test <- read.csv("adm_by_year_17_19.csv")
test <- test %>% select(-X)

summary.newcommit = adm_by_year_17_19 %>%
  filter(year) %>%
  summarise(n=n(), ncases=sum(case), case.rate=mean(case)) 




# Grouped
plot1<- ggplot(adm_group, aes(fill=category, y=total, x=year)) + 
                geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = scales::comma(total)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=3, hjust=.5,color="white", size=5, width = 0.65,fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#6baed6", "#3182bd", "#08519c")) + 
  theme_bw()
plot1 <- plot1 + theme_csgjc  
plot1

ggsave(plot = plot1,
       path="plots",
       filename = "adm.png",
       width = 6, height = 4, dpi = 150)










