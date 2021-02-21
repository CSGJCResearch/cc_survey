# # grouped barplot of violations by state (2017-2019)
# g <- south %>% 
#   mutate(state.abb = factor(state.abb, levels = sel_order$state.abb, ordered = TRUE)) %>% 
#   ggplot(aes(x = state.abb, y = total, fill = year), group = state.abb) +
#   geom_bar(stat="identity", width=.75, position = "dodge")  +
#   geom_text(aes(label = total), 
#             position = position_dodge(0.9),
#             color="black",vjust = 0.5, hjust = -.2,
#             size=2.7, family="sans",angle=90) +
#   # theme(axis.text.x = element_text(hjust = 1)) + 
#   scale_fill_manual(values = c("#FCAC4C","#47B5C4","#044C7C")) +  
#   theme_bw() 
# 
# g + theme(legend.position = "bottom",
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(), 
#           panel.border = element_blank(),
#           axis.title = element_blank(), 
#           #axis.text = element_blank(),
#           axis.ticks = element_blank()) +
#           labs(title="Southern Region", 
#                subtitle="Violations by State (2017-2019)",
#                fill = "Year") 

# https://rpubs.com/Koundy/71792
# Bar <- ggplot(mtcars, aes(factor(carb),fill=factor(carb))) + geom_bar(alpha=0.7) + labs(title="Bar Plot")
# grid.arrange(Bar,(Bar + scale_fill_Publication() +theme_Publication()),nrow=1)
