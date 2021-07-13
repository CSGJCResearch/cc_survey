#################
# Graph Requests from Carl for CO
# by Mari Roberts
# 6/23/2021
#################

# read data
co <- read_xlsx("data/CO_carl.xlsx", .name_repair = "universal")

# change data types
co$type <- factor(co$type)
co$year <- factor(co$year)

theme_csgjc_labels <- function(){ 
  
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      panel.border = element_blank(),
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 18,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      # axis.text = element_text(              #axis text
      #   family = font,            #axis family
      #   size = 10),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        size = 10,
        color = black,
        face = 'bold',
        margin=margin(5, b = 10)),
      
      axis.text.y = element_text(            #margin for axis text
        size = 10,
        color = black,
        face = 'bold',
        margin=margin(5, b = 10)),
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}



# graph
ggplot(data = co, aes(x=reorder(type, -pct), y = pct, fill=year, width = .75)) +
  geom_bar(position="dodge", stat="identity") +
  #scale_y_continuous(labels = scales::comma) +
  #xlab("Year") + 
  #ylab("Count") +
  labs(subtitle = "Prison Admissions by Year") +
  #geom_text(aes(label = round(count,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="black", size=3.5, width = 0.65,fontface = "bold") +
  scale_fill_manual(values = c(blue2, blue3, blue4, blue5)) +
  theme_csgjc_minimal()
