#######################################
# Confined and Costly Survey
# Custom Plot Functions for Automated Reports
# by Mari Roberts
# 2/21/2020
#######################################

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     #axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 10),size=10,face="bold"),
                     axis.text.y = element_text(size=10),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.border = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size=10,face="bold"),
                     legend.position = "right",
                     plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5, size = 15),
                     plot.margin = margin(0, 0, 0, 0, "cm"))

# custom function for total population (save lines of code)
total_pop_plot_2019 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.population), y=Total.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = scales::comma(Total.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#0db4e4", "#007392")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.population.pct>0,paste0("", round(Total.population.pct,1),"%"),
                                            paste0("",Total.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
  return(plot1)
}

# same as above but with different color for 2020
total_pop_plot_2020 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.population), y=Total.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = scales::comma(Total.population)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=2, hjust=.5,color="white", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#007392", "#00475d")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.population.pct>0,paste0("", round(Total.population.pct,1),"%"),
                                            paste0("",Total.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
  return(plot1)
}

# custom function for total violation population
total_viol_pop_plot_2019 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.violation.population), y=Total.violation.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = round(Total.violation.population,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#0db4e4", "#007392")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.violation.population.pct>0,paste0("", round(Total.violation.population.pct,1),"%"),
                                            paste0("",Total.violation.population.pct,"%")),y = 0), color = "#007392", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#007392", fill = "white") 
  return(plot1)
}

# same as above but with different color for 2020
total_viol_pop_plot_2020 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.violation.population), y=Total.violation.population, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = round(Total.violation.population,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#007392", "#00475d")) +
    # ggtitle("States With Increases in Population Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.violation.population.pct>0,paste0("", round(Total.violation.population.pct,1),"%"),
                                            paste0("",Total.violation.population.pct,"%")),y = 0), color = "#00475d", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#00475d", fill = "white") 
  return(plot1)
}

# custom function for total admissions
total_adm_plot_2019 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.admissions), y=Total.admissions, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = round(Total.admissions,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#7fc241", "#5c922f")) +
    # ggtitle("States With Increases in admissions Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.admissions.pct>0,paste0("", round(Total.admissions.pct,1),"%"),
                                            paste0("",Total.admissions.pct,"%")),y = 0), color = "#5c922f", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#5c922f", fill = "white") 
  return(plot1)
}

# same as above but with different color for 2020
total_adm_plot_2020 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.admissions), y=Total.admissions, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = round(Total.admissions,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#5c922f", "#315c15")) +
    # ggtitle("States With Increases in admissions Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.admissions.pct>0,paste0("", round(Total.admissions.pct,1),"%"),
                                            paste0("",Total.admissions.pct,"%")),y = 0), color = "#315c15", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#315c15", fill = "white") 
  return(plot1)
}

# custom function for total violation admissions
total_viol_adm_plot_2019 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.violation.admissions), y=Total.violation.admissions, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = round(Total.violation.admissions,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#7fc241", "#5c922f")) +
    # ggtitle("States With Increases in admissions Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.violation.admissions.pct>0,paste0("", round(Total.violation.admissions.pct,1),"%"),
                                            paste0("",Total.violation.admissions.pct,"%")),y = 0), color = "#315c15", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#315c15", fill = "white") 
  return(plot1)
}

# same as above but with different color for 2020
total_viol_adm_plot_2020 <- function(df){
  
  plot1 <- ggplot(df, aes(x=reorder(States, -Total.violation.admissions), y=Total.violation.admissions, fill = year)) +
    geom_bar(position="dodge", stat="identity") +
    geom_text(aes(label = round(Total.violation.admissions,0)),position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, hjust=.5,color="#8c8c8c", size=3.5, width = 0.65,fontface = "bold") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("#5c922f", "#315c15")) +
    # ggtitle("States With Increases in admissions Over 2%") + 
    theme_bw()
  plot1 <- plot1 + theme_csgjc + 
    geom_label(data = df, vjust = 1,
               mapping = aes(label = ifelse(Total.violation.admissions.pct>0,paste0("", round(Total.violation.admissions.pct,1),"%"),
                                            paste0("",Total.violation.admissions.pct,"%")),y = 0), color = "#315c15", fill = "white", size = 3) + 
    geom_point(x = 4.65, y = 3099900000, shape = 22, size = 8, color = "#315c15", fill = "white") 
  return(plot1)
}


