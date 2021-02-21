#######################################
# Confined and Costly Survey
# Imports/cleans CC Survey for Automated Reports
# by Mari Roberts
# 2/21/2020
#######################################

# load R file which cleans data
source("automated_clean.R")

# create factor variables
adm_long$year <- factor(adm_long$year)
pop_long$year <- factor(pop_long$year)

# custom theme
theme_csgjc <- theme(axis.ticks = element_blank(),
                     #axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 6.5, margin = margin(t = 6),size=8,face="italic"),
                     axis.title.y = element_blank(),
                     panel.border = element_blank(), 
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(), 
                     #panel.grid.major.y = element_blank(), 
                     #panel.grid.minor.y = element_blank(),
                     legend.position = "none",
                     plot.title = 
                       element_text(hjust = 0.5),
                     plot.margin = margin(0, 0, 0, 0, "cm"))

##################
# admissions by year (prob, parole, new commits)
##################

# subset data
adm_by_year <- adm_long %>% filter(category=="Total.probation.violation.admissions"|
                                   category=="Total.parole.violation.admissions"| 
                                   category=="New.commitments") %>% select(-Totals,-Probation,-Parole)
adm_by_year <- adm_by_year %>% filter(States=="Alaska"|States=="Alabama"|States=="Delaware"|States=="Hawaii"|States=="New York"|States== "Wyoming")

# custom plot function
adm_by_year_plot <- function(df, myvar) {      
  ggplot(data = df %>% filter(States == myvar), 
         aes(x = year, y = count, fill=category)) +
         geom_bar(stat = "identity", position = "stack", width = 0.65) +
         scale_y_continuous(labels = scales::comma) +
         xlab("Year") + 
         #ylab("Count") +
    geom_text(aes(label = scales::comma(count)), color="white", size=2.75,position = position_stack(vjust = .5)) +
    scale_fill_manual(values=c("#08519c", "#3182bd", "#6baed6")) +
    theme_bw() +
    theme_csgjc
}

# test
# adm_by_year_plot(adm_by_year, 'Alaska')

# loop through states var, create plots & store them in a list
adm_by_year_plot_list <- unique(adm_by_year$States) %>% 
  purrr::set_names() %>% 
  purrr::map( ~ adm_by_year_plot(adm_by_year, .x))

# str(adm_by_year_plot_list, max.level = 1) # view list

# save all plots to PNG files
purrr::iwalk(adm_by_year_plot_list,
             ~ ggsave(plot = .x,
                      path="plots",
                      filename = paste0("adm_by_year_", .y, ".png"),
                      type = 'cairo', width = 6, height = 6, dpi = 150)
)

##################
# admissions for probation violations (tech vs nontech)
##################

##################
# admissions for parole violations (tech vs nontech)
##################











##################
# pop by year (prob, parole, new commits)
##################

##################
# pop for probation violations (tech vs nontech)
##################

##################
# pop for parole violations (tech vs nontech)
##################
