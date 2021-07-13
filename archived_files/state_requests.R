# requests for state data from staff

# read in data
source("automated_clean.R")

# subset to minnesota
mn_adm <- adm_long %>% filter(States=="Minnesota") %>% select(-Totals, -Probation, -Parole)
mn_pop <- pop_long %>% filter(States=="Minnesota") %>% select(-Totals, -Probation, -Parole)

# remove new commits
mn_adm <- mn_adm %>% filter(category != "New.commitments")
mn_pop <- mn_pop %>% filter(category != "New.commitments")

# reshape data
mn_adm <- cast(mn_adm, States+category~year)
mn_pop <- cast(mn_pop, States+category~year)

# combine data
mn <- rbind(mn_adm, mn_pop)

write.csv(mn, "mn.csv")
