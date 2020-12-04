#######################################
# Confined and Costly Survey
# Creates visualizations
# by Mari Roberts
# 12/1/2020
#######################################

# read libraries and data
# source("clean.R")

df <- data.frame(population)

# transform df
df.m <- melt(df)
df.m <- ddply(df.m, .(variable), transform, value = scale(value))

# convert the factor levels to numeric + quanity to determine size of hole
df.m$var2 = as.numeric(df.m$variable) + 2

# create dummy variables and values for other half of circle
df.m2 <- df.m
df.m2$state <- paste0("Z", df.m2$state)
df.m2$value <- NA
df.m <- rbind(df.m, df.m2)

# labels and breaks need to be added with scale_y_discrete
y_labels = levels(df.m$variable)
y_breaks = seq_along(y_labels) + 15

# create radial heat map
df.labs <- subset(df.m, variable==levels(df.m$variable)[nlevels(df.m$variable)])
df.labs <- df.labs[order(df.labs$state),]
df.labs$ang <- seq(from=(360/nrow(df.labs))/1.5, 
                   to=(1.5*(360/nrow(df.labs)))-360, 
                   length.out=nrow(df.labs))+80
df.labs$hjust <- 0
df.labs$hjust[which(df.labs$ang < -90)] <- 1
df.labs$ang[which(df.labs$ang < -90)] <- (180+df.labs$ang)[which(df.labs$ang < -90)]

# plot
p2 = ggplot(df.m, aes(x=state, y=var2, fill=value)) +
  geom_tile(colour="white") +
  geom_text(data=df.labs, aes(x=state, y=var2+1.5,
                              label=state, angle=ang, hjust=hjust), size=3) +
  scale_fill_gradient(low = "white", high = "#FF8300") + # color of gradient
  ylim(c(0, max(df.m$var2) + 1.5)) +
  #scale_y_discrete(breaks=y_breaks, labels=y_labels) +
  coord_polar(theta="x") +
  theme(panel.background=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_text(size=5))
p2
