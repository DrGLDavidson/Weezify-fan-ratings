###Weezify ratings###


#data source: https://docs.google.com/spreadsheets/d/1ZYghbmvTreCO5JPI9AEN-iLwnE7VYqoUW4MrO-Znr98/edit?gid=966375241#gid=966375241 


library(janitor)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plyr)


df1<-read.csv("F:/RWorkspace/GitHub/Weezify-ratings/weezifyRatings.csv", header =TRUE) #dataframe with song names, bundles and rankings.

#check data
names(df1)
head(df1)
unique(df1$bundle)

#cleanup values

df1$bundle <- chartr(" ", "_", df1$bundle)

df1<-df1 %>%
  mutate(bundle = str_replace(bundle, "&", "and"))

unique(df1$bundle)

### summarySE() function to extract means and SE ###

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, 
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# means and error
means <- summarySE(df1, measurevar="fanRating", groupvars="bundle")
means

colnames(means)[colnames(means) == "fanRating"] <- "averageFanRating"
colnames(means)[colnames(means) == "N"] <- "numberOfSongs"
colnames(means)[colnames(means) == "sd"] <- "standardDeviation"
colnames(means)[colnames(means) == "se"] <- "standardError"
colnames(means)[colnames(means) == "ci"] <- "confidenceInterval"

##This saves an csv of the averages (means) and error of fan ratings across songs per bundle. 
path_out = 'F:/RWorkspace/GitHub/Weezify-ratings'
write.csv(means, file.path(path_out,'weezifyFanRatingsSummary.csv'))

tiff(file="F:/RWorkspace/GitHub/Weezify-ratings/weezifyRatingBoxplots.tiff", width = 12, height = 9, units = 'in', res = 300)
ggplot(df1, aes(x = bundle, y = fanRating, color = bundle)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 1) +
  geom_boxplot(alpha = 0.8) +
  geom_pointrange(data = means, aes(y = averageFanRating, ymin = averageFanRating - standardError, ymax = averageFanRating + standardError),
                  colour = "black", alpha = 1, size = 0.6) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.8),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20), # Adjust title size here
    axis.text.x = element_text(angle = 45, hjust = 0.95, vjust = 1.0),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 24),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "Weezify Fan Ratings Across Bundles",
    x = "Bundle",
    y = "Fan Rating"
  ) +
  guides(colour = guide_legend(override.aes = list(size = 16))) +
  scale_color_manual(values = c(
    "#C06E22", "#EFDD9F", "purple", "#577E43",
    "#00ABE6", "#BECE30", "black", "darkgrey",
    "#ED1B33", "lightgrey", "darkblue"
  ))
dev.off()

