###Weezify ratings###


# data source: https://docs.google.com/spreadsheets/d/1ZYghbmvTreCO5JPI9AEN-iLwnE7VYqoUW4MrO-Znr98/edit?gid=966375241#gid=966375241 


library(janitor)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plyr)


df1<-read.csv("F:/RWorkspace/GitHub/Weezify-fan-ratings/weezifyRatings.csv", header =TRUE) #dataframe with song names, bundles and rankings.

# check data
names(df1)
head(df1)
unique(df1$bundle)

# cleanup values

df1$bundle <- chartr(" ", "_", df1$bundle)

df1<-df1 %>%
  mutate(bundle = str_replace(bundle, "&", "and"))

unique(df1$bundle)

# summarySE() function to extract means and SE ###

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

# This saves an csv of the averages (means) and error of fan ratings across songs per bundle. 
path_out = 'F:/RWorkspace/GitHub/Weezify-fan-ratings'
write.csv(means, file.path(path_out,'weezifyFanRatingsSummary.csv'))

tiff(file="F:/RWorkspace/GitHub/Weezify-fan-ratings/weezifyRatingBoxplots.tiff", width = 12, height = 9, units = 'in', res = 300)
ggplot(df1, aes(x = bundle, y = fanRating, color = bundle)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 1) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + #outlier.shape = NA otherwise outliers are duplicated data points on plot
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

####sub bundles Black Room Green and Maladroit
df2<-read.csv("F:/RWorkspace/GitHub/Weezify-fan-ratings/weezifyRatings_original_filename.csv", header =TRUE) # dataframe with demo names, bundles, rankings, total fan raking, original songname and uuid.
df3<-read.csv("F:/RWorkspace/GitHub/Weezify-fan-ratings/weezifyRatings_BlackRoomGreenMaladroit.csv", header =TRUE) # original song names and bundles of BGM bundle

names(df2) # there are 894 less demos today (15th Jan 2025) than yesterday (either removed or perhaps filtered as I have view only permission)
names(df3)

unique(df1$bundle)
unique(df2$bundle) # same number of bundles

# cleanup values

df2$bundle <- chartr(" ", "_", df2$bundle)

df2<-df2 %>%
  mutate(bundle = str_replace(bundle, "&", "and"))

unique(df2$bundle)

df3$bundle <- chartr(" ", "_", df3$bundle)

unique(df3$bundle)

#cleanup song titles so they can be merged

df2$original_filename<-make_clean_names(df2$original_filename)
df3$original_filename<-make_clean_names(df3$original_filename)

head(df2)
head(df3)

df4<- merge(df3, df2 , by = "original_filename", all.x = FALSE) 
# 100 demos present on sub-bundles not present on master sheet (there are )

# means and error using summarySE function above
meansBGM <- summarySE(df4, measurevar="fanRating", groupvars="bundle.x")
meansBGM


colnames(meansBGM)[colnames(meansBGM) == "fanRating"] <- "averageFanRating"
colnames(meansBGM)[colnames(meansBGM) == "N"] <- "numberOfSongs"
colnames(meansBGM)[colnames(meansBGM) == "sd"] <- "standardDeviation"
colnames(meansBGM)[colnames(meansBGM) == "se"] <- "standardError"
colnames(meansBGM)[colnames(meansBGM) == "ci"] <- "confidenceInterval"

# This saves an csv of the averages (means) and error of fan ratings across songs per sub-bundle. 
path_out = 'F:/RWorkspace/GitHub/Weezify-fan-ratings'
write.csv(meansBGM, file.path(path_out,'weezifyFanRatingsSummary_BGM.csv'))

# boxplot 
tiff(file="F:/RWorkspace/GitHub/Weezify-fan-ratings/BlackGreenMalWeezifyRatingBoxplots.tiff", width = 10, height = 9, units = 'in', res = 300)
ggplot(df4, aes(x = bundle.x, y = fanRating, color = bundle.x)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 2) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +  # Removed outliers from boxplot
  geom_pointrange(data = meansBGM, aes(y = averageFanRating, ymin = averageFanRating - standardError, ymax = averageFanRating + standardError),
                  colour = "black", alpha = 1, size = 0.6) +  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.8),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "Weezify Fan Ratings for Black Room, Green and Maladroit Bundles",
    x = "Bundle",
    y = "Fan Rating"
  ) +
  guides(colour = guide_legend(override.aes = list(size = 16))) +
  scale_color_manual(values = c(
    "black", "#BECE30", "#DDAE56"
  ))
dev.off()

# boxplot requirement
df4$bundle.x <- as.factor(df4$bundle.x)

# boxplot with colour gradient of fan rating count for BGM sub-bundles
tiff(file="F:/RWorkspace/GitHub/Weezify-fan-ratings/BlackGreenMalWeezifyTotalRatingBoxplots.tiff", width = 10, height = 9, units = 'in', res = 300)
ggplot(df4, aes(x = bundle.x, y = fanRating, color = fanRatingCount)) +
  geom_boxplot(alpha = 0.8, aes(group = bundle.x), outlier.shape = NA) +  # Removed outliers from boxplot
  geom_point(position = position_jitter(width = 0.35), alpha = 0.7, size = 3) +  
  geom_pointrange(data = meansBGM, aes(y = averageFanRating, ymin = averageFanRating - standardError, ymax = averageFanRating + standardError),
                  colour = "black", alpha = 1, size = 0.6) +  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "Weezify Fan Ratings for Black Room, Green and Maladroit Bundles",
    x = "Bundle",
    y = "Fan Rating",
    color = "Fan rating count"
  ) +
  scale_color_gradient(low = "#EDB58F", high = "#02307C")
 # guides(colour = guide_legend(override.aes = list(size = 16))) +
  #scale_color_manual(values = c(
   # "black", "#BECE30", "#DDAE56"
  dev.off()
  
# fan rating count for all bundles

  meansFRC <- summarySE(df2, measurevar="fanRatingCount", groupvars="bundle")
  meansFRC
  
  
  colnames(meansFRC)[colnames(meansFRC) == "fanRatingCount"] <- "averageFanRatingCount"
  colnames(meansFRC)[colnames(meansFRC) == "N"] <- "numberOfSongs"
  colnames(meansFRC)[colnames(meansFRC) == "sd"] <- "standardDeviation"
  colnames(meansFRC)[colnames(meansFRC) == "se"] <- "standardError"
  colnames(meansFRC)[colnames(meansFRC) == "ci"] <- "confidenceInterval"
  
  # This saves an csv of the averages (means) and error of fan ratings across songs per sub-bundle. 
  path_out = 'F:/RWorkspace/GitHub/Weezify-fan-ratings'
  write.csv(meansBGM, file.path(path_out,'weezifyFanRatingCountSummary.csv'))
  
# fan rating count 
  tiff(file="F:/RWorkspace/GitHub/Weezify-fan-ratings/weezifyRatingCountBoxplots.tiff", width = 12, height = 9, units = 'in', res = 300)
  ggplot(df2, aes(x = bundle, y = fanRatingCount, color = bundle)) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 1) +  
    geom_boxplot(alpha = 0.8, outlier.shape = NA) +  
    geom_pointrange(data = meansFRC, aes(y = averageFanRatingCount, ymin = averageFanRatingCount - standardError, ymax = averageFanRatingCount + standardError),
                    colour = "black", alpha = 1, size = 0.6) +  
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.8),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(angle = 45, hjust = 0.95, vjust = 1.0),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 24),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    ) +
    labs(
      title = "Weezify Fan Rating Count Across Bundles",
      x = "Bundle",
      y = "Fan Rating Count"
    ) +
    ylim(0,90)+
    guides(colour = guide_legend(override.aes = list(size = 16))) +
    scale_color_manual(values = c(
      "#C06E22", "#EFDD9F", "purple", "#577E43",
      "#00ABE6", "#BECE30", "black", "darkgrey",
      "#ED1B33", "lightgrey", "darkblue"
    ))
  dev.off()

  #fan rating for all bundles using df2
  
  # fan rating count for all bundles
  
  meansFR <- summarySE(df2, measurevar="fanRating", groupvars="bundle")
  meansFR
  
  
  colnames(meansFR)[colnames(meansFR) == "fanRating"] <- "averageFanRating"
  colnames(meansFR)[colnames(meansFR) == "N"] <- "numberOfSongs"
  colnames(meansFR)[colnames(meansFR) == "sd"] <- "standardDeviation"
  colnames(meansFR)[colnames(meansFR) == "se"] <- "standardError"
  colnames(meansFR)[colnames(meansFR) == "ci"] <- "confidenceInterval"
  
  # boxplot with colour gradient of fan rating count for all bundles
  # need to remove the piano that is rated by 300+ users as it skews colour gradient
  
 df2b<-df2[-1126,] 
 
  tiff(file="F:/RWorkspace/GitHub/Weezify-fan-ratings/weezifyRatingwithCountBoxplots.tiff", width = 12, height = 9, units = 'in', res = 300)
  ggplot(df2b, aes(x = bundle, y = fanRating, color = fanRatingCount)) +
        geom_boxplot(alpha = 0.8, outlier.shape = NA) +  
    geom_point(position = position_jitter(width = 0.2), alpha = 0.7, size = 1) +  
    geom_pointrange(data = meansFR, aes(y = averageFanRating, ymin = averageFanRating - standardError, ymax = averageFanRating + standardError),
                    colour = "black", alpha = 1, size = 0.6) +  
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.8),
     # legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(angle = 45, hjust = 0.95, vjust = 1.0),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 24),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    ) +
    labs(
      title = "Weezify Fan Rating Across Bundles",
      x = "Bundle",
      y = "Fan Rating Count",
      color = "Fan rating count"
    ) +
    scale_color_gradient(low = "#EDB58F", high = "#02307C")
  dev.off()
  