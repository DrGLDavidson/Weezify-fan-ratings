# Weezify-fan-ratings
 
data source from Rivers Cuomo's Weezify demo bundles spreadhseet: https://docs.google.com/spreadsheets/d/1ZYghbmvTreCO5JPI9AEN-iLwnE7VYqoUW4MrO-Znr98/edit?gid=966375241#gid=966375241 

This repository contains an R script for visualising and summarising the fan ratings across bundles. 

R script = weezify.R
input files = weezifyRatings.csv ; weezifyRatings_BlackRoomGreenMaladroit.csv; weezifyRatings_original_filename.csv

There are several outputs: 

1. A figure showing the ratings of each demo per bundle (each song is a datapoint), with boxplots (median, quartiles etc). The black dot and line are the means and standard error for each bundle. i.e. average fan rating per bundle.
2. A csv table with the summary of data (means, error and number of demos per bundle).
3. A figure showing the ratings of each demo per sub-bundle of Green (Black Room, Green, Maladroit).
4. a csv table with the summary of data (means, error and number of demos per Green sub-bundle).
5. A figure showing the ratings of each demo per sub-bundle of Green, overlaid with total fan rating count as a colour gradient.
6. A figure showing the ratings of each demo for all bundles, overlaid with total fan rating count as a colour gradient. One outlier (300+ user ratings for a piano recording) was removed as it skewed colour gradient. 
7. A figure showing the fan rating count on the y axis. Y axis was limited to 90 fan rating count because of the above outlier.
8. A figure showing the ratings of each demo per bundle, filtered by type and fan rating count >3
9. A figure showing the rating of each demo per bundle, filtered by fan rating count >5
10. a csv file of 894 missing demos titles (i.e. no longer visible to me on shared public folder, but were there on the 14th Jan, and also searchable in Weezify app).
11. a histogram of these missing demos for fan rating (they are ranked pretty high).
12. a csv file of values included in "type".
