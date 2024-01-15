
# Code Shifting interactions under climate change
#################################################

# Code last update: 2024-01-15
# R version 4.3.2 (2023-10-31 ucrt)
# RStudio 2023.12.0+369 "Ocean Storm" Release (33206f75bd14d07d84753f965eaa24756eda97b7, 2023-12-17) for windows
# Details of the R session at the end of the script

# Authors:
# Baptiste Bovay
# Patrice Descombes
# Gaétan Glauser
# Yannick Chittaro
# Hanna Nomoto
# Sergio Rasmann


# 0. Packages, theme and functions 
##################################

# Clean R memory
rm(list =ls()) 

# Set the working directory
setwd("\\\\home\\bovayb$\\PhD\\Paper master thesis\\Paper for resubmisson\\Analyses for github\\Supplementary")
getwd()


library(dplyr)     # To modify the data
library(ggplot2)   # To make the plots
library(car)       # Use to analyse the models
library(DHARMa)    # Check the assumption of the models
library(Rmisc)     # To summaries the data (mean, sd, se and CI)
library(emmeans)   # To make post-hoc analyses
library(factoextra)# To plot the PCA
library(vegan)     # To make the statistics with PERMANOVA

# Customize themes for graphics 
si <- 20
my_theme<- theme_classic()+
  theme(plot.title = element_text(color="black", hjust = 0.5),  
    plot.caption = element_text(hjust = 0, face = "italic"), 
    axis.text.x =element_text(size=si,face = "italic"),
    axis.title.x =element_text(size=si),
    axis.title.y =element_text(size=si),
    axis.text.y =element_text(size=si),
    legend.text =  element_text(size=si, face = "italic"),
    legend.title =  element_text(size=si),
    title = element_text(size=si),
    axis.ticks.length = unit(2, "mm"),
    axis.line = element_line(colour = 'black', linewidth = 1.6),
    axis.ticks = element_line(colour = "black", linewidth = 1.6),
    legend.position = "bottom",
    strip.text.x = element_text(
      size = 18,face = "italic"),
    panel.spacing = unit(2, "lines"))

# Color stes for graphics:
ColM <- c("#1A4B0A", "#56CE56")
ColZ <- c("steelblue4", "steelblue2")

#----


# 1. Plant traits 
##################

# Data
#- - -

plants <- read.csv("Plant_traits.csv", h = T, sep = ";")
head(plants)
str(plants)

# Tranform the column to numeric data
plants$Herbivory.normalised <- as.numeric(plants$Herbivory.normalised)
plants$Plant <- factor(plants$Plant, levels = c("P. lanceolata", "P. atrata", "L. corniculatus", "L. alpinus"))
plants$Elevation <- as.factor(plants$Elevation)


# PCA
#- - -

# Prepare the PCA axis

# In order to have no NA, select only the row for which we have all the values
Pre_PCA <- subset(plants, SLA >= 0)

# Prepare the data for Plantago spp.
Plantago_traits <- subset(Pre_PCA, Plant =="P. atrata" | Plant =="P. lanceolata") # select only the data of Plantago spp.
Pre_PCA_Plantago <- dplyr::select(Plantago_traits, Leaves, Height, Stems, Inflorescences, SLA) # Select only the quantitative traits
PCA_Plantago <- prcomp(Pre_PCA_Plantago, center = TRUE,scale. = TRUE) # Calculate the distance between points

# Prepare the data for Lotus spp.
Lotus_traits <- subset(Pre_PCA, Plant =="L. alpinus" | Plant =="L. corniculatus") # select only the data of Lotus spp.
Pre_PCA_Lotus <- dplyr::select(Lotus_traits, Leaves, Height, Stems, Inflorescences, SLA) # Select only the quantitative traits
PCA_Lotus <- prcomp(Pre_PCA_Lotus, center = TRUE,scale. = TRUE) # Calculate the distance between points

# Plot the PCA

# Plot the PCA (95% confidence ellipse) for Plantago
fviz_pca_biplot(PCA_Plantago, geom="point", pointsize = 3,col.var = "black", labelsize = 5, 
                habillage=Plantago_traits$Plant, addEllipses=TRUE, ellipse.level=0.95) + 
  my_theme + scale_color_manual(values = c(ColM))+ scale_fill_manual(values = c(ColM))

# Plot the PCA (95% confidence ellipse) for Lotus
fviz_pca_biplot(PCA_Lotus, geom="point", pointsize = 3,col.var = "black", labelsize = 5, 
                habillage=Lotus_traits$Plant, addEllipses=TRUE, ellipse.level=0.95) + 
  my_theme + scale_color_manual(values = c(ColZ))+ scale_fill_manual(values = c(ColZ))



# PERMANOVA
#- - - - - -

# Calculate the relative distance between the points
dist_bray_plantago <- vegdist(Pre_PCA_Plantago, method = "bray")
dist_bray_lotus <- vegdist(Pre_PCA_Lotus, method = "bray")

# Analyse if there is significant differences between treatments 
adonis2(dist_bray_plantago ~ Plant, data = Plantago_traits)
adonis2(dist_bray_lotus ~ Plant, data = Lotus_traits)


# C:N ratio
# - - - - - -

# Visualize the data of the C:N ratio
ggplot(plants, aes(x = Plant, y = CN_ratio, fill = Plant)) + geom_boxplot(size = 1) +
  scale_fill_manual(values = c(ColM, ColZ)) +
  ylab("C:N ratio") +  
  my_theme + theme(legend.position = "none") + xlab(NULL) 

# Built a linear model
chn <- lm(CN_ratio ~ Plant, data = plants)

# Assumption check
par(mfrow = c(2, 2)) ; plot(chn) ; par(mfrow = c(1, 1))
plot(simulateResiduals(chn)) 

# Statistical analyses
summary(chn)
Anova(chn)

# Post-hoc test
plot(pairs(emmeans(chn, ~ Plant))) ; pairs(emmeans(chn, ~ Plant)) # Post-hoc

# To have the mean, the confidence interals and the standard error
summarySE((plants), measurevar="CN_ratio", groupvars=c("Plant"),na.rm = TRUE)

#----


# 2. Temperature analyses
##########################

# Data
#- - - -

temperature <- read.csv("Temperature.csv", h = T, sep = ";")  # Import the data
temperature$Temperature <- (temperature$Temperature-32)*(5/9) # transform the temperature from °F to °C
temperature$Date <- as.Date(temperature$Date)                 # Transform the date in the R format "Date"
temperature <- subset(temperature, Date < "2022-08-01")       # Select the date of June-July (the course of the experiment)

# set the parameter for the function:
col_temp <- c(5:5)      # column with the temperature (here only 1, but can have more, for different data logger)


# Function to calculate daily values
# - - - - - - - - - - -  - - - - - -

# Function that calculate the daily value for the temperature (mean, min, max and variation)
daily_temperature <- function(col_temp, data_frame) {
  j <- unique(data_frame$Date) # Vector with all the dates when temperature were recorded
  nb <- length(col_temp)       # Calculate the number of column with temperature data

# For-loop that calculate the daily maximum values
  max <- cbind()
  for(x in col_temp){ # For each temperature column (datalogger)
    maxi <- c()
     for(i in j){     # Identify the maximal value
      jour <- filter(data_frame, Date == i)
      maxa <- max(as.numeric(jour[,x]))
      maxi <- c(maxi, maxa)
      }
  max <- cbind(max, maxi) # Return all the maximal value in this data frame
  }
  
# For-loop that calculate the daily minimum values
  min <- cbind()
  for(x in col_temp){ # For each temperature column (datalogger)
    mini <- c()
     for(i in j){     # Identify the minimal value
      jour <- filter(data_frame, Date == i)
      mina <- min(as.numeric(jour[,x]))
      mini <- c(mini, mina)
      }
  min <- cbind(min, mini) # Return all the maximal value in this data frame
  }

# For-loop that calculate the daily mean values
  mean <- cbind()
  for(x in col_temp){ # For each temperature column (datalogger)
    m <- c()
     for(i in j){     # Calculate the average temperature
      jour <- filter(data_frame, Date == i)
      meana<- mean(as.numeric(jour[,x]))
      m <- c(m, meana)
      }
  mean <- cbind(mean, m) # Return all the maximal value in this data frame
  }

# Calculate the daily variation of temperature
  variation <- max - min

# create a new dataset 
  all1 <- data.frame(as.Date(j), max[,1:nb], min[,1:nb], mean[,1:nb], variation[,1:nb])
  
# Add names to the dataset
  colnames(all1) <- c("date", rep(c("max", "min", "mean", "variation"), nb))

# Final dataframe, output of the function
  all1
}

# Use the function with our data
High <- daily_temperature(col_temp, subset(temperature, Elevation == "High"))
Low <- daily_temperature(col_temp, subset(temperature, Elevation == "Low"))

# Plot
#- - - 

# Visualize the temperature data
ggplot(High, aes(x=date, y = mean))+
  geom_line(data = High, aes(x=date, y = mean, color = "2150 m"), linewidth = 1.2) + 
  geom_line(data = Low, aes(x=date, y = mean, color = "1500 m"), linewidth = 1.2)+ 
  scale_x_date(date_breaks = "13 day", date_labels = "%Y-%m-%d")+
  scale_y_continuous(name = "Average temperature (°C)")+ xlab(NULL) +
  my_theme + theme(
    axis.text.x =element_text(size=si-3,face = "plain"),
    legend.text =  element_text(size=si-3, face = "plain"),
    legend.position = c(.26, .98),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6), 
    legend.background = element_rect(linewidth=1, colour ="black")) +
  labs(color = "Elevation:") + theme(plot.title = element_text(color="black", hjust = 0.5, face = "plain")) +
  scale_color_manual(values = c("indianred", "deepskyblue"))

# Calculate the mean temperature and the difference between the sites
mean(High$mean)                  # for Rionda (high elevation)
mean(Low$mean)                   # for Rosseline (low elevation)
mean(Low$mean) - mean(High$mean) # Difference of temperature between the two sites 

#----


# 3. Details of the R session
#############################

sessioninfo::session_info()


#─ Session info ────────────────────────────────────────────────────────────────
#setting  value
#version  R version 4.3.2 (2023-10-31 ucrt)
#os       Windows 10 x64 (build 19045)
#system   x86_64, mingw32
#ui       RStudio
#language (EN)
#collate  French_Switzerland.utf8
#ctype    French_Switzerland.utf8
#tz       Europe/Zurich
#date     2024-01-15
#rstudio  2023.12.0+369 Ocean Storm (desktop)
#pandoc   NA

#─ Packages ────────────────────────────────────────────────────────────────────
#package      * version  date (UTC) lib source
#abind          1.4-5    2016-07-21 [1] CRAN (R 4.3.0)
#backports      1.4.1    2021-12-13 [1] CRAN (R 4.3.0)
#boot           1.3-28.1 2022-11-22 [2] CRAN (R 4.3.2)
#broom          1.0.5    2023-06-09 [1] CRAN (R 4.3.1)
#car          * 3.1-2    2023-03-30 [1] CRAN (R 4.3.1)
#carData      * 3.0-5    2022-01-06 [1] CRAN (R 4.3.1)
#cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.1)
#cluster        2.1.4    2022-08-22 [2] CRAN (R 4.3.2)
#colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.1)
#crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.1)
#DHARMa       * 0.4.6    2022-09-08 [1] CRAN (R 4.3.1)
#dplyr        * 1.1.2    2023-04-20 [1] CRAN (R 4.3.1)
#emmeans      * 1.8.7    2023-06-23 [1] CRAN (R 4.3.1)
#estimability   1.4.1    2022-08-05 [1] CRAN (R 4.3.0)
#factoextra   * 1.0.7    2020-04-01 [1] CRAN (R 4.3.1)
#fansi          1.0.4    2023-01-22 [1] CRAN (R 4.3.1)
#farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.1)
#gap            1.5-3    2023-08-26 [1] CRAN (R 4.3.1)
#gap.datasets   0.0.6    2023-08-25 [1] CRAN (R 4.3.1)
#generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.1)
#ggplot2      * 3.4.2    2023-04-03 [1] CRAN (R 4.3.1)
#ggpubr         0.6.0    2023-02-10 [1] CRAN (R 4.3.1)
#ggrepel        0.9.3    2023-02-03 [1] CRAN (R 4.3.1)
#ggsignif       0.6.4    2022-10-13 [1] CRAN (R 4.3.1)
#glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.1)
#gtable         0.3.3    2023-03-21 [1] CRAN (R 4.3.1)
#labeling       0.4.2    2020-10-20 [1] CRAN (R 4.3.0)
#lattice      * 0.22-5   2023-10-24 [1] CRAN (R 4.3.2)
#lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.1)
#lme4           1.1-34   2023-07-04 [1] CRAN (R 4.3.1)
#magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.1)
#MASS           7.3-60   2023-05-04 [2] CRAN (R 4.3.2)
#Matrix         1.6-0    2023-07-08 [1] CRAN (R 4.3.1)
#mgcv           1.9-0    2023-07-11 [2] CRAN (R 4.3.2)
#minqa          1.2.5    2022-10-19 [1] CRAN (R 4.3.1)
#munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.1)
#mvtnorm        1.2-2    2023-06-08 [1] CRAN (R 4.3.1)
#nlme           3.1-163  2023-08-09 [2] CRAN (R 4.3.2)
#nloptr         2.0.3    2022-05-26 [1] CRAN (R 4.3.1)
#permute      * 0.9-7    2022-01-27 [1] CRAN (R 4.3.1)
#pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.1)
#pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.1)
#plyr         * 1.8.8    2022-11-11 [1] CRAN (R 4.3.1)
#purrr          1.0.1    2023-01-10 [1] CRAN (R 4.3.1)
#R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.1)
#rbibutils      2.2.15   2023-08-21 [1] CRAN (R 4.3.1)
#Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
#Rdpack         2.5      2023-08-21 [1] CRAN (R 4.3.1)
#rlang          1.1.1    2023-04-28 [1] CRAN (R 4.3.1)
#Rmisc        * 1.5.1    2022-05-02 [1] CRAN (R 4.3.1)
#rstatix        0.7.2    2023-02-01 [1] CRAN (R 4.3.1)
#rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
#scales         1.2.1    2022-08-20 [1] CRAN (R 4.3.1)
#sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.2)
#tibble         3.2.1    2023-03-20 [1] CRAN (R 4.3.1)
#tidyr          1.3.0    2023-01-24 [1] CRAN (R 4.3.1)
#tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.1)
#utf8           1.2.3    2023-01-31 [1] CRAN (R 4.3.1)
#vctrs          0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
#vegan        * 2.6-4    2022-10-11 [1] CRAN (R 4.3.1)
#withr          2.5.0    2022-03-03 [1] CRAN (R 4.3.1)
#xtable         1.8-4    2019-04-21 [1] CRAN (R 4.3.1)

#[1] C:/Users/Bovayb/AppData/Local/R/win-library/4.3
#[2] C:/Program Files/R/R-4.3.2/library

#───────────────────────────────────────────────────────────────────────────────
#----

