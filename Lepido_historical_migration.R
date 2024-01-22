# # # # # # # # # # # # # # # # # # # # # # # # 
# Historical migration of species in elevation
# # # # # # # # # # # # # # # # # # # # # # # #

# Baptiste Bovay
# nov. 2023


# 0. Set the script + packages
##############################

# Clean R memory
rm(list =ls()) 

# Set the working directory
setwd("\\\\home\\bovayb$\\PhD\\Paper master thesis\\butterlfy modelling\\bioclim_CH_olivier")
getwd()


# Packages: 

library(dplyr)     # To modify the data
library(ggpubr)    # to put several plots in one figure
library(ggplot2)   # To make the plots
library(raster)    # To work with the raster data
library(sf)        # To work with the raster data
library(DHARMa)    # Check the assumption of the models

# Basic theme for ggplot2:

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


#####


# 1.Load and check the data 
###########################

# import the data
obs <- read.csv("Lepidoptera_switzerland.csv", sep = ";")
obs$X <- as.numeric(sub('.', '', obs$X)) # Set up the coordinate like the raster
obs$Y <- as.numeric(sub('.', '', obs$Y)) # Set up the coordinate like the raster
obs <- subset(obs, is.na(year) == F)     # Remove all the observation with no dates
obs2 <- obs[!duplicated(obs), ]          # Remove all the duplicates

# Select the Swiss regions with M. celadussa (the cantons mentioned below contains only M. celadussa)
Melitaea <- subset(obs2, Taxon == "Melitaea celadussa " | Taxon == "Melitaea athalia " | Taxon =="Melitaea athalia aggr. ")      # Create a data frame with the aggregate
Melitaea <- subset(Melitaea, Canton == "VS" | Canton == "VD" | Canton == "GE" | Canton == "FR" | Canton == "TI"| Canton == "NE") # Select data from regions where only M. celadussa is attested
Melitaea$Taxon <- c(rep("Melitaea celadussa ", length(Melitaea$X)))                                                              # rename the taxa
observation_data1 <- rbind(subset(obs2, Taxon =="Zygaena filipendulae "), Melitaea)                                              # Merge the two data frames
observation_data1$year <- as.numeric(observation_data1$year)                                                                     # Change the class of the "year" column

# Select the years of interest (1985-2022): 
observation_data <- subset(observation_data1, year > 1984 & year < 2023)

# Dissagregate the data:

# Create an empty raster with the same extent as the data and a 25x25m resolution
empty_raster <- raster(xmn =478500, ymn=59500, xmx=835500, ymx=294673, resolution = c(100, 100), crs = crs("+init=epsg:21781"))




# Species names to add to the function
species_name_MC <- c("Melitaea celadussa ")
species_name_ZF <- c("Zygaena filipendulae ")

# Check the data
head(observation_data1)
str(observation_data1)

#####


# 2. Find the missing elevation data
####################################

# Import a raster layer with an accuracy of 25m:
# Specify the path to the folder containing the .adf file (not the file itself)
adf_folder <- "\\\\home\\bovayb$\\PhD\\Paper master thesis\\butterlfy modelling\\bioclim_CH_olivier\\mnt25_ch"

# Use the raster() function to read the raster data
raster_data <- raster(adf_folder)

# Extracted the values associated to the points in the raster
observation_data1$extracted_elevation <- raster::extract(raster_data, observation_data1[, c("X", "Y")])
head(observation_data1)

# Replace all the missing values of the normal elevation by the extracted data
for(i in 1:length(observation_data1$elevation)){
  if(is.na(observation_data1$elevation[i] == T)){
    observation_data1$elevation[i] <- observation_data1$extracted_elevation[i]
  }
}

# Check how many data we finally have
length(na.omit(observation_data$elevation)) # 38000

# Check the data distribution according to the years:
par(mfrow = c(2, 1))
hist(subset(observation_data, Taxon == "Melitaea celadussa ")$year, breaks = length(unique(observation_data$year)))
hist(subset(observation_data, Taxon == "Zygaena filipendulae ")$year, breaks = length(unique(observation_data$year)))
par(mfrow = c(1, 1))

#####

# 3. Migration rate according to Vitasse et al. (2021)
######################################################


# Function which calculate all the stuff as in Vitasse et al. (2021)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

migration_rate <- function(observation_data, species_name){
  
# Select the data of the species of interest
data_elevation <- subset(observation_data, Taxon == species_name)

# select the median and the 95th percentile for each year
leading_edge <- c()
combination <- c()
for(i in unique(data_elevation$year)){
  
  # disaggregatet the data (at least one data by cell)
  species_sf <- st_as_sf(subset(data_elevation, year == i), coords = c("X", "Y")) # Convert the dataframe to an sf object
  species_sf$cell_id <- cellFromXY(empty_raster, st_coordinates(species_sf)) # Assign each data point to the corresponding raster cell
  filtered_species_data <- species_sf %>% group_by(cell_id) %>% slice(1) %>% ungroup()  # Use dplyr to filter the dataframe to have at most one observation per cell
  filtered_species_data$X <- st_coordinates(filtered_species_data)[, 1]
  filtered_species_data$Y <- st_coordinates(filtered_species_data)[, 2]
  data <- as.data.frame(st_drop_geometry(filtered_species_data))
  
    # Claculate the values
    quantile_value <- quantile(data$elevation, 0.95, na.rm = T) # Calculate the 95th percentile
    median_value <- quantile(data$elevation, 0.5, na.rm = T) # Calculate the median
    n <- length(data$year)
    
      # Store the values in a dataframe
      comb <- c(i, quantile_value, median_value, n)
      combination <- rbind(combination, comb)
}

combination <- as.data.frame(combination)
colnames(combination) <- c("year", "leading_edge", "median", "n")
# Combination is a dataframe which contain the value of the median and the 95th percentile for each year

# Plot of the evolution of the median and leading years according to the years
plot_migration <- annotate_figure(
  ggarrange(ggplot(combination, aes(y = leading_edge, x = year)) + 
            geom_point() + geom_line() + geom_smooth(method = "lm") + my_theme + 
            ggtitle("Leading edge") +  ylab("Median yearly elevation (m)"),
            ggplot(combination, aes(y = median, x = year)) + 
            geom_point() + geom_line() + geom_smooth(method = "lm") + my_theme +
            ggtitle("Median") +  ylab("Median yearly elevation (m)")), 
  fig.lab = unique(data_elevation$Taxon),fig.lab.size = 20, fig.lab.pos = "top")

# linear models for the migration rate 
lm_leading_edge <- lm((leading_edge) ~ year, data = combination)
lm_median <- lm((median) ~ year, data = combination)

# Extract the coefficients of the model
coef_summary1 <- summary(lm_leading_edge)$coefficients
coef_summary2 <- summary(lm_median)$coefficients

# Use the coefficient of the model to calculate the 
migration_per_decade <- as.data.frame(rbind(
  c("median",      coef_summary2["year", "Estimate"]*10, coef_summary2["year", "Estimate"]+ c(-1, 1) * qt(0.975, df = lm_leading_edge$df.residual) * coef_summary2["year", "Std. Error"] * 10),
  c("leading_edge", coef_summary1["year", "Estimate"]*10, coef_summary1["year", "Estimate"]+ c(-1, 1) * qt(0.975, df = lm_leading_edge$df.residual) * coef_summary1["year", "Std. Error"] * 10)))
colnames(migration_per_decade) <- c("type", "estimate", "ci1", "ci2")
final <- list(plot_migration, combination, migration_per_decade, lm_leading_edge, lm_median)
return(final)
}

# Run the function with our data
# - - - - - - - - - - - - - - - -

MV_MC <- migration_rate(observation_data, species_name_MC)
MV_ZF <- migration_rate(observation_data, species_name_ZF)


# Visualize the data
#- - - - - - - - - - -

# Plot the migration across the years
ggarrange(MV_MC[[1]], MV_ZF[[1]], ncol = 1)

# Plot the migration in m/decade
ggarrange(
  ggplot(MV_MC[[3]], aes(x = type, y = as.numeric(estimate), fill = type, color = type))+ 
    geom_point(position=position_dodge(0), size=7,shape=21) + scale_y_continuous(breaks=c(-75, 0, 75, 150), limits=c(-124,145))+
    geom_errorbar(aes(ymin=as.numeric(estimate)+as.numeric(ci1), ymax=as.numeric(estimate)+as.numeric(ci2),color=type), 
                  width=0.2, linewidth=1.4, position=position_dodge(0.5)) +
    geom_abline(slope = 0, lty = 2) + ggtitle("Melitaea celadussa") +
    ylab("Migration rate (m/decade)")+ xlab(NULL) + my_theme +   guides(color = "none", fill = "none"),
  ggplot(MV_ZF[[3]], aes(x = type, y = as.numeric(estimate), fill = type, color = type))+ 
    geom_point(position=position_dodge(0), size=7,shape=21) + scale_y_continuous(breaks=c(-75, 0, 75, 150), limits=c(-124,145))+
    geom_errorbar(aes(ymin=as.numeric(estimate)+as.numeric(ci1), ymax=as.numeric(estimate)+as.numeric(ci2),color=type), 
                  width=0.2, linewidth=1.4, position=position_dodge(0.5)) +
    geom_abline(slope = 0, lty = 2) + ggtitle("Zygaena filipendulae") +
    ylab("Migration rate (m/decade)")+ xlab(NULL) + my_theme +   guides(color = "none", fill = "none"))


# Test the assumption of the models
# - - - - - - - - - - - - - - - - -

# Linear model for the leading edge for M. celadussa
anova(MV_MC[[4]])
plot(simulateResiduals(MV_MC[[4]]))
par(mfrow = c(2, 2)) ; plot(MV_MC[[4]]) ; par(mfrow = c(1, 1))

# Linear model for the median for M. celadussa
anova(MV_MC[[5]])
plot(simulateResiduals(MV_MC[[5]]))
par(mfrow = c(2, 2)) ; plot(MV_MC[[5]]) ; par(mfrow = c(1, 1))

# Linear model for the leading edge for Z. filipendulae
anova(MV_ZF[[4]])
plot(simulateResiduals(MV_ZF[[4]]))
par(mfrow = c(2, 2)) ; plot(MV_ZF[[4]]) ; par(mfrow = c(1, 1))

# Linear model for the median for Z. filipendulae
anova(MV_ZF[[5]])
plot(simulateResiduals(MV_ZF[[5]]))
par(mfrow = c(2, 2)) ; plot(MV_ZF[[5]]) ; par(mfrow = c(1, 1))


# extract the number of data used
#- - - - - - - - - - - - - - - - -

sum(MV_MC[[2]]$n) # For M. celadussa
sum(MV_ZF[[2]]$n) # For Z. filipendulae

######

