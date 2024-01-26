
# Code Species Distribution Model under climate change
######################################################

# Code last update: 2024-01-25
# R version 4.2.2 (2022-10-31)
# RStudio 2022.11.999-dev+999 "Spotted Wakerobin" Release (99999999, 2022-11-09) for Linux
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
setwd("/cluster/raid/home/bovayb/Spacial_analyses")
getwd()


# Packages: 

library(rgdal)     # To work with rester
library(enmSdmX)   # To build the MaxEnt models
library(rJava)     # Use java with R
library(Rmisc)     # Work with the data
library(DHARMa)    # Check the assumption of the models
library(sf)        # To work with spacial data
library(ggpubr)    # To put several plots in one figure
library(ggplot2)   # To make the plots
library(dismo)     # Work with the model
library(raster)    # To work with raster
library(sp)        # Work with GPS data
library(dplyr)     # To modify the data
library(terra)     # Work with the GPS data and the raster
library(emmeans)   # To make post-hoc analyses

# Basic theme for ggplot2
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

# Without the label and the legends in italic 
my_theme3 <- my_theme +
  theme(legend.text =  element_text(size=si, face = "plain"))

# Color palette
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

#####


# 1.Load and check the data 
###########################

# Load the raster (.rdata)

# Rasters with current climatic data
load("bio1_t_8110.rData")
bioclim_1 <- x

load("bio2_tdr_8110.rData")
bioclim_2 <- x

load("bio5_tmaxw_8110.rData")
bioclim_5 <- x

load("bio12_p_8110.rData")
bioclim_12 <- x

load("bio15_ps_8110.rData")
bioclim_15 <- x

# Raster with prevision under RPC 8.5
load("bio1_t_CLMCOM-CCLM4_HADGEM_EUR11_RCP45_2085.rData")
RPC45_bioclim_1 <- x

load("bio2_tdr_CLMCOM-CCLM4_HADGEM_EUR11_RCP45_2085.rData")
RPC45_bioclim_2 <- x

load("bio5_tmaxw_CLMCOM-CCLM4_HADGEM_EUR11_RCP45_2085.rData")
RPC45_bioclim_5 <- x

load("bio12_p_CLMCOM-CCLM4_HADGEM_EUR11_RCP45_2085.rData")
RPC45_bioclim_12 <- x

load("bio15_ps_CLMCOM-CCLM4_HADGEM_EUR11_RCP45_2085.rData")
RPC45_bioclim_15 <- x

# Raster with prevision under RPC 8.5
load("bio1_t_CLMCOM-CCLM4_HADGEM_EUR11_RCP85_2085.rData")
RPC85_bioclim_1 <- x

load("bio2_tdr_CLMCOM-CCLM4_HADGEM_EUR11_RCP85_2085.rData")
RPC85_bioclim_2 <- x

load("bio5_tmaxw_CLMCOM-CCLM4_HADGEM_EUR11_RCP85_2085.rData")
RPC85_bioclim_5 <- x

load("bio12_p_CLMCOM-CCLM4_HADGEM_EUR11_RCP85_2085.rData")
RPC85_bioclim_12 <- x

load("bio15_ps_CLMCOM-CCLM4_HADGEM_EUR11_RCP85_2085.rData")
RPC85_bioclim_15 <- x


# variable explanation: 
#- - - - - - - - - - - -
# bioclim_1  = BIO1  = Annual Mean Temperature
# bioclim_2  = BIO2  = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# bioclim_5  = BIO5  = Max Temperature of Warmest Month
# bioclim_12 = BIO12 = Annual Precipitation
# bioclim_15 = BIO15 = Precipitation Seasonality (Coefficient of Variation)
#- - - - - - - - - - - - 


# Load the shapefile for cutting the raster
crop_extent_MC <- read_sf("Clipped_MC_4.shx")
crop_extent_ZF <- read_sf("Clipped_ZF_3.shx")

# Put all the raster together
raster_stack <- stack(bioclim_5, bioclim_2, bioclim_12, bioclim_15)
raster_stack_RPC45 <- stack(RPC45_bioclim_5, RPC45_bioclim_2, RPC45_bioclim_12, RPC45_bioclim_15)
raster_stack_RPC85 <- stack(RPC85_bioclim_5, RPC85_bioclim_2, RPC85_bioclim_12, RPC85_bioclim_15)

# load the observation data
observation_data <- read.csv("Lepidoptera_switzerland.csv", sep = ";")
observation_data$X <- as.numeric(sub('.', '', observation_data$X))
observation_data$Y <- as.numeric(sub('.', '', observation_data$Y))


# Select only the region where only Melitaea celadussa is present 
Melitaea <- subset(observation_data, Taxon == "Melitaea athalia " | Taxon =="Melitaea athalia aggr. "| Taxon =="Melitaea celadussa ")
Melitaea <- subset(Melitaea, Canton == "VS" | Canton == "VD" | Canton == "GE" | Canton == "FR" | Canton == "TI"| Canton == "NE")
Melitaea$Taxon <- c(rep("Melitaea celadussa ", length(Melitaea$X)))
observation_data <- rbind(subset(observation_data, Taxon =="Zygaena filipendulae "), Melitaea)

observation_data2 <- subset(observation_data, Pr < 100) # keep only the data with enough precision !!#

# Species names to add to the function
species_name_MC <- c("Melitaea celadussa ")
species_name_ZF <- c("Zygaena filipendulae ")


# Visualize the data to see if all is ok: 
#- - - - - - - - - - - - - - - - - - - - -

# plot all the raster:
plot(raster_stack)

# Plot a raster with all the points:
plot(bioclim_15) + points(observation_data2$X, observation_data2$Y)



# Import the raster for the elevation data
#- - - - - - - - - - - - - - - - - - - - -

# Specify the path to the folder containing the .adf file (not the file itself)
adf_folder <- "/cluster/raid/home/bovayb/Spacial_analyses/mnt25_ch"

# Use the raster() function to read the raster data
raster_data <- raster(adf_folder)

# Plot the raster data to check if all is ok
# plot(raster_data) + points(observation_data1$X, observation_data1$Y)

# Extracted the values associated to the points in the raster
observation_data2$extracted_elevation <- raster::extract(raster_data, observation_data2[, c("X", "Y")])
head(observation_data2)

# Check how many NA we have 
length(na.omit(observation_data2$elevation)) # 20745
length((observation_data2$elevation)) # 40358

# Replace all the missing values of the normal elevation by the extracted data
for(i in 1:length(observation_data2$elevation)){
  if(is.na(observation_data2$elevation[i] == T)){
    observation_data2$elevation[i] <- observation_data2$extracted_elevation[i]
  }
}


#####


# 2. Function to build the SDM
##############################


# Function which create the maxent model
model_creation <- function(observation_data2, raster_stack, predictors, species_name, raster_data, crop_extent){
  
  # 2.1 take the climate values where the sp is present
  #- - - - - - - - - - - - - - - - - - - - - -  - - - -- 
  
  # Set the characteristic for the maps and the model
  f_data <- 100000   # Number of false data
  model_traing <- 30 # Number of time that maxent will run
  
  # Select the data of the species of interest
  data_gps1 <- subset(observation_data2, Taxon == species_name)
  
  # Disagregate the data (keep 1 obs for each 25x25m)
  empty_raster <- raster(xmn = xmin(raster_stack), xmx = xmax(raster_stack),ymn = ymin(raster_stack),ymx = ymax(raster_stack),resolution = c(100, 100), crs = crs("EPSG:21781"))
  species_sf <- st_as_sf(data_gps1, coords = c("X", "Y")) # Convert the dataframe to an sf object
  species_sf$cell_id <- cellFromXY(empty_raster, st_coordinates(species_sf)) # Assign each data point to the corresponding raster cell
  filtered_species_data <- species_sf %>% group_by(cell_id) %>% slice(1) %>% ungroup()# Use dplyr to filter the dataframe to have at most one observation per cell
  filtered_species_data$X <- st_coordinates(filtered_species_data)[, 1]
  filtered_species_data$Y <- st_coordinates(filtered_species_data)[, 2]
  data_gps1 <- as.data.frame(st_drop_geometry(filtered_species_data))
  
  
  data_gps <-   data_gps1[, 10:11] # Keep only the GPS data 
  raster_list <- crop(raster_stack, crop_extent) # Keep only the portion of the raster of our study area
  names(data_gps) <- c("x", "y") 
  
  
  # This for loop allow us to take the environmental data where the sp is present
  for (raster_name in names(raster_list)) {
    raster_obj <- raster_list[[raster_name]]
    extracted_data <- raster::extract(raster_obj, data_gps[, c("x", "y")])
    data_gps[[raster_name]] <- extracted_data
  }
  data_loc <- data_gps
  head(data_loc) # Data_loc contains the gps coordinates + the ecological values corresponding 
  
  #- - - - - - 
  
  
  # 2.2 Create "fake" data where the species is not present
  #- - - - - - - - - - - - - - - - - - - - - -  - - - -- 
  
  climat_data <- rast(raster_list)
  
  # Code to sample randomly values in the raster with their coordinates
  data_locf <- terra::spatSample(climat_data, f_data, xy = TRUE)
  data_locf <- data_locf[complete.cases(data_locf), ]
  data_locf <- data_locf[1:min(f_data, nrow(data_locf)), ]
  head(data_locf)# Data_locf contains the coordinates of random points with the corresponding ecological variables 
  
  #- - - - - - 
  
  
  # 2.3 Merge the two dataframe 
  #- - - - - - - - - - - - - - - - - - - - - -  - - - -- 
  
  # merge a dataframe with the info if a value is radom (0) or true (1)
  presBg <- data.frame(
    presBg = c(
      rep(1, nrow(data_loc)),
      rep(0, nrow(data_locf))))
  
  # merge the two dataframes
  env <- rbind(data_loc, data_locf)
  
  # Add the info whether the data is a fake or a true one
  env <- cbind(presBg, env)
  env <- env[complete.cases(env), ] 
  head(env) # Env containt the observed (1) and random (1) points and the ecological values corresponding. 
  
  #- - - - - - 
  
  
  # 2.4 make the SDM
  #- - - - - - - - - - - - - - - - - - - - - -  - - - -- 
  
  # Select the predictors 
  cores <- 1
  
  # MaxEnt
  mx <- trainMaxEnt(
    data = env,
    resp = 'presBg',
    preds = predictors,
    regMult = model_traing,
    verbose = TRUE,
    cores = cores
  )
  
  # Plot the relativce contribution of each varaibles
  relative_contribution <- plot(mx)
  
  #- - - - - - 
  
  liste_final <- list(mx, env)
  return(liste_final)
}

#####


# 3. Models creation
####################

# predictors 
# (as Biol1 and Biol5 are strongly correlated, we kept only Biol5)
pred_MC <- c("bio5", "bio2", "bio12", "bio15")
pred_ZF <- c("bio5", "bio2", "bio12", "bio15")


# Run the function which make the model
MC_mx <- model_creation(observation_data2, raster_stack, pred_MC, species_name_MC, raster_data, crop_extent_MC) # Run the model for Melitaea celadussa
ZF_mx <- model_creation(observation_data2, raster_stack, pred_ZF, species_name_ZF, raster_data, crop_extent_ZF) # Run the model for Zygaena filipendulae

# Check the variable contribution to the model
plot(MC_mx[[1]])
plot(ZF_mx[[1]])
head(MC_mx[[2]])
head(ZF_mx[[2]])

# Check the correlation of the variables
cor(MC_mx[[2]][, 4:7], method = c("pearson"))
cor(MC_mx[[2]][, 4:7], method = c("spearman"))
cor(ZF_mx[[2]][, 4:7], method = c("pearson"))
cor(ZF_mx[[2]][, 4:7], method = c("spearman"))

#####


# 4. Maps and model predictions 
###############################


# make a function which draw maps of the model
plot_map <- function(mx, raster_stack, species_name, raster_data, crop_extent){
  
  map_pr <- 1000000   # Resolution of the final map
  
  # Try to fit with the future cc scenario
  raster_list <- crop(raster_stack, crop_extent)
  raste <- rast(raster_list)
  
  # choose of the precision of the map
  pred_data <- terra::spatSample(raste ,map_pr,"regular",xy=TRUE, cells=TRUE)
  pred_data <- pred_data[complete.cases(pred_data), ]
  pred_data <- pred_data[-1]
  colnames(pred_data)[1:2] <- c("longitude","latitude") 
  
  # draw the model predictions
  mxMap <- predictEnmSdm(mx, pred_data) 
  pred_data$predict_sp <- mxMap
  head(pred_data)
  
  # Plot the model predictions
  predicted_niche <- ggplot() +
    geom_point(data = pred_data, aes(x = longitude, y = latitude, color = predict_sp), size = 3, shape = 15) +
    labs(title = species_name) + theme_minimal()+
    scale_color_gradientn(colors = mycol,
                          breaks=c(0,0.25,0.5,0.75,1),labels=c(0,0.25,0.5,0.75,1), limits=c(0,1))
  
  final_list <- list(pred_data, predicted_niche)
}  

# Current niches
MC_map_current <- plot_map(MC_mx[[1]], raster_stack, species_name_MC, raster_data, crop_extent_MC) # Run the model for Melitaea celadussa
ZF_map_current <- plot_map(ZF_mx[[1]], raster_stack, species_name_ZF, raster_data, crop_extent_ZF) # Run the model for Zygaena filipendulae

# Climatic niche in 2085 with RPC4.5
MC_map_RPC45 <- plot_map(MC_mx[[1]], raster_stack_RPC45, species_name_MC, raster_data, crop_extent_MC) # Run the model for Melitaea celadussa
ZF_map_RPC45 <- plot_map(ZF_mx[[1]], raster_stack_RPC45, species_name_ZF, raster_data, crop_extent_ZF) # Run the model for Zygaena filipendulae

# Climatic niche in 2085 with RPC8.5
MC_map_RPC85 <- plot_map(MC_mx[[1]], raster_stack_RPC85, species_name_MC, raster_data, crop_extent_MC) # Run the model for Melitaea celadussa
ZF_map_RPC85 <- plot_map(ZF_mx[[1]], raster_stack_RPC85, species_name_ZF, raster_data, crop_extent_ZF) # Run the model for Zygaena filipendulae

# Plot the swiss map with the SDM
MC_map_current[[2]] + my_theme + theme(legend.position = "none")
MC_map_RPC45[[2]] + my_theme + theme(legend.position = "none")
MC_map_RPC85[[2]] + my_theme + theme(legend.position = "none")
ZF_map_current[[2]] + my_theme + theme(legend.position = "none")
ZF_map_RPC45[[2]] + my_theme + theme(legend.position = "none")
ZF_map_RPC85[[2]] + my_theme + theme(legend.position = "none")

#####


# 5. Extraction of the niches elevations
########################################


# Function which extract the elevation data
elevation_extraction <- function(pred_data_new, pred_data_RPC45, pred_data_RPC85, p, raster_data){
  
  # Set the dataframe with the right proba
  elevation_current <- subset(pred_data_new, predict_sp > p)
  elevation_RPC45 <- subset(pred_data_RPC45, predict_sp > p)
  elevation_RPC85 <- subset(pred_data_RPC85, predict_sp > p)
  
  
  # get the elevation for the current niche
  elevation_current$elevation <- raster::extract(raster_data, elevation_current[, c("longitude", "latitude")])
  elevation_current$type <- rep("Current", length(elevation_current$elevation))

  # get the elevation for the future niche (2085, RPC 4.5)
  elevation_RPC45$elevation <- raster::extract(raster_data, elevation_RPC45[, c("longitude", "latitude")])
  elevation_RPC45$type <- rep("RPC4.5", length(elevation_RPC45$elevation))

  # get the elevation for the future niche (2085, RPC 8.5)
  elevation_RPC85$elevation <- raster::extract(raster_data, elevation_RPC85[, c("longitude", "latitude")])
  elevation_RPC85$type <- rep("RPC8.5", length(elevation_RPC85$elevation))

  # Make a dataframe with only the coordinates and the elevation
  elevations <- rbind(dplyr::select(as.data.frame(elevation_current), elevation, type),
                      dplyr::select(as.data.frame(elevation_RPC45)  , elevation, type),
                      dplyr::select(as.data.frame(elevation_RPC85)  , elevation, type))
  
  #####
  return(elevations)
  
}

# Extract the elevation for the butterflies
MC_elevations <- elevation_extraction(MC_map_current[[1]], MC_map_RPC45[[1]], MC_map_RPC85[[1]], 0.5, raster_data)
ZF_elevations <- elevation_extraction(ZF_map_current[[1]], ZF_map_RPC45[[1]], ZF_map_RPC85[[1]], 0.5, raster_data)

# Plot the elevation distribution as boxplot 
ggarrange(
  ggplot(MC_elevations, aes(x = type, y = elevation, fill = type )) + geom_boxplot() + 
    ggtitle(species_name_MC) + xlab(NULL) +  my_theme3,
  ggplot(ZF_elevations, aes(x = type, y = elevation, fill = type )) + geom_boxplot() + 
    ggtitle(species_name_ZF)+ xlab(NULL)+ my_theme3)

# Plot the elevation distributions as density plots
ggarrange(
  ggplot(MC_elevations, aes(y = elevation, fill = type)) +
    geom_density(alpha = 0.5) + ggtitle(species_name_MC) + 
    labs(y = "Elevation (m)") + coord_cartesian(ylim = c(min(MC_elevations$elevation), max(MC_elevations$elevation))) + 
    my_theme3 + theme(plot.title = element_text(face = "italic"), axis.title.x = element_blank(),
                      axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none"),
  ggplot(ZF_elevations, aes(y = elevation, fill = type)) +
    geom_density(alpha = 0.5) + ggtitle(species_name_ZF) +
    labs(y = "Elevation (m)") + coord_cartesian(ylim = c(min(ZF_elevations$elevation), max(MC_elevations$elevation))) + 
    my_theme3 + theme(plot.title = element_text(face = "italic"), axis.title.x = element_blank(),
                      axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none"),
  ncol = 2, common.legend = F)

# Shift in the mediane RPC 4.5:
quantile(subset(MC_elevations, type == "RPC4.5")$elevation, probs = 0.5, na.rm = T) - quantile(subset(MC_elevations, type == "Current")$elevation, probs = 0.5, na.rm = T)
quantile(subset(ZF_elevations, type == "RPC4.5")$elevation, probs = 0.5, na.rm = T)- quantile(subset(ZF_elevations, type == "Current")$elevation, probs = 0.5, na.rm = T)

# Shift in the leading edge RPC 4.5:
quantile(subset(MC_elevations, type == "RPC4.5")$elevation, probs = 0.95, na.rm = T) - quantile(subset(MC_elevations, type == "Current")$elevation, probs = 0.95, na.rm = T)
quantile(subset(ZF_elevations, type == "RPC4.5")$elevation, probs = 0.95, na.rm = T) - quantile(subset(ZF_elevations, type == "Current")$elevation, probs = 0.95, na.rm = T)

# Shift in the mediane RPC 8.5:
quantile(subset(MC_elevations, type == "RPC8.5")$elevation, probs = 0.5, na.rm = T) - quantile(subset(MC_elevations, type == "Current")$elevation, probs = 0.5, na.rm = T)
quantile(subset(ZF_elevations, type == "RPC8.5")$elevation, probs = 0.5, na.rm = T)- quantile(subset(ZF_elevations, type == "Current")$elevation, probs = 0.5, na.rm = T)

# Shift in the leading edge RPC 8.5:
quantile(subset(MC_elevations, type == "RPC8.5")$elevation, probs = 0.95, na.rm = T) - quantile(subset(MC_elevations, type == "Current")$elevation, probs = 0.95, na.rm = T)
quantile(subset(ZF_elevations, type == "RPC8.5")$elevation, probs = 0.95, na.rm = T) - quantile(subset(ZF_elevations, type == "Current")$elevation, probs = 0.95, na.rm = T)

#####


# 6. Check the models
#####################


# This function evaluate our models by split resampling with a calibration on 80% and an evaluation on 20% of the data

speed.Eval.Bin.Split <- function(model, data, yname, predictors, ratio, nrep, type) {    
  
  ### Packages
  require(PresenceAbsence)  
  
  ### Start spliting and evaluation
  Evaluation <- c()
  for (k in 1:nrep) {
    
    ### Split data
    T1 <- data[which(data[,yname]==1),]
    sample.1 <- sample(seq(from=1, to=dim(T1)[1]), size=round(ratio*dim(T1)[1]), replace=FALSE, prob=NULL)
    train.1 <- T1[c(sample.1),]
    test.1 <- T1[-c(sample.1),]
    T0 <- data[which(data[,yname]==0),]
    sample.0 <- sort(sample(seq(from=1,to=dim(T0)[1]), size=round(ratio*dim(T0)[1]), replace=FALSE, prob=NULL))
    train.0 <- T0[c(sample.0),]
    test.0 <- T0[-c(sample.0),]
    split.train <- rbind(train.1, train.0)
    split.test <- rbind(test.1, test.0)
    
    ### Random permutation of the data  
    split.train <- split.train[sample(dim(split.train)[1], replace=F),]
    split.test <- split.test[sample(dim(split.test)[1], replace=F),]
    
    ### Define weigths
    wgth <- split.train[,yname]
    wgth[wgth==0] <- round(sum(wgth)/length(which(wgth==0)),5)
    
    ### Update model
    split.model <- dismo::maxent(x=split.train[,predictors,drop=FALSE], p=split.train[,yname], w=wgth)

    ### Prediction
    Predicted <- predict(split.model, split.test, type="response")
    Observed <- split.test[,yname]
    
    ### Evaluations
    require(ecospat)
    Boyce <- ecospat.boyce(Predicted, Predicted[which(Observed==1)], PEplot=F)$cor #Normally Spearman.cor but not here 
    x <- data.frame(Id=1:length(Observed), Observed=Observed, Predicted=Predicted)
    Tresholds <- c("MaxSens+Spec","MaxKappa","MinROCdist","ReqSens","ReqSens","ReqSens","ReqSens","ReqSens")
    REQsens <- c(NA,NA,NA,0.9,0.95,0.975,0.99,1)
    Eval <- c()
    for (t in 1:length(Tresholds)) {
      Tresh <- optimal.thresholds(x, opt.methods=paste0(Tresholds[t]), req.sens=REQsens[t])[[2]]
      Eva <- presence.absence.accuracy(x, th=Tresh)[2:7]
      TSS <- (Eva[[3]]+Eva[[4]])-1  
      Replicat <- k
      Type <- Tresholds[t]
      Eva <- cbind(as.data.frame(Type), as.data.frame(Replicat), Eva, as.data.frame(TSS), as.data.frame(Boyce))
      Eval <- rbind(Eval, Eva)
    }
    Eval$Type <- c("MaxSens+Spec","MaxKappa","MinROCdist","ReqSens_0.9","ReqSens_0.95","ReqSens_0.975","ReqSens_0.99","ReqSens_1")
    Evaluation <- data.frame(rbind(Evaluation, Eval))
  }
  return(Evaluation)
}


# 1. M. celadussa

# Preapare the data frames 
model.max_MC <- (MC_mx[[1]])  # the Maximum entropy model (MaxEnt)
Data_SDM_MC <-  (MC_mx[[2]])  # the pseudo-absence data and the presence data

# Run the function
EvalSplit.max_MC <- speed.Eval.Bin.Split(model=model.max_MC, data=Data_SDM_MC, yname="presBg", 
                                         predictors=c("bio5", "bio2", "bio12", "bio15"), ratio=0.8, 
                                         nrep=5, type="response")

# Visualize the data 
head(EvalSplit.max_MC)
EvalSplit.max2_MC <- tidyr::gather(data = EvalSplit.max_MC, key="ID", value = "NB", 
                                   threshold, PCC, sensitivity, specificity, Kappa, AUC, TSS, Boyce)
ggplot(EvalSplit.max2_MC, aes(x=ID, y=NB, fill = ID)) + geom_boxplot() + ggtitle("M. celadussa")


# 2. Z. filipendulae

# Preapare the data frames 
model.max_ZF <- (ZF_mx[[1]])  # the Maximum entropy model (MaxEnt)
Data_SDM_ZF <-  (ZF_mx[[2]])  # the pseudo-absence data and the presence data

# Run the function
EvalSplit.max_ZF <- speed.Eval.Bin.Split(model=model.max_ZF, data=Data_SDM_ZF, yname="presBg", 
                                         predictors=c("bio5", "bio2", "bio12", "bio15"), ratio=0.8, 
                                         nrep=5, type="response")

# Visualize the data 
head(EvalSplit.max_ZF)
EvalSplit.max2_ZF <- tidyr::gather(data = EvalSplit.max_ZF, key="ID", value = "NB", 
                                   threshold, PCC, sensitivity, specificity, Kappa, AUC, TSS, Boyce)
ggplot(EvalSplit.max2_ZF, aes(x=ID, y=NB, fill = ID)) + geom_boxplot() +ggtitle("Z. filipendulae")

#####


# 7. Details of the R session
#############################

sessioninfo::session_info()

#─ Session info ────────────────────────────────────────────────────────────────
#setting  value
#version  R version 4.2.2 (2022-10-31)
#os       Ubuntu 22.04.1 LTS
#system   x86_64, linux-gnu
#ui       RStudio
#language (EN)
#collate  en_US.UTF-8
#ctype    en_US.UTF-8
#tz       Europe/Zurich
#date     2024-01-26
#rstudio  2022.11.999-dev+999 Spotted Wakerobin (desktop)
#pandoc   2.9.2.1 @ /usr/bin/pandoc

#─ Packages ────────────────────────────────────────────────────────────────────
#package         * version  date (UTC) lib source
#abind             1.4-5    2016-07-21 [2] CRAN (R 4.2.2)
#backports         1.4.1    2021-12-13 [2] CRAN (R 4.2.2)
#boot              1.3-28   2021-05-03 [2] CRAN (R 4.2.2)
#broom             1.0.1    2022-08-29 [2] CRAN (R 4.2.2)
#car               3.1-1    2022-10-19 [2] CRAN (R 4.2.2)
#carData           3.0-5    2022-01-06 [2] CRAN (R 4.2.2)
#class             7.3-20   2022-01-16 [2] CRAN (R 4.2.2)
#classInt          0.4-8    2022-09-29 [2] CRAN (R 4.2.2)
#cli               3.6.1    2023-03-23 [2] CRAN (R 4.2.2)
#coda              0.19-4   2020-09-30 [2] CRAN (R 4.2.2)
#codetools         0.2-18   2020-11-04 [2] CRAN (R 4.2.2)
#colorspace        2.0-3    2022-02-21 [2] CRAN (R 4.2.2)
#data.table        1.14.4   2022-10-17 [2] CRAN (R 4.2.2)
#DBI               1.1.3    2022-06-18 [2] CRAN (R 4.2.2)
#DHARMa          * 0.4.6    2022-09-08 [1] CRAN (R 4.2.2)
#digest            0.6.30   2022-10-18 [2] CRAN (R 4.2.2)
#dismo           * 1.3-9    2022-09-19 [2] CRAN (R 4.2.2)
#dplyr           * 1.1.2    2023-04-20 [2] CRAN (R 4.2.2)
#e1071             1.7-12   2022-10-24 [2] CRAN (R 4.2.2)
#ecospat         * 4.0.0    2023-10-17 [1] CRAN (R 4.2.2)
#elevatr         * 0.99.0   2023-09-12 [1] CRAN (R 4.2.2)
#emmeans         * 1.8.2    2022-10-27 [2] CRAN (R 4.2.2)
#enmSdmX         * 1.1.2    2023-09-08 [1] CRAN (R 4.2.2)
#estimability      1.4.1    2022-08-05 [2] CRAN (R 4.2.2)
#fansi             1.0.3    2022-03-24 [2] CRAN (R 4.2.2)
#foreach           1.5.2    2022-02-02 [2] CRAN (R 4.2.2)
#generics          0.1.3    2022-07-05 [2] CRAN (R 4.2.2)
#ggplot2         * 3.4.0    2022-11-04 [2] CRAN (R 4.2.2)
#ggpubr          * 0.4.0    2020-06-27 [2] CRAN (R 4.2.2)
#ggsignif          0.6.4    2022-10-13 [2] CRAN (R 4.2.2)
#glue              1.6.2    2022-02-24 [2] CRAN (R 4.2.2)
#gtable            0.3.1    2022-09-01 [2] CRAN (R 4.2.2)
#iterators         1.0.14   2022-02-05 [2] CRAN (R 4.2.2)
#KernSmooth        2.23-20  2021-05-03 [2] CRAN (R 4.2.2)
#lattice         * 0.20-45  2021-09-22 [2] CRAN (R 4.2.2)
#lifecycle         1.0.3    2022-10-07 [2] CRAN (R 4.2.2)
#lme4              1.1-31   2022-11-01 [2] CRAN (R 4.2.2)
#magrittr          2.0.3    2022-03-30 [2] CRAN (R 4.2.2)
#MASS              7.3-58.1 2022-08-03 [2] CRAN (R 4.2.2)
#Matrix            1.5-1    2022-09-13 [2] CRAN (R 4.2.2)
#minqa             1.2.5    2022-10-19 [2] CRAN (R 4.2.2)
#munsell           0.5.0    2018-06-12 [2] CRAN (R 4.2.2)
#mvtnorm           1.1-3    2021-10-08 [2] CRAN (R 4.2.2)
#nlme              3.1-160  2022-10-10 [2] CRAN (R 4.2.2)
#nloptr            2.0.3    2022-05-26 [2] CRAN (R 4.2.2)
#pillar            1.9.0    2023-03-22 [2] CRAN (R 4.2.2)
#pkgconfig         2.0.3    2019-09-22 [2] CRAN (R 4.2.2)
#plyr            * 1.8.7    2022-03-24 [2] CRAN (R 4.2.2)
#PresenceAbsence * 1.1.11   2023-01-07 [1] CRAN (R 4.2.2)
#progressr         0.13.0   2023-01-10 [2] CRAN (R 4.2.2)
#proxy             0.4-27   2022-06-09 [2] CRAN (R 4.2.2)
#purrr             1.0.1    2023-01-10 [2] CRAN (R 4.2.2)
#R6                2.5.1    2021-08-19 [2] CRAN (R 4.2.2)
#raster          * 3.6-14   2023-01-16 [2] CRAN (R 4.2.2)
#Rcpp              1.0.9    2022-07-08 [2] CRAN (R 4.2.2)
#rgdal           * 1.6-7    2023-05-31 [2] CRAN (R 4.2.2)
#rJava           * 1.0-6    2021-12-10 [1] CRAN (R 4.2.2)
#rlang             1.1.1    2023-04-28 [2] CRAN (R 4.2.2)
#Rmisc           * 1.5.1    2022-05-02 [1] CRAN (R 4.2.2)
#rstatix           0.7.0    2021-02-13 [2] CRAN (R 4.2.2)
#rstudioapi        0.14     2022-08-22 [2] CRAN (R 4.2.2)
#scales            1.2.1    2022-08-20 [2] CRAN (R 4.2.2)
#sessioninfo       1.2.2    2021-12-06 [2] CRAN (R 4.2.2)
#sf              * 1.0-13   2023-05-24 [2] CRAN (R 4.2.2)
#sp              * 1.5-1    2022-11-07 [2] CRAN (R 4.2.2)
#terra           * 1.6-47   2022-12-02 [2] CRAN (R 4.2.2)
#tibble            3.2.1    2023-03-20 [2] CRAN (R 4.2.2)
#tidyr             1.2.1    2022-09-08 [2] CRAN (R 4.2.2)
#tidyselect        1.2.0    2022-10-10 [2] CRAN (R 4.2.2)
#units             0.8-0    2022-02-05 [2] CRAN (R 4.2.2)
#utf8              1.2.2    2021-07-24 [2] CRAN (R 4.2.2)
#vctrs             0.6.3    2023-06-14 [2] CRAN (R 4.2.2)
#withr             2.5.0    2022-03-03 [2] CRAN (R 4.2.2)
#xtable            1.8-4    2019-04-21 [2] CRAN (R 4.2.2)

#[1] /cluster/raid/home/bovayb/R/x86_64-pc-linux-gnu-library/4.2
#[2] /cluster/software/bioinformatic/R/4.2.2/lib/R/library

#───────────────────────────────────────────────────────────────────────────────
 

#####


