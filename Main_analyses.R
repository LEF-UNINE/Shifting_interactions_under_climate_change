
# Code Shifting interactions under climate change
#################################################

# Code last update: 2024-01-12
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
setwd("\\\\home\\bovayb$\\PhD\\Paper master thesis\\Paper for resubmisson\\Analyses for github")
getwd()

# Packages
library(dplyr)     # To modify the data
library(ggplot2)   # To make the plots
library(car)       # Use to analyse the models
library(DHARMa)    # Check the assumption of the models
library(Rmisc)     # To summaries the data (mean, sd, se and CI)
library(emmeans)   # To make post-hoc analyses
library(lmerTest)  # To build linear mixed effect models

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

# Without the labels in italic
my_theme2 <- my_theme +
  theme(axis.text.x =element_text(size=si,face = "plain"))

# Color stes for graphics:
ColM <- c("#1A4B0A", "#56CE56")
ColZ <- c("steelblue4", "steelblue2")

# Function that replace negative values by NA: 
Remove_neg <- function(v) {
  caterp <- c()
  for(i in v){
    if(i <= 0){a <- NA}
    else{a <- i}
    caterp <- c(caterp, a)
  }
  return(caterp)
}

#----


# 1. Field experiment
#####################

# Data preparation:
# - - - -  - - - - -

# Import the oviposition and traits dataset
Field_experiment <- read.csv("Field_experiment.csv", h = T, sep = ";")
head(Field_experiment)

# To keep only the cages where Lepidoptera made a choice
Field_experiment <- subset(Field_experiment, Success == "Y")          

# Order the factors
Field_experiment$Plant <- factor(Field_experiment$Plant, levels =
                     c("P. lanceolata", "P. atrata", "L. corniculatus", "L. alpinus"))
Field_experiment$Lepidoptera <- factor(Field_experiment$Lepidoptera, levels = c("M. celadussa", "Z. filipendulae"))

# Create a dataset for each butterfly
Melitaea <- subset(Field_experiment, Lepidoptera == "M. celadussa")
Zygaena  <- subset(Field_experiment, Lepidoptera == "Z. filipendulae")


# Visualize the data
#- - - - - - - - - - -

# Plot the oviposition probability of M. celadussa

# Calculate the mean and the standard error (se)
sum_up_dataM <- summarySE(Melitaea, measurevar="Presence", groupvars=c("Plant"),na.rm = TRUE)

# Plot the mean and the se
propM <-ggplot(sum_up_dataM, aes(x=Plant, y=Presence, fill=Plant, colour=Plant)) + ggtitle("M. celadussa") +
  geom_errorbar(aes(ymin=Presence-se, ymax=Presence+se,color=Plant), width=0.2, linewidth=1.4, position=position_dodge(0.5)) +
  geom_line(position=position_dodge(0.5)) + scale_y_continuous(breaks=c(0, 0.5, 1), labels=c("0.0", "0.5", "1.0"), limits=c(0,1)) + 
  scale_fill_manual(values=c(ColM, ColZ)) + 
  scale_color_manual(guide ="none",values=c(ColM, ColZ)) + labs(fill="Plant species:") +
  geom_point(position=position_dodge(0.5), size=7.2,shape=21) + ylab("Oviposition probability") + xlab(NULL) +
  my_theme + theme(plot.title = element_text(face = "italic")) ; propM

# Plot the oviposition probability of M. celadussa

# Calculate the mean and the standard error (se)
sum_up_dataZ <- summarySE(Zygaena, measurevar="Presence", groupvars=c("Plant"),na.rm = TRUE)

# Plot the mean and the se
propZ <-ggplot(sum_up_dataZ, aes(x=Plant, y=Presence, fill=Plant, colour=Plant)) + ggtitle("Z. filipendulae") +
  geom_errorbar(aes(ymin=Presence-se, ymax=Presence+se,color=Plant), width=0.2, linewidth=1.4, position=position_dodge(0.5)) +
  geom_line(position=position_dodge(0.5)) + scale_y_continuous(breaks=c(0, 0.5, 1), labels=c("0.0", "0.5", "1.0"), limits=c(0,1)) + 
  scale_fill_manual(values=c(ColZ))+ 
  scale_color_manual(guide ="none",values=c(ColZ))+ labs(fill="Plant species:")+
  geom_point(position=position_dodge(0.5), size=7.2,shape=21)+ylab("Oviposition probability")+ xlab(NULL) +
  my_theme + theme(plot.title = element_text(face = "italic")) ; propZ

# Plot the number of egg patches for M. celadussa

# Calculate the mean and the standard error (se)
bar_plot_1M<- summarySE((Melitaea), measurevar="Patchs", groupvars=c("Plant"),na.rm = TRUE)

# Plot the mean and the se
barplot1M <- ggplot(bar_plot_1M, aes(y = Patchs, x = Plant, fill = Plant, color = Plant)) + ggtitle("M. celadussa") +
  geom_bar(stat = "identity",  width = 0.7, position=position_dodge(), size = 1)+  ylim(min = 0, max = 2)+
  geom_errorbar(aes(ymin=Patchs-se, ymax=Patchs+se), width=0.2, linewidth=1.4, position=position_dodge(0.7)) +
  xlab(NULL) +  scale_fill_manual(values = c(ColM, ColZ)) +   my_theme +  
  scale_color_manual(values = rep("#000000", 4)) + ylab("N° of egg patches")+ theme(plot.title = element_text(face = "italic")) ; barplot1M

# Plot the number of egg patches for Z. filipendulae

# Calculate the mean and the standard error (se)
bar_plot_1Z<- summarySE((Zygaena), measurevar="Patchs", groupvars=c("Plant"),na.rm = TRUE)

# Plot the mean and the se
barplot1Z <- ggplot(bar_plot_1Z, aes(y = Patchs, x = Plant, fill = Plant, color = Plant)) + ggtitle("Z. filipendulae") +
  geom_bar(stat = "identity",  width = 0.7, position=position_dodge(), size = 1)+ylim(min = 0, max = 2)+
  geom_errorbar(aes(ymin=Patchs-se, ymax=Patchs+se), width=0.2, linewidth=1.4, position=position_dodge(0.7)) +
  xlab(NULL) +  scale_fill_manual(values = c(ColZ)) +   my_theme +  
  scale_color_manual(values = rep("#000000", 4)) + ylab("N° of egg patches")+ theme(plot.title = element_text(face = "italic")) ; barplot1Z


# Statistical analyses of the field experiment
# - - - - - - - - - - - - - - - - - - - - - - -

# 1. Melitaea

# Built a GLM model with a Poisson distribution for the number of egg patches
m1 <- glm(Patchs~Plant*Elevation, family = poisson(), data = Melitaea)

# Assumption check
plot(simulateResiduals(m1))                            # Test the assumption of the residuals
par(mfrow = c(2, 2)) ; plot(m1) ; par(mfrow = c(1, 1)) # Classic assumptions check

# Analyse the model
summary(m1) # Intercepts and slopes
Anova(m1)   # Analyse of variance 

# Built a GLM model with a binomial distribution for the oviposition probability
m2 <- glm(Presence~Plant*Elevation, family =binomial(), data = Melitaea)

# Assumptions test
plot(simulateResiduals(m2))                            # Test the assumption of the residuals
par(mfrow = c(2, 2)) ; plot(m2) ; par(mfrow = c(1, 1)) # Classic assumptions check

# Analyse the model
summary(m2) # Intercepts and slopes
Anova(m2)   # Analyse of variance


# 2. Zygaena

# Built a GLM model with a Poisson distribution for the number of egg patches
z1 <-glm(Patchs~Plant*Elevation, family = poisson(), data = Zygaena)

# Assumption check
plot(simulateResiduals(z1))                            # Test the assumption of the residuals
par(mfrow = c(2, 2)) ; plot(z1) ; par(mfrow = c(1, 1)) # Classic assumptions check

# Analyse the model
summary(z1) # Intercepts and slopes
Anova(z1)   # Analyse of variance

# Built a GLM model with a binomial distribution for the oviposition probability
z2 <- glm(Presence~Plant*Elevation , family = binomial(), data = Zygaena)

# Assumption check
plot(simulateResiduals(z2))                            # Test the assumption of the residuals
par(mfrow = c(2, 2)) ; plot(z2) ; par(mfrow = c(1, 1)) # Classic assumptions check

# Analyse the model
summary(z2) # Intercepts and slopes
Anova(z2)   # Analyse of variance
#----


# 2. Caterpillar preference
###########################

# Data preparation:
# - - - -  - - - - -

CT <- read.csv("Caterpillar_preference.csv", h = T, sep = ";")
head(CT)

# calculate the quantity of leaf eaten by the caterpillars and select all the caterpillars that feed on the leaves (that do not eat a negative surface)
CT$Quantity_eat <- Remove_neg(CT$Size_before - CT$Size_after) 
CT$Food_plant <- factor(CT$Food_plant, levels =   c("P. lanceolata", "P. atrata", "L. corniculatus", "L. alpinus"))


# Make a separate database for each Lepidoptera species 
CTM <- subset(CT, Lepidoptera == "M. celadussa")
CTZ <- subset(CT, Lepidoptera == "Z. filipendulae")


# Data visualization:
# - - - -  - - - - - -

# boxplot for M. celdaussa
choiceM <- ggplot(CTM, aes(y = (Quantity_eat), x = Food_plant, fill = Food_plant)) + 
  geom_boxplot(size = 1) + xlab(NULL) +  scale_fill_manual(values = ColM) + ylab("Surface eaten (mm^2)")+  
  my_theme + theme(legend.position = "none"); choiceM

# boxplot for Z. filipendulae
choiceZ <- ggplot(CTZ, aes(y = (Quantity_eat), x = Food_plant, fill = Food_plant)) + 
  geom_boxplot(size = 1) + xlab(NULL) + ylab("Surface eaten (mm^2)") + scale_fill_manual(values = ColZ) +  
  my_theme + theme(legend.position = "none"); choiceZ


# Statistical analyses of caterpillar preference
# - - - - - - - - - - - - - - - - - - - - - - - -

# 1. Melitaea

# Built a LM model for surface eaten in order the take into account the effect of the size of the leaves
m4 <- lm(log(Quantity_eat) ~ log(Size_before), data= CTM) 

# Assumption check
par(mfrow = c(2, 2)) ; plot(m4) ; par(mfrow = c(1, 1))
plot(simulateResiduals(m4)) 

# Extract the residuals
CTM$area_eaten_res <- m4$residuals + coef(m4)["(Intercept)"] 

#	Fit the a model accounting for size of the leaves by using the residuals
m5 <- lmer((area_eaten_res)~Food_plant*(1|Come_from), data= CTM)

# Assumption check
par(mfrow = c(2, 2)) ; plot(m5) ; par(mfrow = c(1, 1))
plot(simulateResiduals(m5))

# Analyse the model 
summary(m5)
Anova(m5)


# 2. Zygaena

# Built a LM model for surface eaten in order the take into account the effect of the size of the leaves
CTZ <- subset(CTZ, Quantity_eat > 0) # To remove the NA
z4 <- lm(log(Quantity_eat) ~ log(Size_before), data= CTZ)

# Assumption check
par(mfrow = c(2, 2)) ; plot(z4) ; par(mfrow = c(1, 1))
plot(simulateResiduals(z4)) 

# Extract the residuals
CTZ$area_eaten_res <- z4$residuals + coef(z4)["(Intercept)"]# Extract residuals: 

#	Fit the a model accounting for size of the leaves by using the residuals
z5 <- lmer((area_eaten_res)~Food_plant*(1|Come_from), data= CTZ)

# Assumption check
par(mfrow = c(2, 2)) ; plot(z5) ; par(mfrow = c(1, 1))
plot(simulateResiduals(z5)) 

# Analyse the model 
summary(z5)
Anova(z5)

#----


# 3. Caterpillar performance  
############################

# Data preparation:
# - - - -  - - - - -

# Import the data
PT <- read.csv("Caterpillar_performance.csv", h = T, sep = ";")
head(PT)

# Set up the data
PT$Plant <- as.factor(PT$Plant)
PT$Plant <- factor(PT$Plant, levels = c("P. lanceolata", "P. atrata", "L. corniculatus", "L. alpinus"))
PT$Elevation <- as.factor(PT$Elevation)

# Calculate the weight taken during the experiment
PT$growth <- ((PT$Weight_2 - PT$Weight_1)/as.numeric(PT$Time))

# remove negative growth rate (they indicate that the caterpillar do not feed)
PT$growth <- Remove_neg(PT$growth)

# change the units of the growth rate (from g/day to microg/day)
PT$growth <- (PT$growth)*1000000

# Make a separate database for each Lepidoptera species 
PTM <- subset(PT, Lepidoptera == "M. celadussa")
PTZ <- subset(PT, Lepidoptera == "Z. filipendulae")

# Data visualization:
# - - - -  - - - - - -

# Boxplot for the growth of M. celadussa caterpillars
perfM <- ggplot(PTM, aes(x = Elevation, y = log10(growth), fill = Plant)) + geom_boxplot(size = 1)+
  ylim(ymin=1.4, ymin = 3.3) + scale_fill_manual(values = c(ColM))+ labs(fill="")+
  ylab(expression(paste("Growth (log 10 of ", mu,"g/day)"))) + xlab(NULL) +
  ggtitle(expression(paste(italic("M. celadussa")))) + my_theme2 ; perfM 

# Boxplot for the growth of Z. filipendulae caterpillars
perfz <- ggplot(PTZ, aes(x = Elevation, y = log10(growth), fill = Plant)) + geom_boxplot(size = 1)+ 
  ylim(ymin=1.4, ymin = 3.3) + scale_fill_manual(values = c(ColZ)) + labs(fill="")+
  ylab(expression(paste("Growth (log 10 of ", mu,"g/day)"))) + xlab(NULL) +
  ggtitle(expression(paste(italic("Z. filipendulae")))) + my_theme2 ; perfz


# Statistical analyses of caterpillar performance
# - - - - - - - - - - - - - - - - - - - - - - - -

# 1. Melitaea

# Built a LM model for growth rate with a log transformation
m3 <-lm(log(growth)~Plant*Elevation , data = PTM)

# Assumption check
plot(simulateResiduals(m3)) 
par(mfrow = c(2, 2)) ; plot(m3) ; par(mfrow = c(1, 1)) 

# Analyse the model
summary(m3)
Anova(m3) 

# Posthoc test
plot(pairs(emmeans(m3, ~ Plant * Elevation))) ; pairs(emmeans(m3, ~ Plant * Elevation))

# to extract the mean and the CI
summarySE((PTM), measurevar="growth", groupvars=c("Elevation", "Plant"),na.rm = TRUE)


# 2. Zygaena 

# Built a LM model for growth rate with a sqrt transformation
z3 <-lm(log(growth+1)~Plant*Elevation , data = PTZ)

# Assumption check
plot(simulateResiduals(z3)) 
par(mfrow = c(2, 2)) ; plot(z3) ; par(mfrow = c(1, 1)) 

# Analyse the model
summary(z3)
Anova(z3) 

# Posthoc test
plot(pairs(emmeans(z3, ~ Plant * Elevation))) ; pairs(emmeans(z3, ~ Plant * Elevation))

# to extract the mean and the CI
summarySE((PTZ), measurevar="growth", groupvars=c("Elevation", "Plant"),na.rm = TRUE)

#----


# 4. Pupation rate
##################

# Data preparation:
# - - - -  - - - - -

# Import growth data
Pupation_rate <- read.csv("Pupation_rate.csv", h = T, sep = ";")
Pupation_rate$Plant <- factor(Pupation_rate$Plant, levels =  c("P. lanceolata", "P. atrata"))


# Data visualization:
# - - - -  - - - - - -

# Diapause (= 1-pupation) probability 
sum_up_data3 <- summarySE((Pupation_rate), measurevar="Pupation", groupvars=c("Plant"),na.rm = TRUE)
pupation <-ggplot(sum_up_data3, aes(x=Plant, y=Pupation, fill=Plant, colour=Plant)) + 
  geom_errorbar(aes(ymin=Pupation-se, ymax=Pupation+se, color=Plant), width=0.2, size=1.4, position=position_dodge(0.5)) +  
  ylim(ymin= -0.05, ymax = 1) + xlab(NULL) +
  geom_line(position=position_dodge(0.5)) +   ggtitle("Pupation probability") +
  scale_fill_manual(values=c(ColM))+  scale_y_continuous(breaks=c(0, 0.5, 1), labels=c("0.0", "0.5", "1.0"), limits=c(-0.001,1)) +
  scale_color_manual(guide ="none",values=c(ColM))+
  geom_point(position=position_dodge(0.5), size=7.2,shape=21)+ylab(expression("Proportion of pupation"))+ xlab(expression(NULL)) +
  my_theme  +theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) ; pupation


# Statistical analyses of the pupation rate
# - - - - - - - - - - - - - - - - - - - - -

# Diapause/Pupation model

# Built a Binomial GLM model for diapause probability
d1 <- glm(Pupation ~ Plant, data= Pupation_rate, family = binomial)

# Assumption check
plot(simulateResiduals(d1))
par(mfrow = c(2, 2)) ; plot(d1) ; par(mfrow = c(1, 1))

# Analyse the model
summary(d1)
Anova(d1)

#----


# 5. Wing size
##############

# Data preparation:
# - - - -  - - - - -

# Import the wing size dataset
wings <- read.csv("Wing_size.csv", h = T, sep = ";")
head(wings)

# Set up correctly the factors
wings$Plant <- factor(wings$Plant, levels = c("P. lanceolata", "P. atrata"))

# Calculate the total wing area
wings$tot <- (wings$Wing1 + wings$Wing2 + wings$Wing3 + wings$Wing4)


# Data visualization:
# - - - -  - - - - - -

fig4 <- ggplot(wings, aes(x = Plant, y= tot/100, fill = Plant)) + geom_boxplot(size = 1) +
  scale_fill_manual(values = c(ColM)) + ylab("Wings surface (cm^2)") +     
  my_theme + xlab(NULL) +
  theme(legend.position = "none") ; fig4


# Statistical analyses of the Wing size
# - - - - - - - - - - - - - - - - - - -

# Built a LM model for the wing area 
w1 <- lm(sqrt(tot) ~ Plant, data = wings)

# Assumption check
plot(simulateResiduals(w1)) 
par(mfrow = c(2, 2)) ; plot(w1) ; par(mfrow = c(1, 1)) 

# Analyse the model
summary(w1)
Anova(w1) 

#----


# 6. Chemical analyses 
######################

# Data
#- - -

Secondary_metabolites <- read.csv("Secondary_metabolites.csv", h = T, sep = ";")
head(Secondary_metabolites)

# Transform the column to numeric data
Secondary_metabolites$Plant <- as.factor(Secondary_metabolites$Plant)
Secondary_metabolites$Elevation <- as.factor(Secondary_metabolites$Elevation)
Secondary_metabolites$Plant <- factor(Secondary_metabolites$Plant, levels = c("P. lanceolata", "P. atrata", "L. corniculatus", "L. alpinus"))

# Create a database for the IGs and the CNGs
IGs <-  filter(Secondary_metabolites, Plant == "P. lanceolata" | Plant == "P. atrata")
CNGs <- filter(Secondary_metabolites, Plant == "L. corniculatus" | Plant == "L. alpinus")


# Secondary metabolites analyses
#- - - - - - - - - - - - - - - -

# 1. CNGs

# Plot the data
linamarin <- ggplot(CNGs, aes(x = Plant, y = log10(Linamarin), fill = Plant)) + geom_boxplot(size = 1)+
  scale_fill_manual(values = c(ColZ)) +
  ylab("Concentration log(µg/mg)") + ylim(ymin = -3, ymax = 1.5) +
  my_theme +  xlab(NULL)+
  ggtitle("Linamarin") ; linamarin

lotaustralin <- ggplot(CNGs, aes(x = Plant, y = log10(Lotaustralin), fill = Plant)) + geom_boxplot(size = 1)+
  scale_fill_manual(values = c(ColZ)) +
  ylab("Concentration log(µg/mg)") + ylim(ymin = -3, ymax = 1.5) +
  my_theme +  xlab(NULL)+
  ggtitle("Lotaustralin") ; lotaustralin

# Create models for CNGs concentrations with a normal distribution 
# (addition of "+0.0001" so that the 0 are not remove thy the log transformation)
limanarin1 <- lm(log(Linamarin + 0.0001) ~ Plant * Elevation, data = CNGs)
lotaustralin1 <- lm(log(Lotaustralin + 0.0001) ~ Plant * Elevation, data = CNGs)

# Assumptions check
par(mfrow = c(2, 2)) ; plot(limanarin1) ; par(mfrow = c(1, 1))
plot(simulateResiduals(limanarin1)) 
par(mfrow = c(2, 2)) ; plot(limanarin1) ; par(mfrow = c(1, 1))
plot(simulateResiduals(lotaustralin1))

# Analyses of the model
summary(limanarin1)
Anova(limanarin1) 
summary(lotaustralin1)
Anova(lotaustralin1) 

# To have the means and the CI
summarySE((CNGs), measurevar="Linamarin", groupvars=c("Plant"),na.rm = TRUE)
summarySE((CNGs), measurevar="Lotaustralin", groupvars=c("Plant"),na.rm = TRUE)


# 2. IGs

# Transform the data so they are more "readable" (as they are relative values, they are very high)
IGs$Aucubin <- IGs$Aucubin/1000
IGs$Catapol <- IGs$Catapol/1000

# Plot the data

Aucubin <- ggplot(IGs, aes(x = Plant, y = Aucubin, fill = Plant)) + geom_boxplot(size = 1)+
  scale_fill_manual(values = c(ColM)) +
  ylab("Relative concentrations") + 
  my_theme +  xlab(NULL)+
  ggtitle("Aucubin") ; Aucubin

Catapol <- ggplot(IGs, aes(x = Plant, y = Catapol, fill = Plant)) + geom_boxplot(size = 1)+
  scale_fill_manual(values = c(ColM)) +
  ylab("Relative concentrations") + 
  my_theme +  xlab(NULL)+
  ggtitle("Catapol") ; Catapol

# Create models for CNGs concentrations with a normal distribution 
# (addition of "+0.1" so that the 0 are not remove thy the log transformation)
catapol1 <- lm(log(Catapol+0.1) ~ Plant * Elevation, data = IGs)
aucubin1 <- lm(log(Aucubin+0.1) ~ Plant * Elevation, data = IGs)

# Assumptions check
par(mfrow = c(2, 2)) ; plot(catapol1) ; par(mfrow = c(1, 1))
plot(simulateResiduals(catapol1)) 
par(mfrow = c(2, 2)) ; plot(aucubin1) ; par(mfrow = c(1, 1))
plot(simulateResiduals(aucubin1)) 

# Analyses of the model
summary(catapol1)
Anova(catapol1)
summary(aucubin1)
Anova(aucubin1) 

# To have the means and the CI
summarySE((IGs), measurevar="Catapol", groupvars=c("Plant"),na.rm = TRUE)
summarySE((IGs), measurevar="Aucubin", groupvars=c("Plant"),na.rm = TRUE)

#----


# 7. Details of the R session
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
#date     2024-01-12
#rstudio  2023.12.0+369 Ocean Storm (desktop)
#pandoc   NA

#─ Packages ────────────────────────────────────────────────────────────────────
#package      * version    date (UTC) lib source
#abind          1.4-5      2016-07-21 [1] CRAN (R 4.3.0)
#boot           1.3-28.1   2022-11-22 [2] CRAN (R 4.3.2)
#car          * 3.1-2      2023-03-30 [1] CRAN (R 4.3.1)
#carData      * 3.0-5      2022-01-06 [1] CRAN (R 4.3.1)
#cli            3.6.1      2023-03-23 [1] CRAN (R 4.3.1)
#codetools      0.2-19     2023-02-01 [2] CRAN (R 4.3.2)
#colorspace     2.1-0      2023-01-23 [1] CRAN (R 4.3.1)
#crayon         1.5.2      2022-09-29 [1] CRAN (R 4.3.1)
#DHARMa       * 0.4.6      2022-09-08 [1] CRAN (R 4.3.1)
#digest         0.6.33     2023-07-07 [1] CRAN (R 4.3.1)
#doParallel     1.0.17     2022-02-07 [1] CRAN (R 4.3.1)
#dplyr        * 1.1.2      2023-04-20 [1] CRAN (R 4.3.1)
#ellipsis       0.3.2      2021-04-29 [1] CRAN (R 4.3.1)
#emmeans      * 1.8.7      2023-06-23 [1] CRAN (R 4.3.1)
#estimability   1.4.1      2022-08-05 [1] CRAN (R 4.3.0)
#fansi          1.0.4      2023-01-22 [1] CRAN (R 4.3.1)
#farver         2.1.1      2022-07-06 [1] CRAN (R 4.3.1)
#fastmap        1.1.1      2023-02-24 [1] CRAN (R 4.3.1)
#foreach        1.5.2      2022-02-02 [1] CRAN (R 4.3.1)
#gap            1.5-3      2023-08-26 [1] CRAN (R 4.3.1)
#gap.datasets   0.0.6      2023-08-25 [1] CRAN (R 4.3.1)
#generics       0.1.3      2022-07-05 [1] CRAN (R 4.3.1)
#ggplot2      * 3.4.2      2023-04-03 [1] CRAN (R 4.3.1)
#glue           1.6.2      2022-02-24 [1] CRAN (R 4.3.1)
#gtable         0.3.3      2023-03-21 [1] CRAN (R 4.3.1)
#htmltools      0.5.6      2023-08-10 [1] CRAN (R 4.3.1)
#httpuv         1.6.11     2023-05-11 [1] CRAN (R 4.3.1)
#iterators      1.0.14     2022-02-05 [1] CRAN (R 4.3.1)
#labeling       0.4.2      2020-10-20 [1] CRAN (R 4.3.0)
#later          1.3.1      2023-05-02 [1] CRAN (R 4.3.1)
#lattice      * 0.22-5     2023-10-24 [1] CRAN (R 4.3.2)
#lifecycle      1.0.3      2022-10-07 [1] CRAN (R 4.3.1)
#lme4         * 1.1-34     2023-07-04 [1] CRAN (R 4.3.1)
#lmerTest     * 3.1-3      2020-10-23 [1] CRAN (R 4.3.1)
#magrittr       2.0.3      2022-03-30 [1] CRAN (R 4.3.1)
#MASS           7.3-60     2023-05-04 [2] CRAN (R 4.3.2)
#Matrix       * 1.6-0      2023-07-08 [1] CRAN (R 4.3.1)
#mgcv           1.9-0      2023-07-11 [2] CRAN (R 4.3.2)
#mime           0.12       2021-09-28 [1] CRAN (R 4.3.0)
#minqa          1.2.5      2022-10-19 [1] CRAN (R 4.3.1)
#munsell        0.5.0      2018-06-12 [1] CRAN (R 4.3.1)
#mvtnorm        1.2-2      2023-06-08 [1] CRAN (R 4.3.1)
#nlme           3.1-163    2023-08-09 [2] CRAN (R 4.3.2)
#nloptr         2.0.3      2022-05-26 [1] CRAN (R 4.3.1)
#numDeriv       2016.8-1.1 2019-06-06 [1] CRAN (R 4.3.0)
#pillar         1.9.0      2023-03-22 [1] CRAN (R 4.3.1)
#pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.3.1)
#plyr         * 1.8.8      2022-11-11 [1] CRAN (R 4.3.1)
#promises       1.2.1      2023-08-10 [1] CRAN (R 4.3.1)
#qgam           1.3.4      2021-11-22 [1] CRAN (R 4.3.1)
#R6             2.5.1      2021-08-19 [1] CRAN (R 4.3.1)
#rbibutils      2.2.15     2023-08-21 [1] CRAN (R 4.3.1)
#Rcpp           1.0.11     2023-07-06 [1] CRAN (R 4.3.1)
#Rdpack         2.5        2023-08-21 [1] CRAN (R 4.3.1)
#rlang          1.1.1      2023-04-28 [1] CRAN (R 4.3.1)
#Rmisc        * 1.5.1      2022-05-02 [1] CRAN (R 4.3.1)
#rstudioapi     0.15.0     2023-07-07 [1] CRAN (R 4.3.1)
#scales         1.2.1      2022-08-20 [1] CRAN (R 4.3.1)
#sessioninfo    1.2.2      2021-12-06 [1] CRAN (R 4.3.2)
#shiny          1.7.5      2023-08-12 [1] CRAN (R 4.3.1)
#tibble         3.2.1      2023-03-20 [1] CRAN (R 4.3.1)
#tidyselect     1.2.0      2022-10-10 [1] CRAN (R 4.3.1)
#utf8           1.2.3      2023-01-31 [1] CRAN (R 4.3.1)
#vctrs          0.6.3      2023-06-14 [1] CRAN (R 4.3.1)
#withr          2.5.0      2022-03-03 [1] CRAN (R 4.3.1)
#xtable         1.8-4      2019-04-21 [1] CRAN (R 4.3.1)

#[1] C:/Users/Bovayb/AppData/Local/R/win-library/4.3
#[2] C:/Program Files/R/R-4.3.2/library

#───────────────────────────────────────────────────────────────────────────────
#----
