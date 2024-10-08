# Shifting_interactions_under_climate_change

This is a repository for the code and the data used for the paper "**Adapting to change: exploring the consequences of climate-induced host plant shifts in two specialist Lepidoptera species**" written by Baptiste Bovay, Patrice Descombes, Yannick Chittaro, Gaétan Glauser, Hanna Nomoto and Sergio Rasmann.

The code is divided into four scripts:
- Main_analyses.R (contain the analyses done for the main text of the paper)
- Supplementary_analyses.R (contains the analyses done for the supplementary document of the paper)
- Lepido_historical_migration.R (contains the analyses for the historical shift in elevation)
- Species_distribution_models.R (contains the script for the species distribution models)

The main script (Main_analyses.R) uses 6 data frames, one for each section with a corresponding name:
- Field_experiment.csv
- Caterpillar_preference.csv
- Caterpillar_performance.csv
- Pupation_rate.csv
- Wing_size.csv
- Secondary_metabolites.csv

The script for the supplementary material (Supplementary_analyses.R) uses 2 data frames, one for each section with a corresponding name:
- Plant_traits.csv
- Temperature.csv

The data frames for the species distribution models (Species_distribution_models.R) and for the calculation of the historical shift in elevation are not published online as occurrences data are protected. However, with a good justification they could be requested to the National Data and Information Center on the Swiss Fauna (www.infofauna.ch). 

## Scripts information

### Main_analyses.R

This script contains the analyses made for the paper "Adapting to change: exploring the consequences of climate-induced host plant shifts in two specialist Lepidoptera species", excluding the analyses presented in the supplementary material and excluding the species distribution models and the estimation of the historical shift in elevations.

This script is divided into 1 + 7 sections. The introduction section (Packages, theme and functions) describes the packages used and sets up the themes for the plots and a function used in the script. The first section of analyses (Field experiment) contains the code for the field experiment which aimed to understand whether adult Lepidoptera prefer to lay eggs on high- or low-elevation plants. The second section (Caterpillar preference) contains the analyses of a feeding choice experiment of caterpillars of two Lepidoptera species. The third section (Caterpillar performance) contains the analyses of a performance experiment which aimed to see any differences in growth between caterpillars feeding on a high- or a low-elevation host plant. The fourth section (Pupation rate) contains the code which allows us to estimate the differences in pupation rate between *Melitaea celadussa* caterpillars feeding either on high- or low-elevation host plants. The fifth section (Wing size) contains the code which allows us to estimate the differences in wing size between *M. celadussa* caterpillars feeding either on high- or low-elevation host plants. The sixth section (Chemical analyses) aims to test whether high- and low-elevation pairs of congeneric species have differences in defensive compound concentration in the leaves. Finally, the seventh section (Details of the R session) contains all the information about the R session used to run these analyses.

### Supplementary_analyses.R

This script contains the analyses made for the supplementary material of the paper "Adapting to change: exploring the consequences of climate-induced host plant shifts in two specialist Lepidoptera species", excluding the species distribution models and the estimation of the historical shift in elevations.

This script is divided into 1 + 2 sections. The introduction section (Packages, theme and functions) describes the packages used and sets up the themes for the plots. The first section of analyses (Plant traits) contains the code for the analyses of the traits measured on the plants used for the field experiment and a on few other individuals (see "Material and methods"). This section includes PCA and PERMANOVA analyses on a set of traits (herbivory, size, number of leaves, SLA, number of stems and number of inflorescences) and analyses using a linear model for the C:N ratio of the plants. The second section (Temperature analyses) allows us to plot the average daily temperature recorded on the site during the field experiment. Finally, the third section (Details of the R session) contains all the information about the R session used to run these analyses.

### Lepido_historical_migration.R

This script contains the analyses made for the main and the supplementary material for the paper "Adapting to change: exploring the consequences of climate-induced host plant shifts in two specialist Lepidoptera species" regarding the the historical shift of two Lepidoptera in higher elevations. 

This script is divided into 1 + 4 sections. The introduction section (Packages, theme and functions) describes the packages used and sets up the themes for the plots. The first section of analyses (Plant traits) includes the loading and the check of the occurrence data. The second section aims to fill the gaps when elevation data are missing. Finally, the third section estimates the migration rates of our focal lepidoptera species according to the methods used by Vitasse et al., 2021. Finally, the fourth section (Details of the R session) contains all the information about the R session used to run these analyses.

### Species_distribution_models.R

This script contains the species distribution models under current and future climatic conditions (average 2070 - 2100, RPC 4.5 and RPC 8.5) for two Lepidoptera species: *Melitaea celadussa* and *Zygaena filipendulae*.

This script is divided into 1 + 7 sections. The introduction section (Packages, theme and functions) describes the packages used and sets up the themes for the plots. The first section of analyses (Load and check the data) includes the loading of the rasters and occurrence data as well as their formatting and some verifications. The second section (Function to build the SDM) sets up a function which allows the creation of a Maximum Entropy distribution model. The third section (Model creation) runs the function built in the previous section and checks the variables contribution to the models as well as the correlation between the variables. The fourth section (Maps and model predictions) allows us to draw maps with the prediction of occurrence under diffrent climatic scenarios. The fifth section (Extraction of the elevations) extracts the elevation for each climatic niche. The sixth section (Check the models) evaluates our models by split resampling with a calibration on 80% and an evaluation on 20% of the data. Finally, the seventh section (Details of the R session) contains all the information about the R session used to run these analyses.

## Dataframe information

### Field_experiment.csv

This data frame contains the data of a host choice experiment performed with adults Lepidoptera in the Swiss Alps at two different elevations (1500 m and 2150 m). This experiment aimed to see whether low-elevation Lepidoptera are able to shift to a new host plant from a higher elevation or not. Data were collected during June and July 2021 by Baptiste Bovay. This data frame contains the following variables:

1. Plant: the plant species used
2. Elevation: The elevation where the plant was translocated (high = 2150 m; low = 1500 m)
3. ID: a unique ID for each plant
4. Lepidoptera: The species of Lepidoptera used for the host choice experiment
5. Cage: The ID of the cage where the replicate took place
6. Success: tell if there were any egg patches laid in the cage (N = no and Y = yes)
7. Patchs: The number of egg patches laid on the plant
8. Eggs: the total number of eggs laid on the plant
9. Presence: tell if there were any egg patches laid in the plant (0 = no and 1 = yes)

### Caterpillar_preference.csv

This data frame contains the data of a host choice experiment performed with caterpillars in controlled conditions. This experiment aimed to see whether low-elevation Lepidoptera are able to shift to a new host plant from a higher elevation or not. Data were collected between July and November 2021 by Baptiste Bovay. This data frame contains the following variables:

1. ID: a unique ID for each caterpillar
2. Come_from: The plant on which the caterpillar was reared
3. Food_plant: The plant species tested
4. Time: The duration of the experiment (hours)
5. Size_before: The size of the leave before the experiment (cm^2)
6. Size_after: The size of the leave after the experiment (cm^2)
7. Lepidoptera: The species of Lepidoptera used for the host choice experiment

### Caterpillar_performance.csv

This data frame contains the data of a performance experiment performed with caterpillars in controlled conditions. This experiment aimed to see the growth of caterpillars when feeding on low- or high-elevation host plants translocated either at low- or high-elevation. Data were collected between July and August 2021 by Baptiste Bovay. This data frame contains the following variables:

1. ID: a unique ID for each caterpillar
2. Lepidoptera: The species of Lepidoptera used for the host choice experiment
3. Plant: The plant species tested
4. Elevation: The elevation at which the tested plant was translocated (high = 2150 m; low = 1500 m)
5. Time: The duration of the experiment (day)
6. Weight_1: The Weight of the caterpillar before the test (g)
7. Weight_2: The Weight of the caterpillar after the test (g)

### Pupation_rate.csv

This data frame contains the data of a rearing experiment performed with caterpillars of *M. celadussa* in controlled conditions. This experiment aimed to see the influence of low- or high-elevation host plants on the diapause. Data were collected between August and October 2021 by Baptiste Bovay. This data frame contains the following variables:

1. ID: a unique ID for each caterpillar
2. Cage: an ID for each cage used (each cage contained 3 caterpillars)
3. Plant: The plant species used as food in the cage
4. Week: The week on which the caterpillar enters diapause
5. Pupation: tell if goes rather in pupation or diapause (0 = diapause and 1 = pupation)

### Wing_size.csv

This data frame contains the data of a rearing experiment performed with *M. celadussa* in controlled conditions. This experiment aimed to see whether the wing size of imago could change according to the host plant. Data were collected between July and October 2021 by Baptiste Bovay. This data frame contains the following variables:

1. ID: a unique ID for each butterfly
2. Plant: The plant species used as food
3. Generation: The generation of the butterfly in the rearing
4. Wing_1: The surface of the top-left wing (mm^2)
5. Wing_2: The surface of the top-right wing (mm^2)
6. Wing_3: The surface of the bottom-left wing (mm^2)
7. Wing_4: The surface of the bottom-right wing (mm^2)

### Secondary_metabolites.csv

This data frame contains the data of of secondary metabolites concentration measured in 4 plant species translocated in the Swiss Alps at two different elevations (1500 m and 2150 m). This experiment aimed to see whether congeneric low- and high-elevation plants produced different amounts of chemical defences and whether a translocation changed the amount of chemical defences. Leaves for the analyses were collected during June and August 2021 by Baptiste Bovay, chemical analyses were performed during October and November 2021 by Baptiste Bovay and Gaétan Glauser. This data frame contains the following variables:

1. Plant: The plant species used
2. Elevation: The elevation where the plant was translocated (high = 2150 m; low = 1500 m)
3. ID: a unique ID for each plant
4. Lepidoptera: The focal Lepidoptera species feeding on the plant
5. Cage: the ID of the cages where plants were translocated
6. Date: the date of the experimentation with butterflies (NA = the plant was not used in previous experiments)
7. Catapol: The concentration of catapol (relative concentration)
8. Aucubin: The concentration of aucubin (relative concentration)
9. Linamarin: The concentration of linamarin (mg/g)
10. Lautostralin: The concentration of lautostralin (mg/g)
11. Sample_weight: weight of the éeaf sample used for the analyses

### Plant_traits.csv

This data frame contains the data of several traits measured in 4 plant species translocated in the Swiss Alps at two different elevations (1500 m and 2150 m). The traits (with the exception of SLA and C:N ratio) were measured on-site during the field experiment during June and August 2021 by Baptiste Bovay and Hanna Nomoto. Leaves for SLA and the C:N ration were collected during June and August 2021 by Baptiste Bovay and Hanna Nomoto. Analyses were performed during August to November 2021 by Baptiste Bovay. This data frame contains the following variables:

1. Plant: The plant species used
2. Elevation: the elevation where the plant was translocated (high = 2150 m; low = 1500 m)
3. ID: a unique ID for each plant
4. Lepidoptera: The focal Lepidoptera species feeding on the plant
5. Cage: the ID of the cages where plants were translocated
6. Date: the date of the experimentation with butterflies (NA = the plant was not used in previous experiments)
7. SLA: the Specific Leaf Area (mm^2/mg)
8. CN_ratio: the Carbon-Nitrogen ratio in the leaves (ratio)
9. Height: the height of the plant from the soil (cm)
10. Leaves: the number of leaves of the plant
11. Stems: the number of stems of the plant
12. Inflorescences: the number of inflorescences of the plant
13. Herbivory: the number of herbivory spots present on the plant
14. Herbivory.damages: the proportion of the plant eaten (categorical)
15. Herbivory.normalised: the proportion of the plant eaten (%)

### Temperature.csv

This data frame contains the temperature data recorded at our sites (1500 m and 2150 m) in the Swiss Alps. Data were collected with HOBO dataloggers between June and September 2021. This data frame contains the following variables:

1. ID: a unique ID for each observation
2. Elevation: the elevation where the plant was translocated (high = 2150 m; low = 1500 m)
3. Date: the date of the observation (yyyy-mm-dd)
4. Hour: the hour of the observation (hh:mm:ss)
5. Temperature: the temperature recorded (°F)
