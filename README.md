# Shifting_interactions_under_climate_change

This is a repository for the code and the data used for the paper "**Adapting to change: exploring the consequences of climate-induced host plant shifts in two specialist Lepidoptera species**" written by Baptiste Bovay, Patrice Descombes, Yannick Chittaro, Gaétan Glauser, Hanna Nomoto and Sergio Rasmann.

The code is divided into three scripts:
- Main_analyses.R (contain the analyses done for the main text of the paper)
- Supplementary_analyses.R (contains the analyses done for the supplementary document of the paper)
- Species_distribution_models.R (contains the script for the creation of the model used in the paper)

The main script (Main_analyses.R) uses 6 data frames, one for each section with a corresponding name:
- Field_experiment.csv
- Caterpillar_preference.csv
- Caterpillar_performance.csv
- Diapause.csv
- Wing_size.csv
- Secondary_metabolites.csv

The script for the supplementary material (Supplementary_analyses.R) uses 2 data frames, one for each section with a corresponding name:
- Plant_traits.csv
- Temperature.csv

The data frames for the script for the models (Species_distribution_models.R) and for the analyses of historical species distribution (section 3 of the code Supplementary_analyses.R) are not published online as occurences data are protected. However, with a good justification, they could be requested to the National Data and Information Center on the Swiss Fauna (www.infofauna.ch). 

## Dataframe information

### Field_experiment.csv

This data frame contains the data of a host choice experiment performed in the Swiss Alps at two different elevations (1500 m and 2150 m). This experiment aimed to see whether low-elevation Lepidoptera are able to shift to a new host plant from a higher elevation or not. Data were collected during June and July 2021 by Baptiste Bovay. This data frame contains the following variables:

1. Plant: the plant species used
2. Elevation: The elevation where the plant was translocated
3. ID: a unique ID for each plant
4. Lepidoptera: The species of Lepidoptera used for the host choice experiment
5. Cage: The ID of the cage where the replicate took place
6. Success: tell if there were any egg patches laid in the cage (N = no and Y = yes)
7. Patchs: The number of egg patches laid on the plant
8. Eggs: the total number of eggs laid on the plant
9. Presence: tell if there were any egg patches laid in the plant (0 = no and 1 = yes)





