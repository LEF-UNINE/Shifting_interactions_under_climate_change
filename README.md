# Shifting_interactions_under_climate_change

This is a repository for the code and the data used for the paper "**Adapting to change: exploring the consequences of climate-induced host plant shifts in two specialist Lepidoptera species**" written by Baptiste Bovay, Patrice Descombes, Yannick Chittaro, Gaétan Glauser, Hanna Nomoto and Sergio Rasmann.

The code is diveded in three scripts:
- Main_analyses.R (contain the analyses done for the main main text of the paper)
- Supplementary_analyses.R (contain the analyes done for the supplementary document of the paper)
- Species_distribution_models.R (contain the script for the creation of the model used in the paper)

The main script (Main_analyses.R) uses 6 dataframes, one for each section with a corresponding name:
- Field_experiment.csv
- Caterpillar_preference.csv
- Caterpillar_performance.csv
- Diapause.csv
- Wing_size.csv
- Secondary_metabolites.csv

The script for the supplementary material (Supplementary_analyses.R) uses 2 dataframes, one for each section with a corresponding name:
- Plant_traits.csv
- Temperature.csv

The dataframes for the script for the models (Species_distribution_models.R) and for the analyses ofhistorical species distribution (section 3 of the code Supplementary_analyses.R) are not published online as occurence data are protected.However, with a good justification, they could be requested to National Data and Information Center on the Swiss Fauna (www.infofauna.ch). 

## Dataframe information

### Field_experiment.csv

This dataframe contains the data of an host choice experiment performed in the Swiss alps at two different elevation (1500 m and 2150 m). The aim of this experiment was to see whether low-elevation lepidoptera are able to shift to a new host plant from higher elevation or not. Data were collected during June and July 2021 by Baptiste Bovay. This dataframe contains the following variables:

1. Plant: the plant species used
2. Elevation: The elevation where the plant was tranlocated
3. ID: an unique ID for each plant
4. Lepidoptera: THe species of Lepidoptera used for the host choice experiment
5. Cage: The ID of the cage where the replicate take place
6. Success: tell if there was any egg patches laid in the cage (N = no and Y = yes)
7. Patchs: The number of egg patch laid on the plant
8. Eggs: the total numbe rof eggs laid on the plant
9. Presence: tell if there was any egg patches laid in the plant (0 = no and 1 = yes)





