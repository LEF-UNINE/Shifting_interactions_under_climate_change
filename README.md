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
- Pupation_rate.csv
- Wing_size.csv
- Secondary_metabolites.csv

The script for the supplementary material (Supplementary_analyses.R) uses 2 data frames, one for each section with a corresponding name:
- Plant_traits.csv
- Temperature.csv

The data frames for the script for the models (Species_distribution_models.R) and for the analyses of historical species distribution (section 3 of the code Supplementary_analyses.R) are not published online as occurrences data are protected. However, with a good justification, they could be requested to the National Data and Information Center on the Swiss Fauna (www.infofauna.ch). 

## Dataframe information

### Field_experiment.csv

This data frame contains the data of a host choice experiment performed with adults Lepidoptera in the Swiss Alps at two different elevations (1500 m and 2150 m). This experiment aimed to see whether low-elevation Lepidoptera are able to shift to a new host plant from a higher elevation or not. Data were collected during June and July 2021 by Baptiste Bovay. This data frame contains the following variables:

1. Plant: the plant species used
2. Elevation: The elevation where the plant was translocated
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
4. Elevation: The elevation at which the tested plant was translocated
5. Time: The duration of the experiment (day)
6. Weight_1: The Weight of the caterpillar before the test (g)
7. Weight_2: The Weight of the caterpillar after the test (g)

### Pupation_rate.csv

This data frame contains the data of a rearing experiment performed with caterpillars of Melitaea celadussa in controlled conditions. This experiment aimed to see the influence of low- or high-elevation host plants on the diapause. Data were collected between August and October 2021 by Baptiste Bovay. This data frame contains the following variables:

1. ID: a unique ID for each caterpillar
2. Cage: an ID for each cage used (each cage contained 3 caterpillars)
3. Plant: The plant species used as food in the cage
4. Week: The week on which the caterpillar enters diapause
5. Pupation: tell if goes rather in pupation or diapause (0 = diapause and 1 = pupation)

### Wing_size.csv

This data frame contains the data of a rearing experiment performed with Melitaea celadussa in controlled conditions. This experiment aimed to see whether the wing size of imago could change according to the host plant. Data were collected between July and October 2021 by Baptiste Bovay. This data frame contains the following variables:

1. ID: a unique ID for each butterfly
2. Plant: The plant species used as food
3. Generation: The generation of the butterfly in the rearing
4. Wing_1: The surface of the top-left wing (mm^2)
5. Wing_2: The surface of the top-right wing (mm^2)
6. Wing_3: The surface of the bottom-left wing (mm^2)
7. Wing_4: The surface of the bottom-right wing (mm^2)

### Secondary_metabolites.csv

This data frame contains the data of the concentration in secondary metabolites measured in 4 plant species translocated in the Swiss Alps at two different elevations (1500 m and 2150 m). This experiment aimed to see whether congeneric low- and high-elevation plants produced different amounts of chemical defences and whether a translocation changed the amount of chemical defences. Leaf for the analyses were collected during June and August 2021 by Baptiste Bovay, chemical analyses were performed during October and November 2021 by Baptiste Bovay and Gaétan Glauser. This data frame contains the following variables:

1. Plant: The plant species used
2. Elevation: The elevation where the plant was translocated
3. ID: a unique ID for each plant
4. Lepidoptera: The focal Lepidoptera species feeding on the plant
5. The ID of the cages where plants were translocated
6. Date: the date of the experimentation with butterflies (NA = the plant was not used in previous experiments)
7. Catapol: The concentration of catapol (relative concentration)
8. Aucubin: The concentration of aucubin (relative concentration)
9. Linamarin: The concentration of linamarin (mg/g)
10. Lautostralin: The concentration of lautostralin (mg/g)
11. Sample_weight: weight of the éeaf sample used for the analyses
