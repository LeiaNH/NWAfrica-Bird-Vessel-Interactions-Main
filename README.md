# NWAfrica-Bird-Vessel-Interactions-Main

This repository includes the data and R scripts used to reproduce the Boosted Regression Tree (BRT) models and main figures from the study "Seabird–vessel interactions in industrial fisheries of Northwest Africa: implications for international bycatch management" (Navarro-Herrero et al., 2025). In this study, we combined high-resolution seabird tracking data with information on industrial fishing and non-fishing vessels to study how nine seabird species interact with fisheries in Northwest African waters.

The scripts here focus on modeling interactions for three key seabird species. The dataset already includes seabird–vessel interactions (both encounters and attendances), along with all the covariates needed for the models. After downloading the dataset from [xxxxx], you can run the scripts to carry out the full analysis.

To run the full modeling workflow—including data exploration, model tuning, fitting, and bootstrap resampling—use the mainBRT.R script. The results (plots, tables, and summaries) will be saved in the /output/ folder. To create the main figures shown in the paper, run the mainFigures.R script after the modeling is complete.

### Sample datasets

The sample datasets are available in the `data/input` folder and follow the structure outlined below:

input folder            |  Description    
----------------------- | -------------------
xxxx                    | xxxx                    
             

`Data` folder (with the input folder and the output folder derived from this repository), can be downloaded using the following link:
xxxxx

### Code

The scripts are located in the `code` folder. The workflow can be easily followed by running the four main scripts listed below, each of which connects to additional scripts as needed:

code folder             |  Description    
----------------------- | -------------------
`setup.R`               | R script to setup your WD and load libraries 
BRT                     | R scripts linked in `mainBRT.R`  
Figures                 | R scripts linked in `mainFigures.R`

Below is the recommended order for running the main scripts:

1. Edit and run `setup.R` to set your working directory, and ensure that you have installed all packages listed.

2. Run `mainBRT.R` to train the models and test on a shared testing dataset.

5. Run `mainFigures.R` to obtain main figures.

This workflow offers a comprehensive guide for replicating the key steps of our analysis, from initial data preparation to advanced modeling and prediction, and most representative figures from the study.

