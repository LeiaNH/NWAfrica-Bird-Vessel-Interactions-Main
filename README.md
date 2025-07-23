# NWAfrica-Bird-Vessel-Interactions-Main

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16327282.svg)](https://doi.org/10.5281/zenodo.16327282)

This repository includes the data and R scripts used to reproduce the Boosted Regression Tree (BRT) models and main figures from the study "Seabird–vessel interactions in industrial fisheries of Northwest Africa: implications for international bycatch management" (Navarro-Herrero et al., 2025). In this study, we combined high-resolution seabird tracking data with AIS vessel tracking data from industrial fishing and non-fishing vessels to investigate how nine seabird species interact with fisheries in Northwest African waters.

The scripts provided here focus on modelling interactions for three seabird species that frequently attend fishing vessels. The dataset includes seabird–vessel interactions (both encounters and attendances), along with all the covariates required for the models and figures.

### Sample datasets

The sample datasets are available in the `data/input` folder and follow the structure outlined below:

input folder            |  Description    
----------------------- | -------------------
CALBOR_inputBRT_L1.rds  | This dataset records seabird-vessel interactions (encounters/attendances) of Cory's shearwater used for BRT and derived figures.  
CALEDW_inputBRT_L1.rds  | This dataset records seabird-vessel interactions (encounters/attendances) of Cape Verde shearwater used for BRT and derived figures.  
LARAUD_inputBRT_L1.rds  | This dataset records seabird-vessel interactions (encounters/attendances) of Audoui'ns gull used for BRT and derived figures.  
events.rds              | This dataset records seabird–vessel interactions (encounter/attendance) used for figure 3 and 6.


`Data` folder (with the input folder and the output folder derived from this repository), can be downloaded using the following link:
https://www.dropbox.com/scl/fo/m6eg8s774jxio9rz4nphr/AHCzUjrWhnwa9jPkIZ1ENI4?rlkey=wmfme00bic3r4vlvj4c27r8nh&dl=1

### Code

To run the full modeling workflow—including data exploration, model tuning, fitting, and bootstrap resampling—use the mainBRT.R script. The required input file is *_inputBRT_L1.rds. Each BRT input dataset includes environmental (e.g., SST, CHL), spatial (e.g., depth, distance to coast), and vessel(e.g., vessel density, gear type) covariates, along with a binary response indicating attendance (response 1) or encounter (response 0). 

To generate the main figures shown in the paper, run the mainFigures.R script after the modeling is complete. This script depends on the output from mainBRT.R, and also uses events.rds to produce Figures 3 and 6.

The results (plots, tables, and summaries) will be saved in the /output/ folder. 

The scripts are located in the `code` folder. The workflow can be easily followed by running the four main scripts listed below, each of which connects to additional scripts as needed:

code folder             |  Description    
----------------------- | -------------------
`setup.R`               | R script to setup your WD and load libraries 
BRT                     | R scripts linked in `mainBRT.R`  
Figures                 | R scripts linked in `mainFigures.R`

Below is the recommended order for running the main scripts:

1. Edit and run `setup.R` to set your working directory, and ensure that you have installed all packages listed.

2. Run `mainBRT.R` to train the models and test on a shared testing dataset, and generate relevant figures.

5. Run `mainFigures.R` to produce the main figures shown in the paper (Figures 3–6).



