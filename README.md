# Human Rights Watch quantitative analysis of the criminalization of unhoused people in Los Angeles, California

Code by Brian Root; report authored by John Raphling.

## Introduction
This repository includes the analytical code that was used for all data analyses included in the Human Rights Watch report on the criminalization of unhoused people, released in August 2024. This repository includes raw data that was provided by city agencies (e.g. Los Angeles Police Department, Los Angeles Sanitation, and Los Angeles Homeless Services Authority) in response to California Public Records Act (PRA) requests or from the city’s open records repository.  The folders include additional coding data used in data processing and all analytical code used to produce our findings. The analysis scripts have been annotated to explain what each snippet is producing. All code is in R.


## Doc
The “doc” folder contains all of Human Rights Watch’s original California Public Records Act requests to city and/or city/county agencies.


## Data
The folder "import/input" contains the raw data as provided by city agencies. The following describes the raw data:
#### Los Angeles Police Department
- "import/input/LAPD/structured data/ARRESTS AND RFCS IDENTIFIED AS HOMELESS.xlsx" is raw data provided to HRW by LAPD in response to a public records act request for data on all arrests of unhoused people between 1/1/2016 and 12/31/2022.

- "import/input/LAPD/structured data/Arrest_Data_downloaded5_9_23.csv" and "import/input/LAPD/structured data/Arrest_Data_through2019_downloaded.csv" are not included in this repository because of their size. They contain data on all LAPD arrests downloaded from the city open records [portal]( https://data.lacity.org/). You must download the arrest data (two files for pre and post 2020) from the portal.

- "import/input/LAPD/structured data/CRIMES WITH HOMELESS AS VICTIM_R.xlsx" is raw data provide in response to the PRA request regarding known offenses with an unhoused victim.

-   "import/input/LAPD/structured data/Crime_Data_from_2020_to_Present.csv" and "import/input/LAPD/structured data/Crime_Data_from_2010_to_2019_20231107.csv" are LAPD datasets on reported crime downloaded from the city open records [portal]( https://data.lacity.org/). They are too large to include in this repository and must be downloaded from the portal.

-   "import/input/LAPD/structured data/USE OF FORCE_R.xlsx" was provided in response to our PRA request for all data on use of force and whether the subject was unhoused.

-   "import/input/LAPD/structured data/41.18_Arrests_2021_-_2023 - Raw File.csv" contains data on 41.18 arrests downloaded from the LA Controller’s [site]( https://controller.lacity.gov/landings/4118).

#### Los Angeles Homeless Services Agency (LAHSA)

- "import/input/LAHSA/Request 1 - Services and locations starting 2019-01-01.xlsx" contains data on services provided to unhoused people by LAHSA staff and was provided to HRW by LAHSA in response to our PRA request.

- "import/input/LAHSA/PRK Clients Served - Entire Program History.xlsx" is also in response to our PRA request and provides data on clients of LAHSA’s Project Roomkey program.

- "import/input/LAHSA/29192 - HRW PRA on ISP.xlsx" is also in response to our PRA request and provides data on clients of LAHSA’s Inside Safe program.

#### Los Angeles Sanitation (LASAN)

- "import/input/LASAN/WPIMS Data - 201910010_20230217_ver2.xls" includes data provided by LASAN to HRW in response to our PRA request for all information about cleanings related to the unhoused population including CARE, CARE+, Operation Healthy Streets, Special Enforcement Cleaning Zones and other LAMC 41.18 enforcement.


#### Housing Authority of the City of Los Angeles (HACLA)

These files were provided to HRW by HACLA in response to our PRA request.


#### “Frozen” subfolders
Any sub-folders titled “../frozen” contain additional data, typically created by Human Rights Watch, used for processing or coding the original data. For example, “processing/frozen/offenses_recoded.csv” contains Human Rights Watch’s coding of the raw offense data provided by LAPD. 


## Processing

This folder contains the scripts in the “processing/src” folder and additional datafiles needed to process the data in the “processing/frozen” folder. There are separate processing scripts for data from each agency. Each processing script is annotated to describe what each code snippet is doing. Within the processing scripts, locations are joined with additional geographic data which was processed outside of the R code using various geocoding apis and functions in GIS software. Geocoded addresses and locations are brought into the processing scripts via the “frozen” folder.
“processing_other” contains processing code for processing the LAHSA point-in-time unhoused count as well as census data for the overall city population.
“tidy_census_processing” contains a script for downloading and cleaning data downloaded from the US Census Bureau and CPI inflation data.
After running all of the processing scripts, the “processing/output” folder will be filled with processed data.


## Geo-matching

This folder contains scripts and data for joining the processed data (in “processing/output”) with geographic data that was generated outside of the R environment in GIS software. The script is in “geo_matching/src” and the datafiles are in “geo_matching/frozen”.  This code then outputs processed, geocoded and matched data into “geo_matching/output”. This is the data that is used by the descriptive analysis scripts.  Because some agencies jitter their location data in order to maintain anonymity, all locations were matched if they were within a 250m radius of each other.


## Analysis
The repository includes all scripts for taking processed data and performing descriptive analyses. This code cannot be run unless the processing and geo_matching code is run first because the scripts rely on data outputted by those functions. The following scripts are in “descriptives/src”:

####  joined_descriptives
This file includes the code for any analyses that required joining the datasets from different agencies based on date or location. This includes analyses in the report that describe what agencies did at a specific location (e.g. Echo Park, Naomi St encampment) 

#### lapd_descriptives
This script contains all code for analysis of LAPD data on arrests of both unhoused and housed people including rates of arrest and analysis of charges.

#### lapd_use_of_force_descriptives
This script contains all code for analysis of LAPD data on the use of force.

#### lapd_unhoused_victims
This script contains the code for analyzing LAPD crime data including crimes with unhoused victims.

#### lasan_descriptives
This code provides descriptive statistics of LASAN cleanings including amount of materials thrown away and bags taken to storage.

#### lahsa_roomkey_descriptives
This code provides descriptive statistics of LAHSA’s Project Roomkey program including hotel occupancy rates.

#### lahsa_inside_safe
This code provides both descriptive statistics of the data LAHSA provided on services related to the Inside Safe program. The file includes analyses that are included in the final report as well as exploratory analysis of the data which was not included.

#### HACLA_descriptives
This script takes in processed census data and input data from HACLA to analyze public housing in the city. 

#### rent_burden
This script queries the US Census Bureau api to download and analyze data on home renting, ownership and burden. You will need to have your census api key in your .R environment.

#### Zillow
This script analyses data downloaded from [Zillow]( https://www.zillow.com/research/data/) on housing prices in the city. The data described in the inputfiles section will need to be downloaded by the user from Zillow.


## Contact
Please contact Brian Root at rootb@hrw.org or dilresearch@hrw.org with any questions.
![image](https://github.com/HumanRightsWatch/USP_los_angeles_unhoused_report/assets/6944960/c6b4ce12-1402-4bd0-9f49-3c4e10148cde)
