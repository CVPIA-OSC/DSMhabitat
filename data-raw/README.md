# Checklist for Habitat Documentation

## Style

* Run spellcheck
* Write descriptions in third person (no we)
* Check for grammar mistake of “data are” instead of “data is”
* Check that acronyms are defined upon their first mention in the document
(ex: SIT = Science Integration Team, WUA = weighted usable area, DSM = Decision Support Model)
* make all hyperlinks target="blank"
* Capitalize Steelhead, Fall Run Chinook, Late Fall Run Chinook etc.

## Data Source

Format data source as "Creator YYYY" or "Creator YYYY (pgs x-y)" this text should be linked to pdf of documentation stored on s3
* Check that current data source is correct. Try and find actual data to confirm. 

### Instructions for when the data source is modified
1. Upload available data source documentation (in pdf) to the AWS s3 bucket named [“cvpiahabitat-r-package/cvpia-sit-model-inputs”](https://s3.console.aws.amazon.com/s3/buckets/cvpiahabitat-r-package/cvpia-sit-model-inputs/?region=us-west-2&tab=overview) and confirm that the permission level is set to public read access and storage class is "Standard-IA" (long-lived, infrequently accessed data)
2. Click on the document in AWS and copy paste the link provided into the respective Rmd file under the "Data Source" section (add pages numbers where data are located) **and** within the modeling description  if applicable

## Modeling Description

Collaborate with Mark Tompkins to write modeling description including the following types of information:

* what type of modeling was done
* what software was used
* how/where/when the data were collected
* who collected it
* etc.

## Code 

* Check that file being read is appropriately described in data source description.  
* All data should be in WUA (square_ft/1000 ft) or if HSI square meters. Confirm that we made the appropriate conversions. 

## Tables
Include a table of spawning and instream rearing data. If there are many species/lifestage combinations, consider splitting these tables up to make the width more manageable. (Recommendation: max 11 columns)

Include a table of floodplain habitat data
For each table, include a caption with the header descriptions. Use the names listed below.

**Table Header Naming Conventions**

* flow_cfs: flow in cubic feet per second
* watershed: section of stream modeled for CVPIA SDM
* FR_spawn + (_wua or _sqm): Fall Run Chinook Spawning Weighted Usable Area or Square Meters
* FR_juv + (_wua or _sqm): Fall Run Chinook Juvenile Weighted Usable Area or Square Meters
* FR_fry + (_wua or _sqm): Fall Run Chinook Fry Weighted Usable Area or Square Meters
* LFR_spawn + (_wua or _sqm): Late Fall Run Chinook Spawning Weighted Usable Area or Square Meters
* LFR_juv + (_wua or _sqm): Late Fall Run Chinook Juvenile Weighted Usable Area or Square Meters
* LFR_fry + (_wua or _sqm): Late Fall Run Chinook Fry Weighted Usable Area or Square Meters
* WR_spawn + (_wua or _sqm): Winter Run Chinook Spawning Weighted Usable Area or Square Meters
* WR_juv + (_wua or _sqm): Winter Run Chinook Juvenile Weighted Usable Area or Square Meters
* WR_fry + (_wua or _sqm): Winter Run Chinook Fry Weighted Usable Area or Square Meters
* SR_spawn + (_wua or _sqm): Spring Run Chinook Spawning Weighted Usable Area or Square Meters
* SR_juv + (_wua or _sqm): Spring Run Chinook Juvenile Weighted Usable Area or Square Meters
* SR_fry + (_wua or _sqm): Spring Run Chinook Fry Weighted Usable Area or Square Meters
* ST_spawn + (_wua or _sqm): Steelhead Spawning Weighted Usable Area or Square Meters
* ST_juv + (_wua or _sqm): Steelhead Juvenile Weighted Usable Area or Square Meters
* ST_fry + (_wua or _sqm): Steelhead Fry Weighted Usable Area or Square Meters
* ST_adult + (_wua or _sqm): Steelhead Adult Weighted Usable Area or Square Meters
* FR_floodplain_acres: Fall Run Chinook Floodplain Acres
* LFR_floodplain_acres: Late Fall Run Chinook Floodplain Acres
* WR_floodplain_acres: Winter Run Chinook Floodplain Acres
* SR_floodplain_acres: Spring Run Chinook Floodplain Acres
* ST_floodplain_acres: Steelhead Floodplain Acres

Notes:  
1. Habitat is modeled in two ways: Weighted Usable Area or Habitat Suitability (square meters)


## Graphs

Include a plot for spawning, rearing (juv + fry), and floodplain. (3 plots if data for all lifestages) 

* Each plot should include a line for each species present in that watershed. If there are multiple species for instream rearing facet by lifestage. 
* The legend for each watershed should be titled "Species" and should list the full name of all species present in that watershed: Spring Run, Fall Run, Late Fall Run, Winter Run, Steelhead. 
* The x-axis should be Flow (cfs). 
* The y-axis should be either WUA (sqft/1000ft) or Suitable Habitat (acres) depending on data source. 
* Every plot should be styled in `theme_minimal()`
* Plot colors should be set using `scale_color_brewer(palette = 'Dark2')`
* When two species have the same habitat one species will overplot another line. In this case adjust the name in the legend to include all species with the same habitat as the same color. 


## Miscellaneous

If the data improvements specified in the “Future Data Improvements” section are completed, delete the relevant bullet points

Check all headers: 

* Use title case
* All plot sections should be titled with lifestage and habitat type (except floodplain is labeled "Floodplain Plot")
* TODO add if more rules come up 

Check that species described in markdown for each watershed are consistant with what is listed in `modeling_exists` spreadsheet ([see app](https://flowwest.shinyapps.io/habitat-modeling-availability/?_ga=2.235487553.1161673530.1630338714-1517480618.1613603367)): 

* **Modeling Exists Spreadsheet description:** This file indicates if modeling exists for each species and life stage for each watershed. The columns are broken into Fall Run (FR), Late Fall Run (LFR), Winter Run (WR), Spring Run (SR), and Steelhead (ST), and then broken into spawning, fry, juvenile, and for Steelhead, adult (ST_adult). There are also 3 columns that specify whether or not rearing, spawning, or floodplain regional approximations were used in the absence of modeling data. **It is important to update this file if new modeling becomes available**.

Encoding:

* **NA:** the species is not present in the watershed
* **FALSE:** the species is present, but habitat modeling does not exist for the stream – typically estimated using a proxy species or scaling method
* **TRUE:** the species is present and habitat modeling exists

## Checklist for Data Update: 
1) Add new data in a csv in the `data/` directory. If the `data/` directory does not exist yet please create one. 
2) Edit data source description to clearly describe new data source. 
3) Edit code in the [watershed].rmd file reading in new csv and updating R script to format as needed. (check units and reach scaling)
4) Knit document 
5) Navigate to `data-raw/modeling-exists.csv` and update if new modeling changes values in spreadsheet. 
6) Navigate to `data-raw/cache-data.R` and rerun code for any affected data (ex: if you change spawning habitat rerun entire spawning section of `cache-habitat.R`, ex: if you updated modeling exists rerun this section of `cache-habitat.R`)
7) Rebuild package (ctrl + shift + B)  and run all tests (ctrl + shift + T) for `DSMhabitat`, if a test breaks check that it is for an expected reason, and fix the test. 
8) Update `cvpia-habitat-markdown-doc` on s3 for the watershed
9) Update all repositories that are dependent on `DSMhabitat` values. (`DSMscenario` - max habitat values (in `DSMhabitat/data-raw/data_helpers.R`), Rerun `fallRunDSM`, `springRunDSM`, `winterRunDSM` `params`)
10) **If the watershed is used to calculate regional approximation you must also update the regional approximation**

### Dependency Graph

TODO: insert dependency graph in here
