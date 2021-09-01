# Checklist for Habitat Documentation

General
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

### Instructions for when the data *source* is modified
1. Upload available data source documentation (in pdf) to the AWS s3 bucket named [“cvpiahabitat-r-package/cvpia-sit-model-inputs”](https://s3.console.aws.amazon.com/s3/buckets/cvpiahabitat-r-package/cvpia-sit-model-inputs/?region=us-west-2&tab=overview) and confirm that the permission level is set to public read access and storage class is "Standard-IA" (long-lived, infrequently accessed data)
2. Click on the document in AWS and copy paste the link provided into the respective Rmd file under the "Data Source" section (add pages numbers where data are located) **and** within the modeling description  if applicable

## Modeling Description
Collaborate with Mark Tompkins to write modeling description including the following types of information:
* what type of modeling was done
* what software was used
* how/where/when the data were collected
* who collected it
* etc.

## Tables
Include a table of spawning and instream rearing data. If there are many species/lifestage combinations, consider splitting these tables up to make the width more manageable.

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



## Miscellaneous
* If the data improvements specified in the “Future Data Improvements” section are completed, delete the relevant bullet points
**Spreadsheet location:** DSMhabitat > data-raw > modeling_exists.csv

## Modeling_exists Spreadsheet Specifications
**Spreadsheet description:** This file indicates if modeling exists for each species and life stage for each watershed. The columns are broken into Fall Run (FR), Late Fall Run (LFR), Winter Run (WR), Spring Run (SR), and Steelhead (ST), and then broken into spawning, fry, juvenile, and for Steelhead, adult (ST_adult). There are also 3 columns that specify whether or not rearing, spawning, or floodplain regional approximations were used in the absence of modeling data. **It is important to update this file if new modeling becomes available**.

Encoding:
* **NA:** the species is not present in the watershed
* **FALSE:** the species is present, but habitat modeling does not exist for the stream – typically estimated using a proxy species or scaling method
* **TRUE:** the species is present and habitat modeling exists

1. Put new data (csv) file within DSMhabitat > data-raw > watershed > [watershed_name] > data
2. Check data availability & structures (units, columns, organization, column headers, additional scaling calculations, etc.) to make sure appropriate for being read into R
* if more information is needed to use the data, reach out to the person/organization who provided the data
* if there are organizational errors you can fix yourself for R compatibility, do so
3. Open respective Rmd file: DSMhabitat > data-raw > watershed > [watershed_name] > [watershed_name].Rmd
4. Update path being read into the `read_csv` with new file name
5. Edit code as necessary to accommodate new data format and be able to create a table with appropriate column headers, units, filled in rows, etc.
6. Check that the column titles in the tables conform to naming conventions (see below), and that all column titles are represented in the "Header Descriptions" section above each table
7. Build the plots and check that they look accurate: the species names/life stages in the plots are correct, units are correct, axis titles are correct, color coding is correct, data visualization looks reasonable
6. If the source includes data on a species not previously represented, ensure that the species is added to data tables and plots
7.
8. Update modeling_exists spreadsheet if necessary (see above for details)


