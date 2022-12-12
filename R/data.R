#' Habitat Data
#' @description Datasets used to calculate spawning, instream, and floodplain
#' rearing habitat areas for Chinook Salmon and Steelhead
#' @format NULL
#'
#' @section Instream (\{watershedname\}_instream):
#' Datasets containing the Weighted Usable Area (WUA) in square feet per 1000 feet
#' as a function of flow in cubic feet per second for Fall Run (FR), Spring Run (SR),
#' Winter Run (WR), and Steelhead (ST) depending on species occurance and modeling availability.
#'
#' Typical data structure for streams:
#' \itemize{
#'   \item{\strong{flow_cfs} - flow value in cubic feet per second}
#'   \item{\strong{FR_spawn_wua} - Spawning WUA in square feet per 1000 feet}
#'   \item{\strong{FR_fry_wua} - Fry (up to 50 mm) WUA in square feet per 1000 feet}
#'   \item{\strong{FR_juv_wua} - Juvenile WUA in square feet per 1000 feet}
#'   \item{\strong{watershed} - Name of watershed}
#' }
#'
#' @section Floodplain (\{watershedname\}_floodplain):
#' Datasets containing the total inuncated area in acres as a function of flow in cubic
#' feet per second for Fall Run (FR), Spring Run (SR), Winter Run (WR), and
#' Steelhead (ST) depending on species occurance and modeling availability.
#'
#' Typical data structure for streams:
#' \itemize{
#'   \item{\strong{flow_cfs} - Flow value in cubic feet per second}
#'   \item{\strong{FR_floodplain_acres} - Total inundated area in acres for Fall Run}
#'   \item{\strong{SR_floodplain_acres} - Total inundated area in acres for Spring Run}
#'   \item{\strong{ST_floodplain_acres} - Total inundated area in acres for Steelhead}
#'   \item{\strong{watershed} - Name of watershed}
#' }
#'
#' @section Modeling Details for Streams:
#' \itemize{
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/american_river.html}{American River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/antelope_creek.html}{Antelope Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/battle_creek.html}{Battle Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/bear_creek.html}{Bear Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/bear_river.html}{Bear River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/big_chico_creek.html}{Big Chico Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/butte_creek.html}{Butte Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/calaveras_river.html}{Calaveras River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/clear_creek.html}{Clear Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/cosumnes_river.html}{Cosumnes River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/cottonwood_creek.html}{Cottonwood Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/cow_creek.html}{Cow Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/deer_creek.html}{Deer Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/elder_creek.html}{Elder Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/feather_river.html}{Feather River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/merced_river.html}{Merced River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/mill_creek.html}{Mill Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/mokelumne_river.html}{Mokelumne River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/paynes_creek.html}{Paynes Creek}
#'   \item \link[=upper_sac_ACID_boards_in]{Sacramento River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/san_joaquin_river.html}{San Joaquin River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/stanislaus_river.html}{Stanislaus River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/stony_creek.html}{Stony Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/thomes_creek.html}{Thomes Creek}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/tuolumne_river.html}{Tuolumne River}
#'   \item \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/yuba_river.html}{Yuba River}
#' }
#'
#' @section Bypasses:
#' \itemize{
#'   \item \link[=bypass_habitat]{Sutter and Yolo Bypasses}
#' }
#'
#' @section Delta:
#' \itemize{
#'   \item \link[=delta_rearing_habitat]{North and South Delta}
#' }
#' 
#' @examples 
#' american_river_instream
#' american_river_floodplain
#' @name habitat_data
NULL

# STREAMS ----
# in alphabetical order

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"american_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"american_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"antelope_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"battle_creek_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"battle_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"bear_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"bear_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"bear_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"big_chico_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"butte_creek_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"butte_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"calaveras_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"calaveras_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"clear_creek_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"clear_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"cosumnes_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"cosumnes_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"cottonwood_creek_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"cottonwood_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"cow_creek_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"cow_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"deer_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"elder_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"feather_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"feather_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"merced_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"merced_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"mill_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"mokelumne_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"mokelumne_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"paynes_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"san_joaquin_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"san_joaquin_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"stanislaus_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"stanislaus_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"stony_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"thomes_creek_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"tuolumne_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"tuolumne_river_floodplain"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"yuba_river_instream"

#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"yuba_river_floodplain"

# REGIONAL ----
#' @rdname habitat_data
#' @format NULL
#' @usage NULL
"upper_mid_sac_region_instream"

# SACRAMENTO ----
#' Mainstem Sacramento River Habitat Data
#' @description Datasets used to calculate spawning, instream, and floodplain
#' rearing habitat areas for Chinook Salmon and Steelhead
#'
#' @format NULL
#'
#' @usage NULL
#'
#' @section Spawning:
#' Datasets containing the Weighted Usable Area (WUA) in square feet per 1000 feet
#' as a function of flow in cubic feet per second for Fall Run (FR), Late-fall Run (LFR),
#' Winter Run (WR), and Steelhead (ST).
#'
#' \itemize{
#'   \item \code{upper_sac_ACID_boards_in}
#'   \item \code{upper_sac_ACID_boards_out}
#' }
#'
#' Data Format:
#' \itemize{
#'   \item{\strong{flow_cfs} - flow value in cubic feet per second}
#'   \item{\strong{FR_spawn_wua} - Spawning WUA in square feet per 1000 feet (Fall Run)}
#'   \item{\strong{LFR_spawn_wua} - Spawning WUA in square feet per 1000 feet (Late-Fall Run)}
#'   \item{\strong{ST_spawn_wua} - Spawning WUA in square feet per 1000 feet (Steelhead)}
#'   \item{\strong{WR_spawn_wua} - Spawning WUA in square feet per 1000 feet (Winter Run)}
#'   \item{\strong{watershed} - Name of watershed}
#' }
#'
#' @section Instream:
#' Datasets containing the instream rearing habitat area in square meters as a
#' function of flow in cubic feet per second.
#'
#' \itemize{
#'   \item \code{upper_sacramento_river_instream}
#'   \item \code{upper_mid_sacramento_river_instream}
#'   \item \code{lower_mid_sacramento_river_instream}
#'   \item \code{lower_sacramento_river_instream}
#' }
#'
#' Data Format:
#' \itemize{
#'   \item{\strong{flow_cfs} - Flow value in cubic feet per second}
#'   \item{\strong{rearing_sq_meters} - Suitable rearing area in square meteres}
#'   \item{\strong{watershed} - Name of watershed}
#' }
#'
#' @section Floodplain:
#' Datasets containing the floodplain habitat area in square meters as a function
#' of flow in cubic feet per second.
#'
#' \itemize{
#'   \item \code{upper_sacramento_river_floodplain}
#'   \item \code{upper_mid_sacramento_river_floodplain}
#'   \item \code{lower_mid_sacramento_river_floodplain}
#'   \item \code{lower_sacramento_river_floodplain}
#' }
#'
#' Data Format:
#' \itemize{
#'   \item{\strong{flow_cfs} - Flow value in cubic feet per second}
#'   \item{\strong{floodplain_sq_meters} - Suitable rearing area in square meteres}
#'   \item{\strong{watershed} - Name of watershed}
#' }
#'
#' @source
#' \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/sacramento_river.html}{Sacramento River Modeling Details}
#'
#' @name sacramento_habitat
NULL

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"upper_sac_ACID_boards_in"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"upper_sac_ACID_boards_out"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"upper_sacramento_river_instream"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"upper_sacramento_river_floodplain"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"upper_mid_sacramento_river_instream"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"upper_mid_sacramento_river_floodplain"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"lower_mid_sacramento_river_instream"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"lower_mid_sacramento_river_floodplain"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"lower_sacramento_river_instream"

#' @rdname sacramento_habitat
#' @format NULL
#' @usage NULL
"lower_sacramento_river_floodplain"

# BYPASS ----
#' Bypass Flow to Habitat Area Relationships
#'
#' @description A dataset containing the suitable habitat area in square meters as a
#' function of flow in cubic feet per second.
#'
#' @format NULL
#'
#' @section yolo_bypass_habitat:
#' \describe{
#'   \item{flow_cfs}{integer flow value in cubic feet per second}
#'   \item{Yolo Bypass 1}{suitable rearing area in square meters in the Yolo Bypass, Fremont Weir to Sacramento Weir}
#'   \item{Yolo Bypass 2}{suitable rearing area in square meters in the Yolo Bypass below Sacramento Weir}
#' }
#'
#' @section sutter_bypass_habitat:
#' \describe{
#'   \item{flow_cfs}{integer flow value in cubic feet per second}
#'   \item{Sutter Bypass 1}{suitable rearing area in square meters in the Sutter Bypass, to Moulton Weir}
#'   \item{Sutter Bypass 2}{suitable rearing area in square meters in the Sutter Bypass, to Colusa Weir}
#'   \item{Sutter Bypass 3}{suitable rearing area in square meters in the Sutter Bypass, to Tisdale Weir}
#'   \item{Sutter Bypass 4}{suitable rearing area in square meters in the Sutter Bypass below Tisdale Weir}
#' }
#'
#'
#' @examples
#' yolo_bypass_habitat
#'
#' @source \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/bypasses.html}{Sutter and Yolo Bypass Modeling Details}
#' @name bypass_habitat
NULL

#' @rdname bypass_habitat
#' @format NULL
#' @usage NULL
'yolo_bypass_habitat'

#' @rdname bypass_habitat
#' @format NULL
#' @usage NULL
'sutter_bypass_habitat'

# DELTA ----
#' Delta Habitat Area
#'
#' @description A dataset containing the area of highly suitable habitat within the
#' North and South Deltas
#'
#' @format dataframe with 372 rows and 3 variables
#' \describe{
#'   \item{date}{modeled results for 1980-2010}
#'   \item{North Delta}{high quality habitat area in square meters}
#'   \item{South Delta}{high quality habitat area in square meters}
#' }
#'
#'
#' @source
#' \href{http://cvpia-habitat-docs-markdown.s3-website-us-west-2.amazonaws.com/watershed/delta.html}{North and South Delta Modeling Details}
#'
#' \itemize{
#'   \item Modeling Output: Correigh Green \email{correigh.greene@@noaa.gov}
#'   \item Delta Node Selection: Mark Tompkins \email{mtompkins@@flowwest.com}
#'   \item Data Wrangling: Sadie Gill \email{sgill@@flowwest.com}
#' }
#' @keywords internal
'delta_rearing_habitat'

# OTHER-------------
#' Habitat Extent Lengths
#' @description A dataset containing the length of rearing and spawning extent within each CVPIA watershed for different species.
#'
#' @format dataframe with 57 rows and 7 variables
#' \describe{
#' \item{order}{integer value representing watershed order in SIT model}
#' \item{watershed}{name of watershed}
#' \item{lifestage}{habitat type by lifestage, 'spawing' or 'rearing'}
#' \item{miles}{length in miles}
#' \item{feet}{length in feet}
#' \item{source}{Expert who delineated habitat extents}
#' \item{species}{species of habitat extent}
#' }
#'
#' @examples
#' watershed_lengths
#'
#' @details Information comes from expert outreach undertaken in 2017. Updated in 2021 to include late fall run extents from expert outreach.
#'
#' @source \href{https://cvpiahabitat-r-package.s3.us-west-2.amazonaws.com/salmonid_habitat_extents.zip}{Salmonid Extents}
'watershed_lengths'

#' Habitat Modeling Status
#' @description This table summaries modeling availability based on species and
#' lifestage combinations for each watershed
#'
#' @format dataframe with 32 rows and 23 variables
#' \describe{
#' \item{Order}{integer value representing watershed order in SIT model}
#' \item{Watershed}{name of watershed}
#' \item{FR_spawn}{TRUE if spawning habitat modeling exists for Fall Run}
#' \item{FR_fry}{TRUE if fry rearing habitat modeling exists for Fall Run}
#' \item{FR_juv}{TRUE if juvenile rearing habitat modeling exists for Fall Run}
#' \item{FR_floodplain}{TRUE if floodplain rearing modeling exists for Fall Run}
#' \item{SR_spawn}{TRUE if spawning habitat modeling exists for Spring Run}
#' \item{SR_fry}{TRUE if fry rearing habitat modeling exists for Spring Run}
#' \item{SR_juv}{TRUE if juvenile rearing habitat modeling exists for Spring Run}
#' \item{SR_floodplain}{TRUE if floodplain rearing modeling exists for Spring Run}
#' \item{ST_spawn}{TRUE if spawning habitat modeling exists for Steelhead}
#' \item{ST_fry}{TRUE if fry rearing habitat modeling exists for Steelhead}
#' \item{ST_juv}{TRUE if juvenile rearing habitat modeling exists for Steelhead}
#' \item{ST_floodplain}{TRUE if floodplain rearing modeling exists for Steelhead}
#' \item{ST_adult}{TRUE if adult rearing habitat modeling exists for Steelhead}
#' \item{LFR_spawn}{TRUE if spawning habitat modeling exists for Late-fall Run}
#' \item{LFR_fry}{TRUE if fry rearing habitat modeling exists for Late-fall Run}
#' \item{LFR_juv}{TRUE if juvenile rearing habitat modeling exists for Late-fall Run}
#' \item{LFR_floodplain}{TRUE if floodplain rearing modeling exists for Late-fall Run}
#' \item{WR_spawn}{TRUE if spawning habitat modeling exists for Winter Run}
#' \item{WR_fry}{TRUE if fry rearing habitat modeling exists for Winter Run}
#' \item{WR_juv}{TRUE if juvenile rearing habitat modeling exists for Winter Run}
#' \item{WR_floodplain}{TRUE if floodplain rearing modeling exists for Winter Run}
#' }
#'
#' @examples
#' modeling_exist
#'
#' @source Sadie Gill \email{sgill@@flowwest.com}
'modeling_exist'

#' Flow Thresholds for Number of Weeks Inundated
#' @description Estimated relationship between duration of inundation and monthly mean flow
#' @format dataframe with rows and 3 variables
#' \describe{
#' \item{watershed}{name of CVPIA watershed}
#' \item{weeks_inundated}{integer value between 0 and 4 to represent weeks of floodplain inundation}
#' \item{flow_threshold}{flow threshold in cubic feet per second associated with number of weeks inundated}
#' }
#'
#' @examples
#' weeks_inundated
#'
#' @details
#' Visual inspection of measured flow and professional judgement were used to define relationships
#' between number of days inundated and the mean monthly flow.
#'
#'
#' \strong{Relationship Modeled:} \cr
#' \itemize{
#'   \item Sacramento River
#'   \item Big Chico Creek
#'   \item Butte Creek
#'   \item Cottonwood Creek
#'   \item Deer Creek
#'   \item Elder Creek
#'   \item Sutter Bypass
#'   \item Bear River
#'   \item Feather River
#'   \item Yuba River
#'   \item Yolo Bypass
#'   \item American River
#'   \item Calaveras River
#'   \item Merced River
#'   \item Stanislaus River
#'   \item Tuolumne River
#'   \item San Joaquin River
#' }
#'
#'
#' All other watersheds are assumed to have a two week inundation.
#'
#' @source Sadie Gill  \email{sgill@@flowwest.com}
'weeks_inundated'

#' Pool Area
#'
#' @description A dataset containing estimated area of pools within a watershed
#'
#' @format dataframe with 31 rows and 3 variables
#' \describe{
#'   \item{watershed}{CVPIA watershed}
#'   \item{SR_pools_sq_meters}{Spring Run pool area estimate in square meters}
#'   \item{ST_pools_sq_meters}{Steelhead pool area estimate in square meters}
#' }
#'
#' @details
#' Estimates of percent pools were provided by Mark Gard for some watersheds.
#' The mean percent pools, excluding Feather River, is 23.1\%. This value is used for watersheds without an estimate.
#' The total area of pools is then caluculated
#' by multiplying the estimated percent pools by the total channel area. Channel
#' areas were estimated using Google Earth Engine.
#'
#' Watersheds With Estimated Percent Pools:
#' \itemize{
#'   \item Antelope Creek
#'   \item Battle Creek
#'   \item Butte Creek
#'   \item Clear Creek
#'   \item Cottonwood Creek
#'   \item Deer Creek
#'   \item Feather River (*80% percent pools seems high, but was confirmed by Jason Kindopp)
#'   \item Mill Creek
#'   \item Mokelumne River
#'   \item Stanislaus River
#'   \item Tuolumne River
#'   \item Upper Sacramento River
#'   \item Yuba River
#' }
#'
#' \strong{Note:} The area of pools that covers both the Upper Sacramento and
#' Upper-mid Sacramento DSM segments is assigned to the Upper Sacramento River.
#'
#' @source
#' \itemize{
#'   \item Percent Pool Estimates: Mark Gard \email{Mark.Gard@@Wildlife.ca.gov}
#'   \item Channel Area Estimates: Sadie Gill \email{sgill@@flowwest.com}
#'   \item QA/QC: Mark Tompkins \email{mtompkins@@flowwest.com}
#' }
#'
'pools'

# TODO revise now that we have more adult rearing modeling
#' Steelhead Adult Rearing Habitat
#'
#' @description A dataset containing adult rearing habitat quanities in square meters
#'
#'
#' @format array[watersheds, months, years]
#'
#' @details
#' Only 5 of the watersheds have wua relationships:
#' 
#' \strong{Adult Trout}
#' \enumerate{
#'    \item Battle Creek
#'    \item Butte Creek
#' }
#'
#' \strong{Steelhead}
#' \enumerate{
#'    \item Merced River
#'    \item Tuolumne River
#'    \item Yuba River
#' }
#'
#' These watershed's habitat values vary by month.
#'
#' For the other watersheds, we created one habitat value using the calsim mean flows for the years 1980-1999.
#' We used Merced's adult steelhead wua curve and scaled it by the ratio of mean flow December through May
#' between itself and Merced.
#'
#' @source
#' \itemize{
#'   \item Wrangling: Sadie Gill \email{sgill@@flowwest.com}
#'   \item Method: Mark Tompkins \email{mtompkins@@flowwest.com}
#' }
#'
'inchannel_habitat_adult'



# Habitat cached ------------------

#' Spawning Habitat
#' @description The 1979-1999 suitable spawning habitat area in square meters for Steelhead
#' and 1979-2000 suitable spawning habitat area in square meters for Fall Run, Spring Run and Winter Run.
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' \item Fall Run, Late Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 22 years]
#' }
#' @details
#' Values created using the function \code{\link[=set_spawning_habitat]{set_spawning_habitat}}
#'
#' @name spawn
#' @aliases NULL
NULL


#' @rdname spawn
#' @format NULL
"fr_spawn"

#' @rdname spawn
#' @format NULL
"wr_spawn"

#' @rdname spawn
#' @format NULL
"sr_spawn"

#' @rdname spawn
#' @format NULL
"st_spawn"

#' @rdname spawn
#' @format NULL
"lfr_spawn"

#' Juvenile Inchannel Habitat
#' @description The 1980-1999 juvenile suitable inchannel rearing habitat area in square meters for  
#' Steelhead and 1980-2000 juvenile suitable inchannel rearing habitat area in square meters
#' for Fall Run, Spring Run and Winter Run.
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 20 years]
#' \item Fall Run, Late Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' }
#' @details 
#' Values created using the function \code{\link[=set_instream_habitat]{set_instream_habitat}}
#' @name juvenile
#' @aliases NULL
NULL

#' Fry Inchannel Habitat
#' @description The 1980-1999 fry suitable inchannel rearing habitat area in square meters for Steelhead
#' and 1979-2000 suitable inchannel rearing habitat area in square meters for Fall Run, Spring Run and Winter Run.
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 20 years]
#' \item Fall Run, Late Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' }
#' @details 
#' Values created using the function \code{\link[=set_instream_habitat]{set_instream_habitat}}
#' @name fry
#' @aliases NULL
NULL

#' @rdname fry
#' @format NULL
"fr_fry"

#' @rdname fry
#' @format NULL
"wr_fry"

#' @rdname fry
#' @format NULL
"sr_fry"

#' @rdname fry
#' @format NULL
"st_fry"

#' @rdname juvenile
#' @format NULL
"fr_juv"

#' @rdname juvenile
#' @format NULL
"wr_juv"

#' @rdname juvenile
#' @format NULL
"sr_juv"

#' @rdname juvenile
#' @format NULL
"st_juv"

#' @rdname fry
#' @format NULL
"lfr_fry"

#' @rdname juvenile
#' @format NULL
"lfr_juv"


#' Floodplain Habitat
#' @description The 1980-1999 total floodplain rearing habitat area in square meters for 
#' Steelhead and 1980-2000 total floodplain rearing habitat area in square meters for Fall Run, Spring Run and
#' Winter Run
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 20 years]
#' \item Fall Run, Late Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' }
#' @details 
#' Values created using the function \code{\link[=set_floodplain_habitat]{set_floodplain_habitat}}
#' Need to apply a suitability factor, recommend 27\%.
#'  
#' @name floodplain
#' @aliases NULL
NULL

#' @rdname floodplain
#' @format NULL
"fr_fp"

#' @rdname floodplain
#' @format NULL
"wr_fp"

#' @rdname floodplain
#' @format NULL
"sr_fp"

#' @rdname floodplain
#' @format NULL
"st_fp"

#' @rdname floodplain
#' @format NULL
"lfr_fp"


#' Bypass Habitat Area
#' @description The 1980-2000 suitable juvenile rearing habitat area in square meters
#' @format a 2 dimensional array [12 months, 21 years]
#' @details 
#' Calculated using the function \code{\link[=set_bypass_habitat]{set_bypass_habitat}}
#' @section Sutter Bypass:
#' 
#' The area within Sutter Bypass is the summation of the four sections listed below
#' \itemize{
#'   \item sutter1 = to Moulton Weir
#'   \item sutter2 = to Colusa Weir
#'   \item sutter3 = to Tisdale Weir
#'   \item sutter4 = below Tisdale Weir
#' }
#' 
#' @section Yolo Bypass:
#' 
#' The area within Yolo Bypass is the summation of the two sections listed below
#' 
#' \itemize{
#'   \item yolo1 = Fremont Weir to Sacramento Weir
#'   \item yolo2 = below Sacramento Weir
#' }
#' @name bypass
#' @aliases NULL
NULL


#' @rdname bypass
#' @format NULL
"sutter_habitat"

#' @rdname bypass
#' @format NULL
"yolo_habitat"


#' Floodplain Habitat Activation Duration
#' @description The 1980-2000 floodplain rearing habitat event duration in number of weeks
#' @format a 3 dimensional array [31 watersheds, 12 months, 21 years]
#' @details 
#' Values created using the function \code{\link[=get_weeks_flooded]{get_weeks_flooded}}
"weeks_flooded"

#' Delta Habitat Array
#' @description The 1980-2000 delta rearing habitat
#' @format a 3 dimensional array [12 months, 21 years, 2 deltas]
#' @details 
#' North Delta data is stored at [ , , 1] and South Delta data is stored at [ , , 2]
#' Values created using \code{\link[=delta_rearing_habitat]{delta_rearing_habitat}}
"delta_habitat"

# MISCELLANEOUS DATA ----

#' Watershed Routing
#' @description Indicator of whether fish have access to a region 
#' @format 1 dimensional array [31 watersheds]
#' @source 
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
#' @name watershed_routing_indicators
NULL

#' @rdname watershed_routing_indicators
#' @format NULL
"tisdale_bypass_watershed"

#' @rdname watershed_routing_indicators
#' @format NULL
"yolo_bypass_watershed"

#' @rdname watershed_routing_indicators
#' @format NULL
"south_delta_routed_watersheds"

#' Number of Contact Points
#' @description Number of contact points, estimated using PAD
#' Contact points were derived from the Passage Assessment Database \href{https://nrm.dfg.ca.gov/PAD/view/query.aspx}{(PAD)}
#' maintained by California Department of Fish and Wildlife. Each location considered 
#' in the model (e.g., tributary, Sacramento reach, and delta subdivisions) was 
#' assessed for all structures identified in the PAD.
#' @format 1 dimensional array [31 watersheds]
#' @details 
#' Delta data is length two (North, South)
#' Watershed data is length 31 (Mainstems, Bypasses, and Tributaries) 
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
#' @name pad_structures
NULL

#' @rdname pad_structures
#' @format NULL
"contact_points"

#' @rdname pad_structures
#' @format NULL
"delta_contact_points"

#' Probability High Predation in the Deltas
#' @description Expert estimated high predation probabilities
"delta_prop_high_predation"

#' Expert Estimated Probabilities
#' @description Experts estimated probabilities for predation, stranding, and nest scouring
#' @format 1 dimensional array [31 watersheds]
#' @source
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#'
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
#' @name estimated_probs
NULL

#' @rdname estimated_probs
#' @format NULL
"prop_high_predation"

#' @rdname estimated_probs
#' @format NULL
"prob_strand_early"

#' @rdname estimated_probs
#' @format NULL
"prob_strand_late"

#' @rdname estimated_probs
#' @format NULL
"prob_nest_scoured"


#' @title Gravel size to proportion of movement
"gravel_size_to_prop_of_movement"

#' @title Watershed Decay Status
#' @description Watershed to TRUE/FALSE lookup indicating whether habitat decay occurs.
"watershed_decay_status"

#' @title Watershed Spawning Habitat Decays
#' @description The total decay by watershed. This includes watersheds with no decay, these simply
#' have all values equal to 0. For watersheds with decay, there are three different curves,
#' 'min', 'avg', 'max'.
#' @format a list of 31 dataframes, each dataframe containing accumulated decay for the 20 year simulation
#' for each curve level ('min', 'avg', 'max') or 'none' for non-decay watersheds. 
"watershed_spawning_decays"

#' @title Fall-run Spawning Decay Multiplier
#' @description List of multiplier matrices to be used as decay per month and year for Spawning Habitat.
#' @details this decay multiplier only exists for Fall-run, Spring and Winter run models rely on probabilistic spawning habitat decay.
#' @format list of 31, 12x22 matrices. 
"spawning_decay_multiplier"


#' @title Spawning Habitat Average
#' @description habitat average by watershed in acres. This data is used to calcualte decays for all 
#' watersheds.
#' @format List of 3 (fr, sr, wr) lenght 31 (watershed) named vectors 
"spawning_habitat_average"







