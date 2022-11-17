library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(DSMhabitat)

watersheds <- DSMscenario::watershed_labels
watersheds_order <- DSMhabitat::watershed_species_present %>% 
  select(order, watershed = watershed_name)

#' Generate SIT Model Compatible Array
#' @description transforms to array data structure for SIT model input
#' @name create_Sit_array
#' @param input a vector of data, length = 252 for 12 months and 20 years of data
#' @return 3 dimension array [location, month, year]
create_SIT_array <- function(input) {
  
  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)
  
}

# functions --------------------------------------------------------------------
# get flow, added calsim_version to describe which flow data to take 
# calsim_version options are "biop_2008_2009" or "biop_itp_2018_2019" 
get_flow <- function(watershed, calsim_version, years = c(1980, 1999)) {
  
  # get the flow values at the dates
  dplyr::pull(dplyr::filter(dplyr::select(DSMflow::flows_cfs[[calsim_version]], date, watershed),
                            lubridate::year(date) >= years[1],
                            lubridate::year(date) <= years[2]), 2)
}

# get rearing habitat for all watersheds 
get_rear_hab_all <- function(watersheds, species, life_stage, calsim_version, years = 1980:1999) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, calsim_version, range(years))
    habitat <- DSMhabitat::set_instream_habitat(watershed,
                                                species = species,
                                                life_stage = life_stage,
                                                flow = flows)
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
  })
  
  # deal with sacramento special cases
  # lower-mid sac
  low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', calsim_version, range(years))
  low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', calsim_version, range(years))
  
  low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
    DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                     species = species,
                                     life_stage = life_stage,
                                     flow = flow, flow2 = flow2)
  })
  
  low_mid_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Lower-mid Sacramento River',
    hab_sq_m = low_mid_sac_hab)
  
  hab <- bind_rows(most, low_mid_sac) %>%
    spread(watershed, hab_sq_m) %>% 
    bind_cols(tibble(`Sutter Bypass` = rep(0, total_obs),
                     `Yolo Bypass` = rep(0, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  return(hab)
}

# get spawning habitat for all watersheds
get_spawn_hab_all <- function(watersheds, species, calsim_version, years = 1979:2000) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, calsim_version, years=range(years))
    habitat <- DSMhabitat::set_spawning_habitat(watershed,
                                                species = species,
                                                flow = flows)
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
  })
  
  # deal with sacramento special cases
  # upper sac
  up_sac_flows <- get_flow('Upper Sacramento River', calsim_version, years=range(years))
  months <- rep(1:12, length(years))
  up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
    DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                     species = species,
                                     flow = flow, month = month)
  })
  
  up_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Upper Sacramento River',
    hab_sq_m = up_sac_hab)
  
  hab <-   bind_rows(most, up_sac) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(NA, total_obs),
                     `Yolo Bypass` = rep(NA, total_obs),
                     `Upper-mid Sacramento River` = rep(NA, total_obs),
                     `Lower-mid Sacramento River` = rep(NA, total_obs),
                     `Lower Sacramento River` = rep(NA, total_obs),
                     `San Joaquin River` = rep(NA, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  
  return(hab)
}

# gets floodplain habitat for all watersheds 
get_floodplain_hab_all <- function(watersheds, species, calsim_version, years = 1980:1999) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, calsim_version, range(years))
    habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, flows)
    
    modeling_in_suitable_area <- c("Antelope Creek", "Battle Creek", "Bear Creek", 
                                   "Cow Creek", "Mill Creek", "Paynes Creek", 
                                   "Deer Creek",'Upper Sacramento River',
                                   'Upper-mid Sacramento River','Lower Sacramento River')
    
    if (!(watershed %in% modeling_in_suitable_area)) {
      habitat <- DSMhabitat::apply_suitability(habitat)
    }
    
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
  })
  
  # lower-mid sacramento
  low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", calsim_version, range(years))
  low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", calsim_version, range(years))
  low_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower-mid Sacramento River', species,
                                                       low_mid_sac_flows1, flow2 = low_mid_sac_flows2)
  
  low_mid_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Lower-mid Sacramento River',
    hab_sq_m = low_mid_sac_fp)
  
  hab <- bind_rows(most, low_mid_sac) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(NA, total_obs),
                     `Yolo Bypass` = rep(NA, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  
  return(hab)
  
}

# spawning habitat data objects ------------------------------------------------
spawning_watersheds <- DSMhabitat::watershed_species_present %>%
  filter(!(watershed_name %in% c("Upper Sacramento River", "Upper Mid Sac Region")),
         spawn) %>%
  pull(watershed_name)

# fall run spawning habitat --
# fr spawn 2008 2009 
fr_spawn_2008_2009 <- get_spawn_hab_all(spawning_watersheds, 'fr', "biop_2008_2009")
dimnames(fr_spawn_2008_2009) <- list(watersheds, month.abb, 1979:2000)
fr_spawn_2008_2009[which(is.na(fr_spawn_2008_2009))] <- 0

# fr spawn 2018 2019 
fr_spawn_2018_2019 <- get_spawn_hab_all(spawning_watersheds, 'fr', "biop_itp_2018_2019")
dimnames(fr_spawn_2018_2019) <- list(watersheds, month.abb, 1979:2000)
fr_spawn_2018_2019[which(is.na(fr_spawn_2018_2019))] <- 0

# list together both fr spawning versions
fr_spawn <- list(biop_2008_2009 = fr_spawn_2008_2009,
                 biop_itp_2018_2019 = fr_spawn_2018_2019
)
usethis::use_data(fr_spawn, overwrite = TRUE)

# steelhead spawning habitat -- 
# st spawn 2008 2009 
st_spawn_2008_2009 <- get_spawn_hab_all(spawning_watersheds, 'st', "biop_2008_2009")
st_spawn_2008_2009[which(is.na(st_spawn_2008_2009))] <- 0
dimnames(st_spawn_2008_2009) <- list(watersheds, month.abb, 1979:2000)

# st spawn 2018 2019 
st_spawn_2018_2019 <- get_spawn_hab_all(spawning_watersheds, 'st', "biop_itp_2018_2019")
st_spawn_2018_2019[which(is.na(st_spawn_2018_2019))] <- 0
dimnames(st_spawn_2018_2019) <- list(watersheds, month.abb, 1979:2000)

# list together both steelhead spawning versions
st_spawn <- list(biop_2008_2009 = st_spawn_2008_2009,
                 biop_itp_2018_2019 = st_spawn_2018_2019
)

usethis::use_data(st_spawn, overwrite = TRUE)

# spring run spawning habitat -- 
# sr spawn 2008 2009 
sr_spawn_2008_2009 <- get_spawn_hab_all(spawning_watersheds, 'sr', "biop_2008_2009")
sr_spawn_2008_2009[which(is.na(sr_spawn_2008_2009))] <- 0
dimnames(sr_spawn_2008_2009) <- list(watersheds, month.abb, 1979:2000)

# several watershed that do not have spring run populations but SIT wants to enable colonization
sr_spawn_2008_2009["Thomes Creek", , ] <- st_spawn$biop_2008_2009["Thomes Creek", , ] 
sr_spawn_2008_2009["Calaveras River", , ] <- fr_spawn$biop_2008_2009["Calaveras River", , ] 
sr_spawn_2008_2009["Cosumnes River", , ] <- fr_spawn$biop_2008_2009["Cosumnes River", , ] 
sr_spawn_2008_2009["Merced River", , ] <- fr_spawn$biop_2008_2009["Merced River", , ] 

# sr spawn 2018 2019 
sr_spawn_2018_2019 <- get_spawn_hab_all(spawning_watersheds, 'sr', "biop_itp_2018_2019")
sr_spawn_2018_2019[which(is.na(sr_spawn_2018_2019))] <- 0
dimnames(sr_spawn_2018_2019) <- list(watersheds, month.abb, 1979:2000)

# several watershed that do not have spring run populations but SIT wants to enable colonization
sr_spawn_2018_2019["Thomes Creek", , ] <- st_spawn$biop_itp_2018_2019["Thomes Creek", , ] 
sr_spawn_2018_2019["Calaveras River", , ] <- fr_spawn$biop_itp_2018_2019["Calaveras River", , ] 
sr_spawn_2018_2019["Cosumnes River", , ] <- fr_spawn$biop_itp_2018_2019["Cosumnes River", , ] 
sr_spawn_2018_2019["Merced River", , ] <- fr_spawn$biop_itp_2018_2019["Merced River", , ] 

# Combine together 
sr_spawn <- list(biop_2008_2009 = sr_spawn_2008_2009,
                 biop_itp_2018_2019 = sr_spawn_2018_2019
)

usethis::use_data(sr_spawn, overwrite = TRUE)

# Winter Run spawning habitat -- 
# only in Sacramento and battle creek
# spawn just in Upper Sac
generate_wr_spawn <- function(calsim_version) {
  wr_spawn <- array(0, dim = c(31, 12, 22), dimnames = list(watersheds, month.abb, 1979:2000)) 
  up_sac_flows <- get_flow('Upper Sacramento River', 
                           calsim_version, 
                           years = c(1979, 2000))
  months <- rep(1:12, 22)
  up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
    DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                     species = 'wr',
                                     flow = flow, month = month)
  })
  
  battle_flows <- get_flow('Battle Creek', calsim_version, years = c(1979, 2000))
  battle_hab <- map_dbl(battle_flows, function(flow) {
    DSMhabitat::set_spawning_habitat('Battle Creek',
                                     species = 'wr',
                                     flow = flow)
  })
  
  wr_spawn["Upper Sacramento River",,] <- up_sac_hab
  wr_spawn["Battle Creek",,] <- battle_hab
  return(wr_spawn)
}

wr_spawn_2008_2009 <- generate_wr_spawn("biop_2008_2009")
wr_spawn_2018_2019 <- generate_wr_spawn("biop_itp_2018_2019")

# combine together
wr_spawn <- list(biop_2008_2009 = wr_spawn_2008_2009,
                 biop_itp_2018_2019 = wr_spawn_2018_2019
)


usethis::use_data(wr_spawn, overwrite = TRUE)


# Late Fall Run 
# only in sacramento clear creek and battle creek
# spawn just in Upper Sac
generate_lfr_spawn <- function(calsim_version) {
  lfr_spawn <- array(0, dim = c(31, 12, 22), 
                               dimnames = list(watersheds, month.abb, 1979:2000))  
  up_sac_flows <- get_flow('Upper Sacramento River', 
                           calsim_version,
                           years = c(1979, 2000))
  months <- rep(1:12, 22)
  up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
    DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                     species = 'lfr',
                                     flow = flow, month = month)
  })
  
  battle_flows <- get_flow('Battle Creek', 
                           calsim_version, 
                           years = c(1979, 2000))
  battle_hab <- map_dbl(battle_flows, function(flow) {
    DSMhabitat::set_spawning_habitat('Battle Creek',
                                     species = 'lfr',
                                     flow = flow)
  })
  
  clear_flows <- get_flow('Clear Creek',
                          calsim_version, 
                          years = c(1979, 2000))
  clear_hab <- map_dbl(clear_flows, function(flow) {
    DSMhabitat::set_spawning_habitat('Clear Creek',
                                     species = 'lfr',
                                     flow = flow)
  })
  
  lfr_spawn["Upper Sacramento River", , ] <- up_sac_hab
  lfr_spawn["Battle Creek", , ] <- battle_hab
  lfr_spawn["Clear Creek", , ] <- clear_hab
  return(lfr_spawn)
}

# apply function to both calsim runs 
lfr_spawn_2008_2009 <- generate_lfr_spawn("biop_2008_2009")
lfr_spawn_2018_2019 <- generate_lfr_spawn("biop_itp_2018_2019")

# combine together
lfr_spawn <- list(biop_2008_2009 = lfr_spawn_2008_2009,
                  biop_itp_2018_2019 = lfr_spawn_2018_2019
)


usethis::use_data(lfr_spawn, overwrite = TRUE)

# rearing habitat objects ------------------------------------------------------
# subset reraing watersheds in order 
watersheds_in_order <- DSMhabitat::watershed_species_present %>%
  filter(!(watershed_name  %in% c('Sutter Bypass',
                             'Lower-mid Sacramento River', 'Yolo Bypass', 
                             'Upper Mid Sac Region'))) %>%
  pull(watershed_name)

# fry habitat ------------------------------------------------------------------
# fall run fry rearing habitat -- 
# fr fry 2008 2009 
fr_fry_2008_2009 <- get_rear_hab_all(watersheds_in_order, 'fr', 'fry', "biop_2008_2009", 1980:2000)
dimnames(fr_fry_2008_2009) <- list(watersheds, month.abb, 1980:2000)
fr_fry_2008_2009[which(is.na(fr_fry_2008_2009))] <- 0

# fr fry 2008 2009 
fr_fry_2018_2019 <- get_rear_hab_all(watersheds_in_order, 'fr', 'fry', "biop_itp_2018_2019", 1980:2000)
dimnames(fr_fry_2018_2019) <- list(watersheds, month.abb, 1980:2000)
fr_fry_2018_2019[which(is.na(fr_fry_2018_2019))] <- 0

# combine together
fr_fry <- list(biop_2008_2009 = fr_fry_2008_2009,
               biop_itp_2018_2019 = fr_fry_2018_2019
)


usethis::use_data(fr_fry, overwrite = TRUE)

# steelhead fry rearing habitat -- 
# st fry 2008 2009 
st_fry_2008_2009 <- get_rear_hab_all(watersheds_in_order, 'st', 'fry', "biop_2008_2009", 1980:2000)
dimnames(st_fry_2008_2009) <- list(watersheds, month.abb, 1980:2000)
st_fry_2008_2009[which(is.na(st_fry_2008_2009))] <- fr_fry_2008_2009[which(is.na(st_fry_2008_2009))]

# st fry 2018 2019 
st_fry_2018_2019 <- get_rear_hab_all(watersheds_in_order, 'st', 'fry', "biop_itp_2018_2019", 1980:2000)
dimnames(st_fry_2018_2019) <- list(watersheds, month.abb, 1980:2000)
st_fry_2018_2019[which(is.na(st_fry_2018_2019))] <- fr_fry_2018_2019[which(is.na(st_fry_2018_2019))]

# combine together
st_fry <- list(biop_2008_2009 = st_fry_2008_2009,
               biop_itp_2018_2019 = st_fry_2018_2019
)

usethis::use_data(st_fry, overwrite = TRUE)

# spring run fry rearing habitat -- 
# sr fry 2008 2009 
sr_fry_2008_2009 <- get_rear_hab_all(watersheds_in_order, 'sr', 'fry', "biop_2008_2009", years = 1980:2000)
dimnames(sr_fry_2008_2009) <- list(watersheds, month.abb, 1980:2000)
sr_fry_2008_2009[which(is.na(sr_fry_2008_2009))] <- fr_fry_2008_2009[which(is.na(sr_fry_2008_2009))]

# sr fry 2008 2009 
sr_fry_2018_2019 <- get_rear_hab_all(watersheds_in_order, 'sr', 'fry', "biop_itp_2018_2019", years = 1980:2000)
dimnames(sr_fry_2018_2019) <- list(watersheds, month.abb, 1980:2000)
sr_fry_2018_2019[which(is.na(sr_fry_2018_2019))] <- fr_fry_2018_2019[which(is.na(sr_fry_2018_2019))]

# combine together
sr_fry <- list(biop_2008_2009 = sr_fry_2008_2009,
               biop_itp_2018_2019 = sr_fry_2018_2019
)

usethis::use_data(sr_fry, overwrite = TRUE)

# winter run rearing habitat -- 
generate_wr_fry_or_juv <- function(calsim_version, lifestage = c("fry", "juv")) {
  # set default values to allow for straying 
  if (lifestage == "fry") {
    wr_hab <- fr_fry[[calsim_version]]
  } 
  if (lifestage == "juv") {
    wr_hab <- fr_juv[[calsim_version]]
  }
  wr_hab["Upper Sacramento River", , ] <- DSMhabitat::set_instream_habitat('Upper Sacramento River',
                                                    species = 'wr',
                                                    life_stage = lifestage,
                                                    flow = get_flow('Upper Sacramento River',
                                                                    calsim_version, 
                                                                    years = c(1980, 2000)))
  wr_hab['Upper-mid Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper-mid Sacramento River',
                                                     species = 'wr',
                                                     life_stage = lifestage,
                                                     flow = get_flow('Upper-mid Sacramento River',
                                                                     calsim_version, 
                                                                     years = c(1980, 2000)))
  # deal with sacramento special cases
  # lower-mid sac
  low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', calsim_version, years = c(1980, 2000))
  low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', calsim_version, years = c(1980, 2000))
  
  low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
    DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                     species = 'wr',
                                     life_stage = 'fry',
                                     flow = flow, flow2 = flow2)
  })
  
  
  wr_hab['Lower-mid Sacramento River', , ] <- low_mid_sac_hab
  
  wr_hab['Lower Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Lower Sacramento River',
                                                     species = 'wr',
                                                     life_stage = lifestage,
                                                     flow = get_flow('Lower Sacramento River',
                                                                     calsim_version, 
                                                                     years = c(1980, 2000)))
  
  
  wr_hab['Battle Creek', , ] <- DSMhabitat::set_instream_habitat('Battle Creek',
                                                    species = 'wr',
                                                    life_stage = lifestage,
                                                    flow = get_flow('Battle Creek',
                                                                    calsim_version, 
                                                                    years = c(1980, 2000)))
  
  
  wr_hab[which(is.na(wr_hab))] <- 0
  
  return(wr_hab)
}
# call on function for both 2009 calsim and 2019 calsim 
wr_fry_2008_2009 <- generate_wr_fry_or_juv(calsim_version = "biop_2008_2009", 
                                           lifestage = "fry")
wr_fry_2018_2019 <- generate_wr_fry_or_juv(calsim_version = "biop_itp_2018_2019", 
                                           lifestage = "fry")

# combine together
wr_fry <- list(biop_2008_2009 = wr_fry_2008_2009,
               biop_itp_2018_2019 = wr_fry_2018_2019
)

usethis::use_data(wr_fry, overwrite = TRUE)

# Late fall run fry rearing habitat -- 
generate_lfr_juv_or_fry <- function(calsim_version, lifestage = c("juv", "fry")) {
  # set default values to allow for straying 
  if (lifestage == "fry") {
    lfr_hab <- fr_fry[[calsim_version]]
  } 
  if (lifestage == "juv") {
    lfr_hab <- fr_juv[[calsim_version]]
  }
  
  lfr_hab['Upper Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper Sacramento River',
                                                    species = 'lfr',
                                                    life_stage = lifestage,
                                                    flow = get_flow('Upper Sacramento River',
                                                                    calsim_version, 
                                                                    years = c(1980, 2000)))
  lfr_hab['Upper-mid Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper-mid Sacramento River',
                                                     species = 'lfr',
                                                     life_stage = lifestage,
                                                     flow = get_flow('Upper-mid Sacramento River',
                                                                     calsim_version, 
                                                                     years = c(1980, 2000)))
  # deal with sacramento special cases
  # lower-mid sac
  low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', calsim_version, years = c(1980, 2000))
  low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', calsim_version, years = c(1980, 2000))
  
  low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
    DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                     species = 'lfr',
                                     life_stage = lifestage,
                                     flow = flow, flow2 = flow2)
  })
  lfr_hab['Lower-mid Sacramento River', , ] <- low_mid_sac_hab
  
  lfr_hab['Lower Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Lower Sacramento River',
                                                     species = 'lfr',
                                                     life_stage = lifestage,
                                                     flow = get_flow('Lower Sacramento River',
                                                                     calsim_version, 
                                                                     years = c(1980, 2000)))
  
  lfr_hab['Battle Creek', , ] <- DSMhabitat::set_instream_habitat('Battle Creek',
                                                    species = 'lfr',
                                                    life_stage = lifestage,
                                                    flow = get_flow('Battle Creek',
                                                                    calsim_version, 
                                                                    years = c(1980, 2000)))
  lfr_hab['Clear Creek', , ] <- DSMhabitat::set_instream_habitat('Clear Creek',
                                                     species = 'lfr',
                                                     life_stage = lifestage,
                                                     flow = get_flow('Clear Creek',
                                                                     calsim_version, 
                                                                     years = c(1980, 2000)))
  lfr_hab[which(is.na(lfr_hab))] <- 0
  
  dimnames(lfr_hab) <- list(watersheds, month.abb, 1980:2000)
  return(lfr_hab)
}

# call on function for both calsim versions 
lfr_fry_2008_2009 <- generate_lfr_juv_or_fry(calsim_version = "biop_2008_2009", 
                                             lifestage = "fry")
lfr_fry_2018_2019 <- generate_lfr_juv_or_fry(calsim_version = "biop_itp_2018_2019", 
                                             lifestage = "fry")

# combine together
lfr_fry <- list(biop_2008_2009 = lfr_fry_2008_2009,
                biop_itp_2018_2019 = lfr_fry_2018_2019
)

usethis::use_data(lfr_fry, overwrite = TRUE)

# juvenile----------------------------------------------------------------------

# fall run juvenile rearing habitat -- 
# fr juv 2008 2009 
fr_juv_2008_2009 <- get_rear_hab_all(watersheds_in_order, 'fr', 'juv', 'biop_2008_2009', 1980:2000)
dimnames(fr_juv_2008_2009) <- list(watersheds, month.abb, 1980:2000)
fr_juv_2008_2009[which(is.na(fr_juv_2008_2009))] <- 0

# fr juv 2018 2019 
fr_juv_2018_2019 <- get_rear_hab_all(watersheds_in_order, 'fr', 'juv', 'biop_itp_2018_2019', 1980:2000)
dimnames(fr_juv_2018_2019) <- list(watersheds, month.abb, 1980:2000)
fr_juv_2018_2019[which(is.na(fr_juv_2018_2019))] <- 0

# combine together
fr_juv <- list(biop_2008_2009 = fr_juv_2008_2009,
               biop_itp_2018_2019 = fr_juv_2018_2019
)


usethis::use_data(fr_juv, overwrite = TRUE)

# steelhead juvenile reraing habitat -- 
# st juv 2008 2009 
st_juv_2008_2009 <- get_rear_hab_all(watersheds_in_order, 'st', 'juv', 'biop_2008_2009', 1980:2000)
dimnames(st_juv_2008_2009) <- list(watersheds, month.abb, 1980:2000)
st_juv_2008_2009[which(is.na(st_juv_2008_2009))] <- fr_juv_2008_2009[which(is.na(st_juv_2008_2009))]

# st juv 2018 2019 
st_juv_2018_2019 <- get_rear_hab_all(watersheds_in_order, 'st', 'juv', 'biop_itp_2018_2019', 1980:2000)
dimnames(st_juv_2018_2019) <- list(watersheds, month.abb, 1980:2000)
st_juv_2018_2019[which(is.na(st_juv_2018_2019))] <- fr_juv_2018_2019[which(is.na(st_juv_2018_2019))]

# combine together
st_juv <- list(biop_2008_2009 = st_juv_2008_2009,
               biop_itp_2018_2019 = st_juv_2018_2019
)

usethis::use_data(st_juv, overwrite = TRUE)

# spring run juvenile rearing habitat -- 
# sr juv 2008 2009 
sr_juv_2008_2009 <- get_rear_hab_all(watersheds_in_order, 'sr', 'juv', 'biop_2008_2009', years = 1980:2000)
dimnames(sr_juv_2008_2009) <- list(watersheds, month.abb, 1980:2000)
sr_juv_2008_2009[which(is.na(sr_juv_2008_2009))] <- fr_juv_2008_2009[which(is.na(sr_juv_2008_2009))]

# sr juv 2008 2009 
sr_juv_2018_2019 <- get_rear_hab_all(watersheds_in_order, 'sr', 'juv', 'biop_itp_2018_2019', years = 1980:2000)
dimnames(sr_juv_2018_2019) <- list(watersheds, month.abb, 1980:2000)
sr_juv_2018_2019[which(is.na(sr_juv_2018_2019))] <- fr_juv_2018_2019[which(is.na(sr_juv_2018_2019))]

# combine together
sr_juv <- list(biop_2008_2009 = sr_juv_2008_2009,
               biop_itp_2018_2019 = sr_juv_2018_2019
)

usethis::use_data(sr_juv, overwrite = TRUE)

# winter run juvenile rearing habitat -- 
# use function defined above in fry section 
wr_juv_2008_2009 <- generate_wr_fry_or_juv(calsim_version = "biop_2008_2009", 
                                           lifestage = "juv")
wr_juv_2018_2019 <- generate_wr_fry_or_juv(calsim_version = "biop_itp_2018_2019", 
                                           lifestage = "juv")

# combine together
wr_juv <- list(biop_2008_2009 = wr_juv_2008_2009,
               biop_itp_2018_2019 = wr_juv_2018_2019
)


usethis::use_data(wr_juv, overwrite = TRUE)

# Late fall Run juvenile habitat -- 
# use function defined in fry section above 
lfr_juv_2008_2009 <- generate_lfr_juv_or_fry(calsim_version = "biop_2008_2009", 
                                             lifestage = "juv")
lfr_juv_2018_2019 <- generate_lfr_juv_or_fry(calsim_version = "biop_itp_2018_2019", 
                                             lifestage = "juv")

# combine together
lfr_juv <- list(biop_2008_2009 = lfr_juv_2008_2009,
               biop_itp_2018_2019 = lfr_juv_2018_2019
)

usethis::use_data(lfr_juv, overwrite = TRUE)

# floodplain--------------------------------------------------------------------
# create floodplain watershed list 
watersheds_fp <- DSMhabitat::watershed_species_present %>%
  filter(!(watershed_name  %in% c('Sutter Bypass','Yolo Bypass',
                             'Lower-mid Sacramento River', 
                             'Upper Mid Sac Region'))) %>%
  pull(watershed_name)

# fall run floodplain habitat -- 
# TODO fix this warning!
# fr floodplain 2008 2009 
fr_fp_2008_2009 <- get_floodplain_hab_all(watersheds_fp, 'fr', 'biop_2008_2009', 1980:2000)
dimnames(fr_fp_2008_2009) <- list(watersheds, month.abb, 1980:2000)
fr_fp_2008_2009[which(is.na(fr_fp_2008_2009))] <- 0

# fr floodplain 2018 2019 
fr_fp_2018_2019 <- get_floodplain_hab_all(watersheds_fp, 'fr', 'biop_itp_2018_2019', 1980:2000)
dimnames(fr_fp_2018_2019) <- list(watersheds, month.abb, 1980:2000)
fr_fp_2018_2019[which(is.na(fr_fp_2018_2019))] <- 0


fr_fp <- list(biop_2008_2009 = fr_fp_2008_2009,
              biop_itp_2018_2019 = fr_fp_2018_2019
)

usethis::use_data(fr_fp, overwrite = TRUE)

# steelhead floodplain habitat -- 
# st fp 2008 2009 
st_fp_2008_2009 <- get_floodplain_hab_all(watersheds_fp, 'st', 'biop_2008_2009', 1980:2000)
dimnames(st_fp_2008_2009) <- list(watersheds, month.abb, 1980:2000)
st_fp_2008_2009[which(is.na(st_fp_2008_2009))] <- fr_fp_2008_2009[which(is.na(st_fp_2008_2009))]

# st fp 2018 2019
st_fp_2018_2019 <- get_floodplain_hab_all(watersheds_fp, 'st', 'biop_itp_2018_2019', 1980:2000)
dimnames(st_fp_2018_2019) <- list(watersheds, month.abb, 1980:2000)
st_fp_2018_2019[which(is.na(st_fp_2018_2019))] <- fr_fp_2018_2019[which(is.na(st_fp_2018_2019))]

st_fp <- list(biop_2008_2009 = st_fp_2008_2009,
              biop_itp_2018_2019 = st_fp_2018_2019
)

usethis::use_data(st_fp, overwrite = TRUE)

# spring run floodplain habitat -- 
# sr floodplain 2008 2009 
sr_fp_2008_2009 <- get_floodplain_hab_all(watersheds_fp, 'sr', 'biop_2008_2009', years = 1980:2000)
dimnames(sr_fp_2008_2009) <- list(watersheds, month.abb, 1980:2000)
sr_fp_2008_2009[which(is.na(sr_fp_2008_2009))] <- fr_fp_2008_2009[which(is.na(sr_fp_2008_2009))]

# sr floodplain 2018 2019
sr_fp_2018_2019 <- get_floodplain_hab_all(watersheds_fp, 'sr', 'biop_itp_2018_2019', years = 1980:2000)
dimnames(sr_fp_2018_2019) <- list(watersheds, month.abb, 1980:2000)
sr_fp_2018_2019[which(is.na(sr_fp_2018_2019))] <- fr_fp_2018_2019[which(is.na(sr_fp_2018_2019))]

sr_fp <- list(biop_2008_2009 = sr_fp_2008_2009,
              biop_itp_2018_2019 = sr_fp_2018_2019
)

usethis::use_data(sr_fp, overwrite = TRUE)

# winter run floodplain habitat -- 
generate_wr_floodplain <- function(calsim_version) {
  wr_fp <- fr_fp[[calsim_version]] # Set default values to fall run to allow for straying
  wr_fp['Upper Sacramento River', , ] <- DSMhabitat::set_floodplain_habitat('Upper Sacramento River', 'wr',
                                                   get_flow('Upper Sacramento River',
                                                            calsim_version, 
                                                            years = c(1980, 2000)))
  wr_fp['Upper-mid Sacramento River', , ] <- DSMhabitat::set_floodplain_habitat('Upper-mid Sacramento River', 'wr',
                                                    get_flow('Upper-mid Sacramento River',
                                                             calsim_version, 
                                                             years = c(1980, 2000)))
  wr_fp['Lower Sacramento River', , ] <- DSMhabitat::set_floodplain_habitat('Lower Sacramento River', 'wr',
                                                    get_flow('Lower Sacramento River',
                                                             calsim_version, 
                                                             years = c(1980, 2000)))
  
  # lower-mid sacramento
  low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", calsim_version, years = c(1980, 2000))
  low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", calsim_version, years = c(1980, 2000))
  low_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower-mid Sacramento River', 'wr',
                                                       low_mid_sac_flows1, flow2 = low_mid_sac_flows2)
  
  wr_fp['Lower-mid Sacramento River',,] <- low_mid_sac_fp
  dimnames(wr_fp) <- list(watersheds, month.abb, 1980:2000)
  return(wr_fp)
}

# call on function for both calsim runs  
wr_fp_2008_2009 <- generate_wr_floodplain("biop_2008_2009")
wr_fp_2018_2019 <- generate_wr_floodplain("biop_itp_2018_2019")

# combine 
wr_fp <- list(biop_2008_2009 = wr_fp_2008_2009,
              biop_itp_2018_2019 = wr_fp_2018_2019
)

usethis::use_data(wr_fp, overwrite = TRUE)

# Late fall run floodplain habitat -- 
generate_lfr_floodplain <- function(calsim_version) {
  lfr_fp <- fr_fp[[calsim_version]] # Set default values to fall run to allow for straying
  lfr_fp['Upper Sacramento River',,] <- DSMhabitat::set_floodplain_habitat('Upper Sacramento River', 'lfr',
                                                   get_flow('Upper Sacramento River',
                                                            calsim_version, 
                                                            years = c(1980, 2000)))
  lfr_fp['Upper-mid Sacramento River',,] <- DSMhabitat::set_floodplain_habitat('Upper-mid Sacramento River', 'lfr',
                                                    get_flow('Upper-mid Sacramento River',
                                                             calsim_version, 
                                                             years = c(1980, 2000)))
  lfr_fp['Lower Sacramento River',,] <- DSMhabitat::set_floodplain_habitat('Lower Sacramento River', 'lfr',
                                                    get_flow('Lower Sacramento River',
                                                             calsim_version, 
                                                             years = c(1980, 2000)))
  
  # lower-mid sacramento
  low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", calsim_version, years = c(1980, 2000))
  low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", calsim_version, years = c(1980, 2000))
  low_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower-mid Sacramento River', 'lfr',
                                                       low_mid_sac_flows1, flow2 = low_mid_sac_flows2)
  
  lfr_fp['Lower-mid Sacramento River',,] <- low_mid_sac_fp
  dimnames(lfr_fp) <- list(watersheds, month.abb, 1980:2000)
  return(lfr_fp)
}

# call on function for both calsim versions 
lfr_fp_2008_2009 <- generate_lfr_floodplain("biop_2008_2009")
lfr_fp_2018_2019 <- generate_lfr_floodplain("biop_itp_2018_2019")

# combine 
lfr_fp <- list(biop_2008_2009 = lfr_fp_2008_2009,
              biop_itp_2018_2019 = lfr_fp_2018_2019
)

usethis::use_data(lfr_fp, overwrite = TRUE)

# bypass in stream -------------------------------------------------------------
# sutter bypass habitat 
generate_sutter_habitat <- function(calsim_version) {
  bpf <- DSMflow::bypass_flows[[calsim_version]] %>%
    filter(between(year(date), 1980, 2000))
  
  sutter_habitat <- bind_cols(
    'date' = pull(bpf, date),
    square_meters = DSMhabitat::set_bypass_habitat(bypass = 'sutter1', flow = pull(bpf, sutter1)) + 
      DSMhabitat::set_bypass_habitat(bypass = 'sutter2', flow = pull(bpf, sutter2)) + 
      DSMhabitat::set_bypass_habitat(bypass = 'sutter3', flow = pull(bpf, sutter3)) +
      DSMhabitat::set_bypass_habitat(bypass = 'sutter4', flow = pull(bpf, sutter4))
  ) %>% 
    mutate(year = year(date), 
           month = month(date)) %>%
    select(-date) %>% 
    spread(year, square_meters) %>% 
    select(-month) %>% 
    as.matrix()
  
  rownames(sutter_habitat) <- month.abb
  return(sutter_habitat) 
}

# call on function for both calsim versions 
sutter_habitat_2008_2009 <- generate_sutter_habitat("biop_2008_2009")
sutter_habitat_2018_2019 <- generate_sutter_habitat("biop_itp_2018_2019")

# combine 
sutter_habitat <- list(biop_2008_2009 = sutter_habitat_2008_2009,
                       biop_itp_2018_2019 = sutter_habitat_2018_2019
)

usethis::use_data(sutter_habitat, overwrite = TRUE)

# yolo bypass habitat -- 
generate_yolo_habitat <- function(calsim_version) {
  bpf <- DSMflow::bypass_flows[[calsim_version]] %>%
    filter(between(year(date), 1980, 2000))
  
  yolo_habitat <- bind_cols(
    'date' = pull(bpf, date),
    square_meters = DSMhabitat::set_bypass_habitat(bypass = 'yolo1', flow = pull(bpf, yolo1)) + 
      DSMhabitat::set_bypass_habitat(bypass = 'yolo2', flow = pull(bpf, yolo2)),
  ) %>%
    mutate(year = year(date), 
           month = month(date)) %>%
    select(-date) %>% 
    spread(year, square_meters) %>% 
    select(-month) %>% 
    as.matrix()
  
  rownames(yolo_habitat) <- month.abb
  return(yolo_habitat)
}

# call on function for both calsim versions 
yolo_habitat_2008_2009 <- generate_yolo_habitat("biop_2008_2009")
yolo_habitat_2018_2019 <- generate_yolo_habitat("biop_itp_2018_2019")

# combine 
yolo_habitat <- list(biop_2008_2009 = yolo_habitat_2008_2009,
                       biop_itp_2018_2019 = yolo_habitat_2018_2019
)

usethis::use_data(yolo_habitat, overwrite = TRUE)


# weeks flooded ----------------------------------------------------------------
generate_weeks_flooded <- function(calsim_version) {
  weeks_flooded <- array(0, dim = c(31, 12, 21))
  
  for (i in 1:31) {
    if (i %in% c(17, 21, 22)) next
    flow <- get_flow(watersheds[i], calsim_version, years = c(1980, 2000))
    flooded_weeks <- map_dbl(flow, ~get_weeks_flooded(watersheds[i], .))
    weeks_flooded[i,,] <- matrix(flooded_weeks, ncol = 12)
  }
  
  flooded <- DSMhabitat::fr_fp > 0
  weeks_flooded <- pmax(flooded*2, weeks_flooded)
  
  not_flooded <- DSMhabitat::fr_fp == 0
  weeks_flooded[not_flooded] <- 0
  dimnames(weeks_flooded) <- list(watersheds, month.abb, 1980:2000)
  return(weeks_flooded)
}

# call on function for both calsim versions 
weeks_flooded_2008_2009 <- generate_weeks_flooded("biop_2008_2009")
weeks_flooded_2018_2019 <- generate_weeks_flooded("biop_itp_2018_2019")

# combine 
weeks_flooded <- list(biop_2008_2009 = weeks_flooded_2008_2009,
                      biop_itp_2018_2019 = weeks_flooded_2018_2019
)


usethis::use_data(weeks_flooded, overwrite = TRUE)

# delta habitat ----------------------------------------------------------------
# TODO confirm not changed with flows so only one version - inputs currently WR lcm 

delta_habitat <- array(0, dim = c(12, 21, 2))
delta_rearing_habitat_filtered <- delta_rearing_habitat %>% 
  filter(year(date) < 2001)
delta_habitat[ , , 1] <- matrix(delta_rearing_habitat_filtered$`North Delta`, ncol = 21)
delta_habitat[ , , 2] <- matrix(delta_rearing_habitat_filtered$`South Delta`, ncol = 21)
dimnames(delta_habitat) <- list(month.abb, 1980:2000, c("North Delta", "South Delta"))
usethis::use_data(delta_habitat, overwrite = TRUE)

tisdale_bypass_watershed <- c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                              1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
names(tisdale_bypass_watershed) <- watersheds
usethis::use_data(tisdale_bypass_watershed, overwrite = TRUE)

yolo_bypass_watershed <- c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                              1L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
names(yolo_bypass_watershed) <- watersheds
usethis::use_data(yolo_bypass_watershed, overwrite = TRUE)

south_delta_routed_watersheds <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L)
names(south_delta_routed_watersheds) <- watersheds
usethis::use_data(south_delta_routed_watersheds, overwrite = TRUE)

prop_high_predation <- c(0.3, 0.17, 0.1, 0.17, 0.07, 0.07, 1, 0.27, 0.17, 0.11, 0.27, 
                                   0.11, 0.17, 0, 0.27, 0.3, 0, 0.67, 0.72, 0.17, 1, 0, 0.2, 1, 
                                   0.5, 0.33, 0.33, 0.33, 0.83, 0.34, 0.83)
names(prop_high_predation) <- watersheds
usethis::use_data(prop_high_predation, overwrite = TRUE)

contact_points <- c(33L, 14L, 18L, 5L, 12L, 81L, 10L, 5L, 50L, 12L, 5L, 5L, 2L, 
                    23L, 7L, 121L, 0L, 16L, 123L, 71L, 352L, 0L, 27L, 101L, 179L, 
                    39L, 170L, 197L, 109L, 126L, 220L)
names(contact_points) <- watersheds
usethis::use_data(contact_points, overwrite = TRUE)

prob_strand_early <- c(0.32, 0.15, 0.09, 0.15, 0.19, 0.09, 0.1, 0.15, 0.23, 0.07, 
                       0.19, 0.17, 0.15, 0.08, 0.15, 0.32, 0, 0.16, 0.31, 0.12, 0, 0, 
                       0.02, 0, 0.05, 0.13, 0.02, 0.16, 0.04, 0.32, 0.5)
names(prob_strand_early) <- watersheds
usethis::use_data(prob_strand_early, overwrite = TRUE)

prob_strand_late <- c(0.36, 0.03, 0.01, 0.03, 0.01, 0.01, 0.03, 0.03, 0.05, 0.01, 
                      0.04, 0.27, 0.08, 0.01, 0.02, 0.36, 0, 0.15, 0.2, 0.37, 0.22, 
                      0, 0.01, 0.22, 0.01, 0.01, 0.3, 0.01, 0.5, 0.04, 0.03)
names(prob_strand_late) <- watersheds
usethis::use_data(prob_strand_late, overwrite = TRUE)

prob_nest_scoured <- c(0.18, 0.04, 0.04, 0.04, 0.03, 0.03, 0.05, 0.01, 0.04, 0.05, 
                       0, 0.03, 0.04, 0.05, 0.03, 0.18, 0, 0.04, 0.2, 0.02, 0.01, 0, 
                       0.01, 0.01, 0.05, 0.02, 0.04, 0.04, 0.04, 0.05, 0.01)
names(prob_nest_scoured) <- watersheds
usethis::use_data(prob_nest_scoured, overwrite = TRUE)

delta_contact_points <- c("North Delta" = 718, "South Delta" = 1437)
usethis::use_data(delta_contact_points, overwrite = TRUE)

delta_prop_high_predation <- c("North Delta" = 1, "South Delta" = 1)
usethis::use_data(delta_prop_high_predation, overwrite = TRUE)

