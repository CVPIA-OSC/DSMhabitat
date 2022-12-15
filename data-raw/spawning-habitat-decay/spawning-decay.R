library(tidyverse)
library(gt)
library(gridExtra)
library(lubridate)

theme_set(theme_bw())
gravel_size_scaledown <- read_csv("data-raw/spawning-habitat-decay/data/sediment-prop-move.csv")

head(gravel_size_scaledown)

gravel_size_to_prop_of_movement <- gravel_size_scaledown |> 
  mutate(
    flow_cfs = flow_m3s * 35.315,
    flow_cfday = flow_cfs * 86400) |> 
  group_by(flow_cfs) |> 
  summarise(
    min_fraction = min(fraction), 
    avg_fraction = mean(fraction),
    max_fraction = max(fraction)
  ) 

usethis::use_data(gravel_size_to_prop_of_movement, overwrite = TRUE)

head(gravel_size_to_prop_of_movement)

files_to_read <- list.files("data-raw/spawning-habitat-decay/data/SedimentRatingCurves/",
                            pattern = ".txt",
                            full.names = TRUE)


rating_curves_by_rm <- map_df(files_to_read, function(x) {
  river_mile <- str_match(x, "[0-9]+\\.?[0-9]+")[,1]
  read_tsv(x, skip = 1, col_names = c("flow", "parker_qs", "wilcock_qs", "gaeuman_qs")) |> 
    mutate(river_mile = as.numeric(river_mile))
})


rating_curve <- rating_curves_by_rm |> 
  rename(flow_cms = flow) |> 
  pivot_longer(parker_qs:gaeuman_qs, names_to = "curve", values_to = "transport_m3_per_second") |> 
  mutate(
    flow_cfs = 35.315 * flow_cms,
    flow_cfd = flow_cfs * 86400, # cubic feet per day
    transport_ft3_per_second = 35.315 * transport_m3_per_second,
    transport_ft3_per_day = 86400 * transport_ft3_per_second # transport per day
  ) |> 
  group_by(flow_cfs, river_mile) |> 
  summarise(
    sed_ft3_per_second_min = min(transport_ft3_per_second), 
    sed_ft3_per_second_avg = mean(transport_ft3_per_second),
    sed_ft3_per_second_max = max(transport_ft3_per_second),    
    sed_ft3_per_day_min = min(transport_ft3_per_day), 
    sed_ft3_per_day_avg = mean(transport_ft3_per_day),
    sed_ft3_per_day_max = max(transport_ft3_per_day)
  ) |> 
  ungroup() |> 
  group_by(flow_cfs) |> 
  summarise(
    sed_ft3_per_second_min = mean(sed_ft3_per_second_min), 
    sed_ft3_per_second_avg = mean(sed_ft3_per_second_avg),
    sed_ft3_per_second_max = mean(sed_ft3_per_second_max),
    sed_ft3_per_day_min = mean(sed_ft3_per_day_min), 
    sed_ft3_per_day_avg = mean(sed_ft3_per_day_avg),
    sed_ft3_per_day_max = mean(sed_ft3_per_day_max),
  ) |>
  ungroup() |> 
  mutate(flow_cfd = flow_cfs * 86400)



# extrapolate a lower bound to the curve at 0,0
srh2d_upper_sac_rating_curves <- rating_curve |> 
  add_row(flow_cfs = 0,
          flow_cfd = 0,
          sed_ft3_per_day_min = 0, 
          sed_ft3_per_day_avg = 0, 
          sed_ft3_per_day_max = 0, 
          sed_ft3_per_second_min = 0, 
          sed_ft3_per_second_avg = 0, 
          sed_ft3_per_second_max = 0
  )

usethis::use_data(srh2d_upper_sac_rating_curves, overwrite = TRUE)

srh2d_upper_sac_rating_curves %>% 
  pivot_longer(names_to = "curve", 
               values_to = "sediment_transport", 
               sed_ft3_per_second_min:sed_ft3_per_second_max) %>%
  mutate(
    curve_label = case_when(
      curve == "sed_ft3_per_second_min" ~ "Min (CFS)",
      curve == "sed_ft3_per_second_avg" ~ "Avg (CFS)",
      curve == "sed_ft3_per_second_max" ~ "Max (CFS)",
    ), 
    curve_label = factor(curve_label, 
                         levels = c(
                           "Max (CFS)",
                           "Avg (CFS)",
                           "Min (CFS)"
                         ))) %>% 
  ggplot(aes(flow_cfs, sediment_transport, color = curve_label)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2.5) + 
  labs(x = "flow (cfs)", y = "Sediment Transport (cfs)", 
       title = "Sediment Transport Capacity Rating Curves", 
       subtitle = "Flow (cfs) to Transport (cfs)",
       color = NULL) + 
  theme_bw() + 
  theme(legend.position="bottom") 

# data used for first scaledown
kwk_usgs <- read_rds("data-raw/spawning-habitat-decay/data/kwk-flows-1980-2022.rds")

objective_func <- function(threshold) {
  
  # scale down the tranport curves to just the d50mm threshold of movement
  scaled_sed_transport <- rating_curve$sed_ft3_per_day_min * 
    DSMhabitat::gravel_size_to_prop_of_movement$avg_fraction
  
  # create an approxfun given a threshold of movement (this value will be searched by the optim function)
  calib_sed_curve <- approxfun(rating_curve$flow_cfs, 
                               scaled_sed_transport * 
                                 rep(threshold, 
                                     length(rating_curve$flow_cfs)))
  
  # convert square meters to cubic feet, assume 2ft depth
  starting_volume <- (254690.3 * 10.764) * 2
  
  calib_kwk_sed_transport <- tibble(
    date = kwk_usgs$Date, 
    flow = kwk_usgs$Flow, 
    sediment_transport_f3_day = calib_sed_curve(flow)
  ) |> 
    mutate(sediment_transport_f3_day = ifelse(is.na(sediment_transport_f3_day), 0, sediment_transport_f3_day))
  
  
  calib_kwk_sed_transport_sim <- calib_kwk_sed_transport |> 
    filter(date >= "2015-01-01", date <= "2017-04-01") |> 
    mutate(sediment_transport_f3_day_accum = cumsum(sediment_transport_f3_day),
           current_vol = starting_volume - sediment_transport_f3_day_accum)
  
  last_volume <- calib_kwk_sed_transport_sim |> tail(1) |> pull(current_vol)
  
  # return absolute distance to zero
  return(abs(last_volume - 0))
}


# We want to optimize the function by the threshold value that results
# in the volumne nearest zero.
result <- optimise(objective_func, interval = c(0, 1), maximum = FALSE)

sac_river_observation_scaledown <- result$minimum

# create a new curve with this scaledown applied
flow_cfs_to_sed_cfd_calibrated <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_min * 
    DSMhabitat::gravel_size_to_prop_of_movement$avg_fraction * 
    rep(sac_river_observation_scaledown, 
        length(rating_curve$flow_cfs))
)

calibrated_kwk_sed_transport <- tibble(
  date = kwk_usgs$Date, 
  flow = kwk_usgs$Flow, 
  sediment_transport_f3_day = flow_cfs_to_sed_cfd_calibrated(flow)
) |> 
  mutate(sediment_transport_f3_day = ifelse(is.na(sediment_transport_f3_day), 0, sediment_transport_f3_day))


starting_volume <- (254690.3 * 10.764) * 2

kwk_sed_transport_sim <- calibrated_kwk_sed_transport |> 
  filter(date >= "2015-01-01", date <= "2017-04-01") |> 
  mutate(sediment_transport_f3_day = ifelse(is.na(sediment_transport_f3_day), 0, sediment_transport_f3_day), 
         sediment_transport_f3_day_accum = cumsum(sediment_transport_f3_day),
         start_vol = starting_volume,
         current_vol = start_vol - sediment_transport_f3_day_accum)

p1 <- kwk_sed_transport_sim |> 
  ggplot() + 
  geom_line(aes(date, current_vol)) + 
  labs(x = "", y = "Spawning Habitat Vol. (cubic feet)") + 
  theme_bw()

p2 <- kwk_usgs |> 
  filter(Date >= "2015-01-01", Date <= "2017-04-01") |> ggplot(aes(Date, Flow)) + geom_line() + 
  theme_bw()

# plot to confirm scaledown did the right thing
grid.arrange(p2, p1, nrow = 2)


# apply to upper sac 
upper_sac_flows_dsm <- DSMflow::flows_cfs$biop_itp_2018_2019 |> 
  filter(year(date) >= 1979, year(date) <= 2000) |> 
  select(date, flow = `Upper Sacramento River`)

upper_sac_decay <- tibble(
  date = upper_sac_flows_dsm$date,
  flow = upper_sac_flows_dsm$flow,
  decay_cfd = ifelse(is.na(
    (x <- flow_cfs_to_sed_cfd_calibrated(flow))), 
    0, x
  ), 
  decay_cfm = decay_cfd * days_in_month(month(date)),
  decay_sqm = decay_cfm / 2, 
  decay_acres_month = decay_sqm / 43560 
)


# the next scaledown is a further scaledown based on input from John Hannon and Chris Hammersmark
# they expect to see an average loss of ~2acres per year in the upper sacramento river

sac_augs <- read_csv("data-raw/spawning-habitat-decay/data/sacramento_river_gravel_augmentation_data.csv", 
                     col_types = c("c", "d", "d"))
stillwater_sac_augs <- readxl::read_excel("data-raw/spawning-habitat-decay/data/gravel-augmentations-stillwater-1978_2006.xlsx") |> 
  mutate(vol_ft3 = volume_m3 * 35.315, 
         area_ft2 = vol_ft3 / 2, 
         acres = area_ft2 / 43560) |> 
  select(site_name, river_mile, acres, year)

aug_acres_fill <- 0

stillwater_sac_augs_totals <- stillwater_sac_augs |> 
  group_by(year) |>
  summarise(
    acres = sum(acres)
  ) |> 
  mutate(date = as_date(paste0(year, "-01-31"))) |> 
  filter(year(date) < 1997) |> 
  select(date, acres) |> 
  mutate(source = "S")

sac_aug_totals <-
  sac_augs |> 
  group_by(date) |> 
  summarise(tons = sum(tons)) |> 
  filter(!is.na(date)) |> 
  mutate(
    date = as_date(paste0(date, "-01-31")),
    cubic_yard = tons / 1.5, 
    cubic_feet = cubic_yard * 27,
    sq_feet = cubic_feet / 2, 
    acres = sq_feet / 43560,
    source = "H"
  ) |> 
  select(date, acres, source) |>  
  bind_rows(stillwater_sac_augs_totals) |> 
  arrange(date) |> 
  filter(!is.na(acres))

gt(sac_aug_totals |> select(-source))

decays <- upper_sac_decay |> select(date, decay_acres_month, flow) |> 
  mutate(scaled_decay = decay_acres_month * .195)

augmentations <- sac_aug_totals |> select(date, aug_acres=acres) 

decays_and_augs <- decays |> 
  left_join(augmentations, by=c("date"="date")) |> 
  mutate(aug_acres = ifelse(is.na(aug_acres), 0, aug_acres), 
         aug_minus_decay = aug_acres - decay_acres_month, 
         aug_minus_decay_scaled = aug_acres - scaled_decay,
         month = month(date),
         year = year(date)) 

decays_and_augs |> 
  group_by(year) |> 
  summarise(
    avg_diff = mean(aug_minus_decay_scaled), 
    sum_loss = sum(aug_minus_decay_scaled)
  ) |> 
  pull(sum_loss) |> 
  mean()

domain_expert_additional_scaledown <- .13
total_scaledown <- domain_expert_additional_scaledown * sac_river_observation_scaledown


# Apply to all watersheds -----------------------------

MIN_flow_cfs_to_sed_cfd_final <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_min * 
    DSMhabitat::gravel_size_to_prop_of_movement$avg_fraction * 
    total_scaledown
)

AVG_flow_cfs_to_sed_cfd_final <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_avg * 
    DSMhabitat::gravel_size_to_prop_of_movement$avg_fraction * 
    total_scaledown
)

MAX_flow_cfs_to_sed_cfd_final <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_max * 
    DSMhabitat::gravel_size_to_prop_of_movement$avg_fraction * 
    total_scaledown
)

# Exceedance probs --------------------------------------
dsm_flows <- DSMflow::flows_cfs$biop_itp_2018_2019 |> 
  pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs")

watersheds_with_decay <- names(which(DSMhabitat::watershed_decay_status))

exceedance_curves <- map(watersheds_with_decay, function(w) {
  d <- dsm_flows |> filter(watershed == w) |> 
    mutate(cume_dist = dplyr::cume_dist(-flow_cfs)) |> 
    arrange(desc(cume_dist))
  
  approxfun(x = d$cume_dist, y = d$flow_cfs)
}) |> 
  set_names(watersheds_with_decay)


exceedance_curves$`Upper Sacramento River`(0.0569578) # ~18000

upper_sac_exceedance_at_18k <- 0.0569578

watershed_offsets <- map_dbl(fallRunDSM::watershed_labels, function(w) {
  if (w %in% watersheds_with_decay) {
    exceedance_curves[[w]](upper_sac_exceedance_at_18k)
  } else {
    NA_real_
  }
}) |> set_names(fallRunDSM::watershed_labels)

watershed_spawning_decays <- map2(fallRunDSM::watershed_labels, watershed_offsets, function(w, x) {
  
  if (w %in% watersheds_with_decay) {
    dsm_flows |>
      filter(watershed == w, year(date) %in% 1979:2000) |> 
      mutate(flow_adjusted = flow_cfs - x, 
             decay_min = MIN_flow_cfs_to_sed_cfd_final(flow_adjusted), 
             decay_avg = AVG_flow_cfs_to_sed_cfd_final(flow_adjusted), 
             decay_max = MAX_flow_cfs_to_sed_cfd_final(flow_adjusted) 
      ) |> 
      pivot_longer(names_to = "decay_type", values_to = "decay_amount", -c(date, watershed, 
                                                                           flow_cfs, flow_adjusted)) |> 
      mutate(decay_cfd = ifelse(is.na(decay_amount), 0, decay_amount), 
             decay_cfm = decay_cfd * days_in_month(month(date)),
             decay_sqm = decay_cfm / 2, 
             decay_acres_month = decay_sqm / 43560, 
             decay_type = stringr::str_extract(decay_type, "min|avg|max")) |> 
      select(date, watershed, flow_cfs, decay_type, decay_acres_month) 
  } else {
    dsm_flows |> 
      filter(watershed == w, year(date) %in% 1979:2000) |> 
      transmute(
        date, watershed, flow_cfs, decay_type = "none", decay_acres_month = 0
      )
  }
}) |> 
  set_names(fallRunDSM::watershed_labels)

usethis::use_data(watershed_spawning_decays, overwrite = TRUE)

watershed_decays$`American River` |> 
  ggplot(aes(date, decay_acres_month, color = decay_type)) + geom_line()

watershed_decays$`American River` |> 
  group_by(decay_type) |> 
  mutate(agg_decay = cumsum(-decay_acres_month)) |> 
  ggplot(aes(date, agg_decay, color = decay_type)) + geom_line()

# steps we need to go through to apply the decay on this dataset
# 1. get the decay amount per watershed, have it filtered to the level min,avg,max,none
# compute the amount of habitat to be removed
watershed_decay_level_lookups <- c(
  `Upper Sacramento River` = "min", `Antelope Creek` = "none", 
  `Battle Creek` = "none", `Bear Creek` = "none", `Big Chico Creek` = "none", 
  `Butte Creek` = "none", `Clear Creek` = "min", `Cottonwood Creek` = "none", 
  `Cow Creek` = "none", `Deer Creek` = "none", `Elder Creek` = "none", 
  `Mill Creek` = "none", `Paynes Creek` = "none", `Stony Creek` = "max", 
  `Thomes Creek` = "none", `Upper-mid Sacramento River` = "none", 
  `Sutter Bypass` = "none", `Bear River` = "none", `Feather River` = "avg", 
  `Yuba River` = "min", `Lower-mid Sacramento River` = "none", `Yolo Bypass` = "none", 
  `American River` = "max", `Lower Sacramento River` = "none", `Calaveras River` = "min", 
  `Cosumnes River` = "none", `Mokelumne River` = "avg", `Merced River` = "avg", 
  `Stanislaus River` = "avg", `Tuolumne River` = "min", `San Joaquin River` = "none"
)


watersheds_with_decay <- names(which(DSMhabitat::watershed_decay_status))

spawning_decay_multiplier <- purrr::map(names(watershed_decay_level_lookups), function(w) {
  if (w %in% watersheds_with_decay) {
    decay <- DSMhabitat::watershed_decays[[w]] |> 
      dplyr::filter(decay_type == watershed_decay_level_lookups[w]) |> 
      dplyr::mutate(decay_accum = cumsum(decay_acres_month), 
                    decay_mult = 1 - (decay_accum / DSMhabitat::spawning_habitat_average$fr[w]), 
                    year = lubridate::year(date),
                    month = lubridate::month(date)) |> 
      dplyr::select(year, month, decay_mult) |> 
      tidyr::pivot_wider(names_from = "year", values_from = "decay_mult")
    
    matrix(unlist(decay[,-1]), nrow = 12, dimnames = list(month.abb, 1979:2000))
    
  } else {
    matrix(1, nrow = 12, ncol = 22)
  }
}) |> 
  setNames(names(watershed_decay_level_lookups))

usethis::use_data(spawning_decay_multiplier, overwrite = TRUE)
