library(tidyverse)
library(lubridate)
library(DSMflow)
library(DSMhabitat)

# pull in flow data for delta 
north_delta_flow <- DSMflow::delta_flows$biop_2008_2009 |> 
  select(date, n_dlt_inflow_cfs, s_dlt_inflow_cfs) |> 
  filter(year(date) > 1978, year(date) < 2001) |> 
  mutate(year = year(date), 
         month = month(date), 
         date = as_date(paste(year, month, "01", sep = "-"))) |> 
  rename(`North Delta` = n_dlt_inflow_cfs, 
         `South Delta` = s_dlt_inflow_cfs) |> 
  pivot_longer(cols = c(`North Delta`, `South Delta`), names_to = "watershed", values_to = "flow_cfs") |> 
  glimpse()

# pull in habitat data for delta 
north_delta_habitat <- delta_rearing_habitat |> 
  select(date, `North Delta`, `South Delta`) |> 
  pivot_longer(cols = c(`North Delta`, `South Delta`), names_to = "watershed", values_to = "habitat_sqmt") |> 
  glimpse()

# join habitat and flow by date 
habitat_by_flow <- left_join(north_delta_habitat, north_delta_flow) |> 
  glimpse()

# flow to suitable area curve 
# curve does not look like as expected - very jagged  
habitat_by_flow %>% 
  ggplot(aes(x = flow_cfs, y = habitat_sqmt, color = watershed)) +
  geom_line()
