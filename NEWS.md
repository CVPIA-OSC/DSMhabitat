# DSMhabitat 2.0

The 2022 update to DSMhabitat includes the following changes:

* **Fall Run spawning habitat decay updates**. A new analysis for spawning habitat decay has 
been added to the package. In doing so a few datasets have been added to the package as
well as a new script under `data-raw/spawning-habitat-decay` that described the new habitat calculations. The following data has been added along with correspondings documentations:
    - `DSMhabitat::watershed_spawning_decays` - dataframes with raw decay for three calcualted curves
    - `DSMhabitat::watershed_decay_status` - a convinience dataframe that shows whether a given watershed has spawning decay or not.
    - `DSMhabitat::spawning_habitat_average` - a convinience dataset with average spawning habitat by watershed and run.
    - `DSMhabitat::spawning_decay_multiplier` - a datset cotaining 31 watershed decay multipliers. Currently this cotains only Fall-run values.

* **CALSIM updates**. Update DSMhabitat datasets integrating in new 2019 biop flows. The result of
doing so means each dataset that is generated using flow as an input will have the following form: 
`fr_fp$biop_2008_2009` and `fr_fp$biop_itp_2018_2019`.


# DSMhabitat 1.0

Habitat related datasets for use with the following DSM models:

* [Fall Run DSM (v2.0)](https://github.com/CVPIA-OSC/fallRunDSM/releases/tag/v2.0)
* [Late Fall Run DSM (v1.0)](https://github.com/CVPIA-OSC/fallRunDSM/releases/tag/v1.0)
* [Winter Run DSM (v2.0)](https://github.com/CVPIA-OSC/winterRunDSM/releases/tag/v2.0)
* [Spring Run DSM (v2.0)](https://github.com/CVPIA-OSC/springRunDSM/releases/tag/v2.0)

# DSMhabitat 0.0.1

Alpha release of habitat input data migrated from CVPIA-OSC/DSMhabitat
