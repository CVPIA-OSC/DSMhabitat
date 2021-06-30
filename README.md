<img src="man/figures/cvpia_logo.jpg" align="right" width="40%"/>

### Modeled Habitat Areas
*This package is for estimating spawning and rearing (instream and floodplain) habitat within the Sacramento and San Joaquin river systems for use with the CVPIA salmon life cycle model.*

#### Installation   

``` r
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/DSMhabitat")
```

#### Usage
This package provides flow related datasets to the [`chinookDSM `](https://flowwest.github.io/cvpiaModels/) package.

``` r
# datasets within the package
data(package = 'DSMhabitat')
```

#### About the Models
Output from the CALSIM II operational model is used to generate hydrologic inputs for the CVPIA Decision Support Model (DSM). CALSIM II is the model used to simulate California State Water Project (SWP) / Central Valley Project (CVP) operations. CALSIM II was developed in the Water Resource Integrated Modeling System (WRIMS model engine or WRIMS), a generalized water resources modeling system for evaluating operational alternatives of large, complex river basins. 

The 2017 DSM uses the existing conditions simulation from the [2009 CalSim-II Draft SWP Delivery Reliability Studies 2009](http://baydeltaoffice.water.ca.gov/modeling/hydrology/CalSim/Downloads/CalSimDownloads/CalSim-IIStudies/SWPReliability2009/index.cfm). The CALSIM II run is a Reclamation product used to replicate current operations for comparison with proposed adjustments under an ongoing Endangered Species Act consultation with the National Marine Fisheries Service.  

A current NMFS Biological Opinion concluded that, as proposed, CVP and SWP operations were likely to jeopardize the continued existence of four federally- listed anadromous fish species:   
  - Sacramento River winter-run Chinook salmon  
  - Central Valley spring-run Chinook salmon  
  - California Central Valley steelhead  
  - Southern distinct population segment of the North American green sturgeon   

This CALSIM II run was used as the basis of comparison for other potential operations that could offset impacts to listed species.

  
[More information on CALSIM II](https://water.ca.gov/Library/Modeling-and-Analysis/Central-Valley-models-and-tools/CalSim-2)    
[CALSIM II Schematic](https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg)  

### Dependencies
The `DSMhabitat` package provides data for several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below. 
<img src="man/figures/dependencyChain.svg" width="100%"/>

<div style="margin-top: 40px;">Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/></div>
