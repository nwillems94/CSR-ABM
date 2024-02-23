The Corporate Social Responsibility Agent-Based Model (CSR-ABM)
=====================================================

#### Author 
[Nick Willems](mailto:nwillems@utexas.edu)
#### Preferred Citation
...

-----------------------------------------------------

Welcome to the CSR-ABM: an empirically-grounded, agent-based modeling framework of corporate decision making in the face of economic, regulatory, and social influences. Agent's here consider not only the isolated cost/benefit of specific actions, but also a range of exogenous factors:
- The actions of peer firms. Especially larger, more successful ones.  
- Pressure from social activists.  
- Shareholders who will value CSR efforts.  
- Increased profits from improved reputation. This can manifest through consumers who will pay more for "premium" goods or reduced costs of labor and capital.  


In this specific application, the decision to avoid gas flaring is viewed as an act of corporate social responsibility by upstream oil and gas firms. 
Agents (firms) are assigned heterogenous assets and interact in markets. Assets are empirically grounded oil and gas leases. Agents engage in 2 basic activities: "exploration" (i.e., acquiring new assets) and "development" (i.e., investing in existing assets). An agent's objective is to maximize their composite market value:  

```math
\begin{align} 
\text{Market Value} &= (\text{Net Cashflow}) &+ \quad&(\text{Net Social value}) \\ 
MV &= (\pi + \Delta\pi - C) &+ \quad&(\rho_\theta C - A)
\end{align}
```

By spending on limiting flaring (CSR: $C$), firms can avoid pressure from social activists ($A$) and generate additional revenue ($\Delta\pi$) by selling certified gas at a premium price. Simultaneously, shareholders value firms' efforts ($\rho_\theta C$), bolstering stock prices. 

The agents, assets, and market conditions are each empirically grounded and validated using [freely available public data](#Data). To read more about the model and research, see this publication.   


# Installation
If you encounter issues with either `R` or package versions, please submit an issue.  

## Download and install R
This code was originally written for `R 4.0.3`, but any later versions should also work.  

## Install required packages
With the goal of minimizing package dependency, there are very few packages required for running the model. Processing the inputs and producing outputs, however, require the use of some more specific packages. I expect the code to continue working as packages evolve over time, but, for the purposes of future-proofing, the correct package versions should be available by calling: 

```r
install.packages(..., repos="https://packagemanager.posit.co/cran/2020-07-13")
```
With the exception of `svglite` which should be version 2.1.0. 

Packages for recreating inputs (not required if using the provided model inputs):  
- `readxl`
- `ggplot2`
- `sf`
- `maps`
- `rmarkdown`
- `formatR`
- `knitr`
- `printr`

Packages to run the model:  
- `data.table`
- `future`
- `future.apply`
- `ggplot2` (for validation scripts)
- `rmarkdown` (for validation scripts)

Packages to post-process results & generate graphics:  
- `DBI`
- `RSQLite`
- `svglite`
- `ggplot2`
- `ggrepel`


The example `bash` `SLURM` scripts assume the following commands (standard Unix utilities which are also included as part of e.g., `Rtools` or `msys2`) are also available from the command line:  
- `awk`
- `cat`
- `cut`
- `grep`
- `sed`
- `unzip`
- `gzip`

## Download code
You're already here. You can download the code and input data from [GitHub](https://github.com/nwillems94/FlaringABM/).

# Running the model  
## Data  
All data needed for running the model are provided in the `inputs/processed/` directory. The data were processed and formatted using `inputs/firms.Rmd`, `inputs/leases.Rmd`, and `inputs/market_history.Rmd`. The intermediate results of each script are cataloged in their associated `.html` files. If you want to experiment with model configurations but use the provided inputs you can skip to the next section.  

Users wishing to replicate or update bundled datasets will need to compile the `Rmarkdown` scripts after downloading the raw input data:  

| Usage(s)                              | Data Type | File / Directory Name | Source |
| :---                                  | :---      | :---  | :---  |
| ***Lease Data***                      |           |  |  |
| Lease numbers<br>Production Data      |   `csv`   | PDQ_DSV.zip | [Texas Railroad Commission](https://www.rrc.texas.gov/resource-center/research/data-sets-available-for-download/) |
| Play Boundaries                       |   `shp`   | PermianBasin_Boundary_Structural_Tectonic | [EIA](https://www.eia.gov/maps/maps.htm) |
|                                       |           | ShalePlays_AboYeso_GlorietaYeso_Spraberry_EIA |  |
|                                       |           | EagleFord_Play_Boundary_Elevation_Isopach_EIA |  |
| Gas processing plants                 |   `shp`   | NaturalGas_ProcessingPlants_US_EIA | [EIA](https://atlas.eia.gov/datasets/natural-gas-processing-plants/) |
| Crude oil pipelines                   |   `shp`   | CrudeOil_Pipelines_US_EIA | [EIA](https://atlas.eia.gov/datasets/eia::crude-oil-pipelines/) |
| Well latitude, longitude, and depth   |   `csv`   | dbf900.txt.gz | [Texas Railroad Commission](https://www.rrc.texas.gov/resource-center/research/data-sets-available-for-download/) |
| Well depth and type                   |   `csv`   | daf804.txt.gz | [Texas Railroad Commission](https://www.rrc.texas.gov/resource-center/research/data-sets-available-for-download/) |
| Decline Curve reference parameters    |   `csv`   | decline_curves.xlsx | [EIA](https://www.eia.gov/analysis/drilling/curve_analysis/archive/2020/) |
| ***Market Data***                     |           |  |  |
| Drilling productivity                 |   `csv`   | dpr-data.xlsx | [EIA](https://www.eia.gov/petroleum/drilling/archive/2021/10/) |
| Drilling cost index                   |   `csv`   | NG_ENR_WELLCOST_S1_A.xls | [EIA](https://www.eia.gov/dnav/ng/NG_ENR_WELLCOST_S1_A.htm) |
| Drilling cost index                   |   `pdf`   | MIT_composite_index.csv | [US DOE, MIT](https://doi.org/10.2172/1220063) |
| Drilling cost index                   |   `pdf`   |  | [IEA](https://www.iea.org/data-and-statistics/charts/iea-us-shale-upstream-cost-index-2005-2020) |
| Oil prices                            |   `csv`   | Cushing_OK_WTI_Spot_Price_FOB.csv | [EIA](https://www.eia.gov/dnav/pet/hist/rwtcW.htm) |
| Natural gas prices                    |   `csv`   | Henry_Hub_Natural_Gas_Spot_Price_weekly.csv | [EIA](https://www.eia.gov/dnav/ng/NG_PRI_FUT_S1_M.htm) |
|                                       |           | Henry_Hub_Natural_Gas_Spot_Price.csv |  |
| Natural gas marketed production       |   `csv`   | U.S._Natural_Gas_Marketed_Production.csv | [EIA](https://www.eia.gov/dnav/ng/hist/n9050us2m.htm) |
|                                       |           | Texas_Natural_Gas_Marketed_Production.csv | [EIA](https://www.eia.gov/dnav/ng/hist/n9050tx2m.htm) |
| Electricity prices                    |   `csv`   | f8612017_table6.xlsx | [EIA](https://www.eia.gov/electricity/sales_revenue_price/) |
| Green electricity premiums            |   `csv`   | utility-green-pricing-program-list.xlsx | [NREL](https://www.nrel.gov/analysis/green-power.html) |

These data are not ingested by the model directly, they are either processed and combined into a new, formatted input file or used for the purposes of validation or parameter selection. Note file names may change as datasets are updated by their respective publishers.  

## Configuration
The model configuration can be altered either by editing `flaringABM_exe.R` directly or by using command line arguments. The default parameters are specified below:  

| **Parameter**                          | **Value**                     | **Source**                                                                        |
|----------------------------------------|-------------------------------|-----------------------------------------------------------------------------------|
| Number of runs                         | 32                            | NA                                                                                |
| Number of agents                       | 2,000                         | Number of operators producing oil and gas in Texas                                |
| Total activist funds                   | \$500,000 per month           | Greenpeace annual spending on energy activism                                     |
| Flaring threshold                      | 0.05                          | Half of baseline                                                                  |
| Proportion of green market             | Linear growth from 0 to 0.075 | New York City residential demand                                                  |
| Premium for green gas                  | 1.07-1.3                      | Green electricity prices                                                          |
| Shareholder valuation                  | 0.5                           | Calibrated                                                                        |
| Percent of time doing exploration      | 11/12                         | NA                                                                                |
| Probability of discovering a new asset | 1.23                          | Validated based on EIA Drilling productivity report                               |
| Probability of imitation               | 0.5                           | NA                                                                                |


## Execution
Once the inputs are in place, running the model is as simple as calling `Rscript flaringABM_exe.R` from the command line. The model configuration can be altered either by editing `flaringABM_exe.R` directly or by using command line arguments. There are several example bash scripts included in the `scripts/` directory which demonstrate those options. The scripts are designed for `par_social_start.sh` to be run first and `graphics_finish.sh` last. The sample scripts are based on a machine with 96 cores and parallelized (hence `par_`) for a user requesting 32 stochastic copies of each configuration.  

| Script            | Run Description   |
| ---               | ---               |
| `par_social`      | A baseline model (everything on)<br> Configurations without: imitiation, product differentiation, activism, shareholder valuation |
| `par_market`      | Vary certified market sizes and thresholds |
| `par_actvism`     | Vary activist strategies |
| `par_reporting`   | Simulate different kinds of false reporting |



## Processing
A significant volume of outputs are generated by a standard model run. To accommodate this, a post processing script is provided (`flaringABM_postproc.R`) which consolidates the output files and stores them in an `sqlite` database. The script assumes that the data have been concatenated. To avoid memory overhead in `R`, the data should be concatenated directly from the command-line. Each of the `par_*.sh` scripts contains an (the same) example on how to do so.  

The focus is on completeness and portability: the data stored in the database replicate 100% of the data output by the model while occupying significantly less disk space. 

# Creating outputs
Users are encouraged to create their own analyses and graphics. A set of svg outputs are included and can be reproduced by calling `flaringABM_graphics.R`. Importantly, this script requires that the outputs have been formatted by `flaringABM_postproc.R`. 

