# Excess mortality attributed to heat and cold: a health impact assessment study in 854 cities in Europe
 
Code and results from the paper:

> Excess mortality attributed to heat and cold: a health impact assessment study in 854 cities in Europe (2023) *The Lancet Planetary Health*

Full results from the analysis can be found in the *results* folder, allowing to use the derived exposure-response functions in other studies. See the **Results** section below.

A fully reproducible example of the methodology is in production and will be available upon publication of a dedicated methodological paper. In the meantime, the full code of the analysis can be consulted in this repo. The code is not fully reproducible because of restrictive data agreements. However, we make the clean metadata and first-stage results available, allowing to run the second part of the analysis. See the **Code** and **Data** sections below. 

The Tables and Figures of the main mansucript are found in folders *tables* and *figures* respectively.

## Results

The *results* folder provides full results from the analysis. They can be used to perform further analysis using the derived exposure-response functions. A preliminary version of these results have already been used in 

> Iungman, T., Cirach, M., Marando, F., Pereira Barboza, E., Khomenko, S., Masselot, P., Quijal-Zamorano, M., Mueller, N., Gasparrini, A., Urquiza, J., Heris, M., Thondoo, M., Nieuwenhuijsen, M., 2023. **Cooling cities through urban green infrastructure: a health impact assessment of European cities**. *The Lancet*. https://doi.org/10.1016/S0140-6736(22)02585-5

The folder contains:

**coefs.csv.gz**: City and age-group specific spline coefficients. These coefficients are meant to be used with the basis specification given in the paper. In R, assuming a temperature series `x`, this basis can be created as

```
library(dlnm)
basis <- onebasis(x, fun = "bs", degree = 2, knots = quantile(x, c(.1, .75, .9)))
```

**vcov.csv.gz**: Variance-covariance matrix associated with each coefficient vector. Each line corresponds to the *lower triangle* of the matrix for efficiency purposes. Assumin `x` contains a line of the file, the matrix can be reconstructed as

```
m <- matrix(NA, 5, 5)
m[lower.tri(m, diag = T)] <- unlist(x)
m[upper.tri(m)] <- t(m)[upper.tri(m)]
```

**simu** folder: Contains simulated coefficients stored as one file by city and age group. These simulations are used to construct empirical confidence intervals for attribution measures.

**city_results.csv.gz**: Full summary of the results at the city and age group level.

## Code

Due to restricted sharing agreement of mortality data, the analysis is not fully reproducible. However, we provide here the gathered city metadata and first-stage results to make the second part of the analysis fully reproducible. 

The code is broken code into task specific scripts, as follows.

**00_Packages_Parameters.R**: Loads necessary packages and define the parameters of the analysis.

> :warning: The following is not reproducible as MCC mortality series are not publicly available. Note also there have been several changes in the Eurostat database since extraction.

**01_PrepCity.R**: Extract and prepare city-level data.

**02_PrepNUTS.R**: Extract and link NUTS level data.

**03_CleanFill.R**: Temporally aggregate and clean metadata.

**04_PrepMCC.R**: Prepare MCC data and first-stage time series.

**05_FirstStage.R**: Run first-stage city-specific models.

> :heavy_check_mark: The following is reproducible by loading data provided in *data* folder. 

**06_PrepSecondStage.R**: Prepare metadata and first-stage results for use in the meta-analysis model.

**07_SecondStage.R**: Runs second-stage meta-regression model.

**08_Spatial.R**: Krige second-stage BLUP residuals.

**09_ResultsCityAge.R**: Compute excess death rates for each city and age group.

**10_ResultsCityOverall.R**: Compute city-level standardised death rates.

**11_ResultsCountry.R**: Compute excess deaths and standardised rates at country and region level.

**12_ResultsVulnerability.R**: Compute age-specific and component-specific results.

**13_Tables.R**: Create the Tables in the manuscript.

**14_Plots.R**: Create the Figures in the manuscript.

**15_ResultsExport.R**: Export results in the *results* folder.

> :heavy_check_mark: The following produces additional results found in the eAppendix. 

**A1_DataDesc.R**: Produces Tables and Figures describing data.

**A2_ModelSelection.R**: Select second stage model among many possibilities.

**A3_AdditionalPlots.R**: Create additional plots.

## Data

The *data* folder contains cleaned and ready to use data to run the reproducible part of the analysis. See the header of the script *06_PrepSecondStage.R* for how to load these data.

**metacityyear.csv.gz**: Contains all extracted metadata at the annual level.

**metadata.csv.gz**: Contains the metadata used in the second-stage of the analysis. 

**metadesc.csv.gz**: Description of the meta-predictors.

** stage1res.csv.gz**: Contains the results of the first stage of the analysis.

**era5series** folder: contains city-specific Era5Land temperature series (one file for each city).
