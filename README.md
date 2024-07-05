# Excess mortality attributed to heat and cold: a health impact assessment study in 854 cities in Europe
 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10288665.svg)](https://doi.org/10.5281/zenodo.10288665)
 
Code implementing the analysis from the paper:

> Excess mortality attributed to heat and cold: a health impact assessment study in 854 cities in Europe (2023) *The Lancet Planetary Health* <https://doi.org/10.1016/S2542-5196(23)00023-2>

To get the full set of exposure-response functions and results derived in this paper, please consult the associated Zenodo repository ([DOI:10.5281/zenodo.10288665](https://doi.org/10.5281/zenodo.10288665)).

This code is not fully reproducible because of restrictive data agreements on mortality series. However, intermediary data, in particular the clean metadata and first-stage results, are freely available on the Zenodo repository associated with the paper. This allows running the second part of the analysis, namely from script *06_PrepSecondStage.R* on. See details in the **Code** section below. 

A fully reproducible example of the methodology is in production and will be available upon publication of a dedicated methodological paper in the near future. In the meantime, please refer to this code for all methodological details of the analysis, or contact either the first or senior author if something is unclear. 

## Code

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

## Results

An example on how to use the results of this analysis is the analysis performed in the following paper. Please see the Zenodo (DOI:10.5281/zenodo.10288665) repository for details on how to extract the results of the analysis.

> Iungman, T., Cirach, M., Marando, F., Pereira Barboza, E., Khomenko, S., Masselot, P., Quijal-Zamorano, M., Mueller, N., Gasparrini, A., Urquiza, J., Heris, M., Thondoo, M., Nieuwenhuijsen, M., 2023. **Cooling cities through urban green infrastructure: a health impact assessment of European cities**. *The Lancet*. https://doi.org/10.1016/S0140-6736(22)02585-5

## Intermediary data

Similarly, the Zenodo repository includes the data necessary to run the scripts in this repository. Please download the files into a data subfolder to use them in the analysis. Alternatively, the R package `zen4R` can be used to download the files directly from R before being used. The way to do it is shown in the script *06_PrepSecondStage.R*.
