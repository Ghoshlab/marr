
[![Travis build
status](https://travis-ci.com/Ghoshlab/marr.svg?branch=master)](https://travis-ci.com/Ghoshlab/marr)
[![codecov](https://codecov.io/gh/Ghoshlab/marr/branch/master/graph/badge.svg?token=K3CDL7MEN2)](https://codecov.io/gh/Ghoshlab/marr)

marr
====

### `marr`: An R package for Maximum Rank Reproducibility (marr) for high-dimensional biological data.

`marr` measures the reproducibility of features per sample pair and
sample pairs per feature in high-dimensional biological replicate
experiments.

### Method

The `marr` paper published in *Journal of American statistical
Association*:

> Philtron, Daisy, et al. “Maximum Rank Reproducibility: A Nonparametric
> Approach to Assessing Reproducibility in Replicate Experiments.”
> Journal of the American Statistical Association 113.523 (2018):
> 1028-1039. <https://doi.org/10.1080/01621459.2017.1397521>

`Paper (in preparation)`

> Ghosh, Tusharkanti, et al. “Reproducibility of Mass Spectrometry based
> Metabolomics Data”

### Installing marr

The R-package **marr** can be installed from GitHub using the R package
[devtools](https://github.com/hadley/devtools):

Use to install the latest version of **marr** from GitHub:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("Ghoshlab/marr")

### Using marr

The main function in the **marr** package is `marr()`. The `marr()`
function needs one required object and three optional objects: (1)
object: a data frame or a matrix or a Summarized Experiment with one
assay object with observations (e.g., metabolites or genes) on the rows
and samples as the columns (e.g. let’s call it `dataSE`). (2)
pSamplepairs (Optional) a threshold value that lies between 0 and 1,
used to assign a feature to be reproducible based on the reproducibility
output of the sample pairs per feature. Default is 0.75. (3) pFeatures
(Optional) a threshold value that lies between 0 and 1, used to assign a
sample pair to be reproducible based on the reproducibility output of
the features per sample pair. Default is 0.75. (4) alpha (Optional)
level of significance to control the False Discovery Rate (FDR). Default
is 0.05.

To run the `marr()` function,

    marrOutput <- marr(object = dataSE, pSamplepairs=0.75,
    pFeatures=0.75, alpha=0.05)

Individual slots can be extracted using accessor methods:

    marrSamplepairs(marrOutput) # extract the distribution of percent
    #reproducible features (column-wise) per sample pair
    
    marrFeatures(marrOutput) # extract the distribution of percent
    #reproducible sample pairs (row-wise) per feature
    
    marrSamplepairsfiltered(marrOutput) # extract the percent of reproducible
    #features based on a threshold value
    
    marrFeaturesfiltered(marrOutput) # extract the percent of reproducible
    #sample pairs based on a threshold value

The percent reproducible sample pairs per feature can be directly
plotted using the `marrPlotFeatures()` function.

    marrPlotFeatures(marrOutput) 

The percent reproducible features per sample pair can be directly
plotted using the `marrPlotSamplepairs()` function.

    marrPlotFeatures(marrOutput) 

For more details, see `vignettes`.

Bug reports
===========

Report bugs as issues on the [GitHub repository new
issue](https://github.com/Ghoshlab/marr/issues/new)

Contributors
============

-   [Tusharkanti Ghosh](https://github.com/tghosh30)
-   [Max McGrath]()
-   [Daisy Philtron]()
-   [Katerina Kechris]()
-   [Debashis Ghosh](https://github.com/ghoshd)
