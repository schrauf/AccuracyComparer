
# AccuracyComparer

<!-- badges: start -->
<!-- badges: end -->

Conduct Significance, Equivalence, Non-inferiority and Superiorty tests between mean accuracies of models,
as assessed with resampling methods such as cross validations.

Companion to work (in press):
> Matías F. Schrauf, Gustavo de Los Campos y Sebastián Munilla. “Comparing genomic prediction models by means of cross validation.” Genomic Selection: Lessons Learned and Perspectives. Special topic in Frontiers in Plant Science (2021): 2648.

## Installation

You can install the development version of AccuracyComparer like so:

``` r
# install.packages("devtools")
devtools::install_github("schrauf/AccuracyComparer")
```

## Example

Basic usage example:

``` r
library(AccuracyComparer)
data(CO2)
fm <- lm(uptake ~ Plant + conc, data = CO2)
compare_means(fm, "Plant", 15.0,
    method = c("sd", "eq"),
    level = 0.99)
```

