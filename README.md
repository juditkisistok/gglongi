
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gglongi

<!-- badges: start -->
<!-- badges: end -->

This package allows you to build longitudinal plots. It was born out of
the need to visualize genomic data over time and overlay clinical events
such as treatment, scan dates, date of progression and date of death;
however, as long as your data has a time component and some continuous
value, you can use `gglongi` in your analysis workflow.

## Installation

You can install the development version of gglongi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juditkisistok/gglongi")
```

## Example

This is a basic example which shows you how to create a longitudinal
plot:

``` r
library(gglongi)
data("mafs")
data("treatment")
data("scans")
data("clinical_events")

## basic example code
## colored rectangles represent the type and duration of treatment
## dotted, red and black lines correspond to scan, progression and death dates, respectively

gglongi(mafs, x_val = "time_from_baseline", y_val = "MAF", col_val = "Gene", 
        col_legend = T, mean_line = T, x_title = "Days from baseline sample", 
        y_title = "Mutant allele frequency", treatments = treatment, scans = scans, 
        events = clinical_events, treatment_start = "start_from_baseline", 
        treatment_end = "stop_from_baseline", treatment_drug = "Treatment_drug", 
        scan_date = "Days_from_baseline", event_date = "Days_from_baseline") 
```

<img src="man/figures/README-example-1.svg" width="100%" />
