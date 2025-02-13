
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Fluxible

<!-- badges: start -->
<!-- badges: end -->

The Fluxible R package is made to transform any dataset of gas
concentration over time into a gas flux dataset. It was originally made
to be used with a closed loop chamber system connected to a gas
analyzer.

The goal of fluxible is to provide a workflow that removes individual
evaluation of each flux, reduces risk of bias, and makes it
reproducible. Users set specific data quality standards and selection
parameters as function arguments that are applied to the entire dataset.
The package runs the calculations automatically, without prompting the
user to take decisions mid-way, and provides quality flags and plots at
the end of the process for a visual check. This makes it easy to use
with large flux datasets and to integrate into a reproducible workflow.
Using the Fluxible R package makes the workflow reproducible, increases
compatibility across studies, and is more time efficient.

## Installation

Fluxible can be installed from CRAN.

``` r
install.packages("fluxible")
```

You can install the development version of fluxible from the [GitHub
repo](https://github.com/Plant-Functional-Trait-Course/fluxible) with:

``` r
# install.packages("devtools")
devtools::install_github("plant-functional-trait-course/fluxible")
```

## Short example

``` r
library(fluxible)

conc_df <- flux_match(
  co2_df_short,
  record_short,
  datetime,
  start,
  conc,
  startcrop = 10,
  measurement_length = 220
)

slopes_df <- flux_fitting(
  conc_df,
  conc,
  datetime,
  fit_type = "exp",
  end_cut = 30
)
#> Cutting measurements...
#> Estimating starting parameters for optimization...
#> Optimizing fitting parameters...
#> Calculating fits and slopes...
#> Done.

slopes_flag_df <- flux_quality(
  slopes_df,
  conc
)
#> 
#>  Total number of measurements: 6
#> 
#>  ok   6   100 %
#>  discard      0   0 %
#>  zero     0   0 %
#>  force_discard    0   0 %
#>  start_error      0   0 %
#>  no_data      0   0 %
#>  force_ok     0   0 %

flux_plot(
  slopes_flag_df,
  conc,
  datetime,
  f_ylim_lower = 390,
  f_ylim_upper = 650,
  facet_wrap_args = list(
    ncol = 3,
    nrow = 2,
    scales = "free"
  )
)
#> Plotting in progress
```

<img src="man/figures/README-short-example-1.png" width="100%" />

``` r

fluxes_df <- flux_calc(
  slopes_flag_df,
  f_slope_corr,
  datetime,
  temp_air,
  conc_unit = "ppm",
  flux_unit = "mmol",
  cols_keep = c("turfID", "type"),
  cols_ave = c("temp_soil", "PAR"),
  chamber_volume = 24.5,
  tube_volume = 0.075,
  atm_pressure = 1,
  plot_area = 0.0625
)
#> Cutting data according to 'keep_arg'...
#> Averaging air temperature for each flux...
#> Creating a df with the columns from 'cols_keep' argument...
#> Creating a df with the columns from 'cols_ave' argument...
#> Calculating fluxes...
#> R constant set to 0.082057
#> Concentration was measured in ppm
#> Fluxes are in mmol/m2/h

fluxes_gep <- flux_gep(
  fluxes_df,
  type,
  datetime,
  id_cols = "turfID",
  cols_keep = c("temp_soil")
)
#> Warning in flux_gep(fluxes_df, type, datetime, id_cols = "turfID", cols_keep = c("temp_soil")): 
#>  NEE missing for measurement turfID: 156 AN2C 156

fluxes_gep
#> # A tibble: 9 × 5
#>   datetime            type  f_flux temp_soil turfID      
#>   <dttm>              <chr>  <dbl>     <dbl> <chr>       
#> 1 2022-07-28 23:43:35 ER      47.7      10.8 156 AN2C 156
#> 2 2022-07-28 23:47:22 GEP     10.3      10.7 74 WN2C 155 
#> 3 2022-07-28 23:47:22 NEE     31.0      10.7 74 WN2C 155 
#> 4 2022-07-28 23:52:10 ER      20.7      10.7 74 WN2C 155 
#> 5 2022-07-28 23:59:32 GEP    -27.2      10.8 109 AN3C 109
#> 6 2022-07-28 23:59:32 NEE     41.5      10.8 109 AN3C 109
#> 7 2022-07-29 00:03:10 ER      68.7      10.5 109 AN3C 109
#> 8 2022-07-29 00:06:35 GEP     NA        12.2 29 WN3C 106 
#> 9 2022-07-29 00:06:35 NEE     26.1      12.2 29 WN3C 106
```

## Further developments

### Segmentation tool

We are working on a tool to automatically select the window of the
measurement on which to fit a model. This selection will be based on
environmental variable, such as photosynthetically active radiation
(PAR), measured simultaneously.

### More fits

As we want fluxible to fit the use of as many projects as possible, more
fitting expressions will be included in the flux_fitting function. Feel
welcome to get in touch if you wish to include yours in fluxible.

## Contact

Joseph Gaudard, University of Bergen, Norway

<joseph.gaudard@pm.me>

[GitHub profile](https://github.com/jogaudard)

## Dissemination

Gaudard, J., Telford, R., Vandvik, V., and Halbritter, A. H.: Fluxible:
an R package to calculate ecosystem gas fluxes in a reproducible and
automated workflow., EGU General Assembly 2024, Vienna, Austria, 14–19
Apr 2024, EGU24-956, <https://doi.org/10.5194/egusphere-egu24-956>,
2024.

<!-- [Direct link to the poster](https://github.com/Plant-Functional-Trait-Course/fluxible/blob/main/dissemination/poster_EGU24_jgaudard.pdf) -->
<!-- [Link to the abstract](https://meetingorganizer.copernicus.org/EGU24/EGU24-956.html) -->
<!-- #### References -->
