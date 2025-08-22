# GPP calculation

    Code
      flux_diff(co2_fluxes, type, f_flux, id_cols = "turfID", cols_keep = c(
        "temp_soil", "f_start"), type_a = "NEE", type_b = "ER", diff_name = "GPP")
    Condition
      Warning in `flux_diff()`:
      
       type_a or type_b missing for measurement turfID: 156 AN2C 156
       type_a or type_b missing for measurement turfID: 29 WN3C 106
    Output
      # A tibble: 8 x 5
        type  f_flux temp_soil f_start             turfID      
        <chr>  <dbl>     <dbl> <dttm>              <fct>       
      1 ER      95.6      10.8 2022-07-28 23:43:35 156 AN2C 156
      2 ER      18.6      10.7 2022-07-28 23:52:10 74 WN2C 155 
      3 GPP     33.8      10.7 2022-07-28 23:47:22 74 WN2C 155 
      4 NEE     52.4      10.7 2022-07-28 23:47:22 74 WN2C 155 
      5 ER      89.9      10.6 2022-07-29 00:03:10 109 AN3C 109
      6 GPP    -20.5      10.8 2022-07-28 23:59:32 109 AN3C 109
      7 NEE     69.4      10.8 2022-07-28 23:59:32 109 AN3C 109
      8 NEE     26.2      12.2 2022-07-29 00:06:35 29 WN3C 106 

# without f_datetime

    Code
      flux_diff(co2_fluxes, type, f_flux, id_cols = "turfID", cols_keep = c(
        "temp_soil"), type_a = "NEE", type_b = "ER", diff_name = "GPP")
    Condition
      Warning in `flux_diff()`:
      
       type_a or type_b missing for measurement turfID: 156 AN2C 156
       type_a or type_b missing for measurement turfID: 29 WN3C 106
    Output
      # A tibble: 8 x 4
        type  f_flux temp_soil turfID      
        <chr>  <dbl>     <dbl> <fct>       
      1 ER      95.6      10.8 156 AN2C 156
      2 ER      18.6      10.7 74 WN2C 155 
      3 GPP     33.8      10.7 74 WN2C 155 
      4 NEE     52.4      10.7 74 WN2C 155 
      5 ER      89.9      10.6 109 AN3C 109
      6 GPP    -20.5      10.8 109 AN3C 109
      7 NEE     69.4      10.8 109 AN3C 109
      8 NEE     26.2      12.2 29 WN3C 106 

# keeping more than one columns

    Code
      flux_diff(co2_fluxes, type, f_flux, id_cols = "turfID", cols_keep = c(
        "temp_soil", "temp_fahr", "f_start"), type_a = "NEE", type_b = "ER",
      diff_name = "GPP")
    Condition
      Warning in `flux_diff()`:
      
       type_a or type_b missing for measurement turfID: 156 AN2C 156
       type_a or type_b missing for measurement turfID: 29 WN3C 106
    Output
      # A tibble: 8 x 6
        type  f_flux temp_soil temp_fahr f_start             turfID      
        <chr>  <dbl>     <dbl>     <dbl> <dttm>              <fct>       
      1 ER      95.6      10.8      45.2 2022-07-28 23:43:35 156 AN2C 156
      2 ER      18.6      10.7      45.4 2022-07-28 23:52:10 74 WN2C 155 
      3 GPP     33.8      10.7      45.3 2022-07-28 23:47:22 74 WN2C 155 
      4 NEE     52.4      10.7      45.3 2022-07-28 23:47:22 74 WN2C 155 
      5 ER      89.9      10.6      45.9 2022-07-29 00:03:10 109 AN3C 109
      6 GPP    -20.5      10.8      46.0 2022-07-28 23:59:32 109 AN3C 109
      7 NEE     69.4      10.8      46.0 2022-07-28 23:59:32 109 AN3C 109
      8 NEE     26.2      12.2      45.9 2022-07-29 00:06:35 29 WN3C 106 

# GPP calculation works with several id cols

    Code
      flux_diff(fluxes, type, flux, id_cols = c("turfid", "campaign"), cols_keep = c(
        "datetime"), type_a = "NEE", type_b = "ER", diff_name = "GPP")
    Output
      # A tibble: 9 x 5
        type   flux datetime            turfid campaign
        <chr> <dbl> <chr>               <chr>     <dbl>
      1 ER        5 2024-02-11 10:00:10 A             1
      2 GPP      -2 2024-02-11 10:00:00 A             1
      3 NEE       3 2024-02-11 10:00:00 A             1
      4 ER        7 2024-02-11 10:00:30 A             2
      5 GPP      -5 2024-02-11 10:00:20 A             2
      6 NEE       2 2024-02-11 10:00:20 A             2
      7 ER       11 2024-02-11 10:00:50 B             2
      8 GPP      -2 2024-02-11 10:00:40 B             2
      9 NEE       9 2024-02-11 10:00:40 B             2

# missing NEE and several id cols

    Code
      flux_diff(fluxes, type, flux, id_cols = c("turfid", "campaign"), type_a = "NEE",
      type_b = "ER", diff_name = "GPP", cols_keep = c("datetime"))
    Condition
      Warning in `flux_diff()`:
      
       type_a or type_b missing for measurement turfid: C, campaign: 3
       type_a or type_b missing for measurement turfid: D, campaign: 4
       type_a or type_b missing for measurement turfid: A, campaign: 4
    Output
      # A tibble: 12 x 5
         type   flux datetime            turfid campaign
         <chr> <dbl> <chr>               <chr>     <dbl>
       1 ER        5 2024-02-11 10:00:10 A             1
       2 GPP      -2 2024-02-11 10:00:00 A             1
       3 NEE       3 2024-02-11 10:00:00 A             1
       4 ER        7 2024-02-11 10:00:30 A             2
       5 GPP      -5 2024-02-11 10:00:20 A             2
       6 NEE       2 2024-02-11 10:00:20 A             2
       7 ER       11 2024-02-11 10:00:50 B             3
       8 GPP      -2 2024-02-11 10:00:40 B             3
       9 NEE       9 2024-02-11 10:00:40 B             3
      10 ER       10 2024-02-11 10:01:00 C             3
      11 ER       13 2024-02-11 10:01:10 D             4
      12 ER        8 2024-02-11 10:01:20 A             4

# option to keep all the cols

    Code
      select(flux_diff(test_df, type, id_cols = "turfID", cols_keep = "all", type_a = "NEE",
        type_b = "ER", diff_name = "GPP"), !c(f_start, PAR, type, f_flux))
    Condition
      Warning in `flux_diff()`:
      
       type_a or type_b missing for measurement turfID: 156 AN2C 156
       type_a or type_b missing for measurement turfID: 29 WN3C 106
    Output
      # A tibble: 8 x 8
        f_fluxid f_slope_tz f_temp_air_ave temp_soil temp_fahr temp_kelvin treatment
        <fct>         <dbl>          <dbl>     <dbl>     <dbl>       <dbl> <chr>    
      1 1             1.56            7.31      10.8      45.2        280. A        
      2 3             0.303           7.46      10.7      45.4        281. A        
      3 2             0.853           7.38      10.7      45.3        281. A        
      4 2             0.853           7.38      10.7      45.3        281. A        
      5 5             1.46            7.71      10.6      45.9        281. C        
      6 4             1.13            7.77      10.8      46.0        281. B        
      7 4             1.13            7.77      10.8      46.0        281. B        
      8 6             0.426           7.75      12.2      45.9        281. C        
      # i 1 more variable: turfID <fct>

# cols keep takes values from NEE

    Code
      select(flux_diff(test_df, type, id_cols = "turfID", cols_keep = "all", type_a = "NEE",
        type_b = "ER", diff_name = "GPP"), turfID, type, test_keep)
    Condition
      Warning in `flux_diff()`:
      
       type_a or type_b missing for measurement turfID: 156 AN2C 156
       type_a or type_b missing for measurement turfID: 29 WN3C 106
    Output
      # A tibble: 8 x 3
        turfID       type  test_keep
        <fct>        <chr> <chr>    
      1 156 AN2C 156 ER    ER_val   
      2 74 WN2C 155  ER    ER_val   
      3 74 WN2C 155  GPP   <NA>     
      4 74 WN2C 155  NEE   <NA>     
      5 109 AN3C 109 ER    ER_val   
      6 109 AN3C 109 GPP   NEE_val  
      7 109 AN3C 109 NEE   NEE_val  
      8 29 WN3C 106  NEE   NEE_val  

# GPP calculation works with several id cols, and extra fluxes

    Code
      flux_diff(fluxes, type, flux, id_cols = c("turfid", "campaign"), type_a = "NEE",
      type_b = "ER", diff_name = "GPP", cols_keep = c("datetime"))
    Output
      # A tibble: 8 x 5
        type   flux datetime            turfid campaign
        <chr> <dbl> <chr>               <chr>     <dbl>
      1 ER        5 2024-02-11 10:00:10 A             1
      2 GPP      -2 2024-02-11 10:00:00 A             1
      3 NEE       3 2024-02-11 10:00:00 A             1
      4 ER        7 2024-02-11 10:00:30 A             2
      5 GPP      -5 2024-02-11 10:00:20 A             2
      6 NEE       2 2024-02-11 10:00:20 A             2
      7 soilR     3 2024-02-12 08:00:10 B             3
      8 soilR     6 2024-02-12 09:00:15 A             4

# type named differently

    Code
      suppressWarnings(flux_diff(test, flux_type, f_flux, id_cols = "turfID",
        cols_keep = c("temp_soil", "f_start"), type_a = "NEE", type_b = "ER",
        diff_name = "GPP"))
    Output
      # A tibble: 8 x 5
        flux_type f_flux temp_soil f_start             turfID      
        <chr>      <dbl>     <dbl> <dttm>              <fct>       
      1 ER          95.6      10.8 2022-07-28 23:43:35 156 AN2C 156
      2 ER          18.6      10.7 2022-07-28 23:52:10 74 WN2C 155 
      3 GPP         33.8      10.7 2022-07-28 23:47:22 74 WN2C 155 
      4 NEE         52.4      10.7 2022-07-28 23:47:22 74 WN2C 155 
      5 ER          89.9      10.6 2022-07-29 00:03:10 109 AN3C 109
      6 GPP        -20.5      10.8 2022-07-28 23:59:32 109 AN3C 109
      7 NEE         69.4      10.8 2022-07-28 23:59:32 109 AN3C 109
      8 NEE         26.2      12.2 2022-07-29 00:06:35 29 WN3C 106 

