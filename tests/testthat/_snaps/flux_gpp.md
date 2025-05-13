# GPP calculation

    Code
      flux_gpp(co2_fluxes, type, f_start, f_flux, id_cols = "turfID", cols_keep = c(
        "temp_soil"))
    Condition
      Warning in `flux_gpp()`:
      
       NEE missing for measurement turfID: 156 AN2C 156
    Output
      # A tibble: 9 x 5
        f_start             type  f_flux temp_soil turfID      
        <dttm>              <chr>  <dbl>     <dbl> <fct>       
      1 2022-07-28 23:43:35 ER      95.6      10.8 156 AN2C 156
      2 2022-07-28 23:47:22 GPP     33.8      10.7 74 WN2C 155 
      3 2022-07-28 23:47:22 NEE     52.4      10.7 74 WN2C 155 
      4 2022-07-28 23:52:10 ER      18.6      10.7 74 WN2C 155 
      5 2022-07-28 23:59:32 GPP    -20.5      10.8 109 AN3C 109
      6 2022-07-28 23:59:32 NEE     69.4      10.8 109 AN3C 109
      7 2022-07-29 00:03:10 ER      89.9      10.6 109 AN3C 109
      8 2022-07-29 00:06:35 GPP     NA        12.2 29 WN3C 106 
      9 2022-07-29 00:06:35 NEE     26.2      12.2 29 WN3C 106 

# keeping more than one columns

    Code
      flux_gpp(co2_fluxes, type, f_start, f_flux, id_cols = "turfID", cols_keep = c(
        "temp_soil", "temp_fahr"))
    Condition
      Warning in `flux_gpp()`:
      
       NEE missing for measurement turfID: 156 AN2C 156
    Output
      # A tibble: 9 x 6
        f_start             type  f_flux temp_soil temp_fahr turfID      
        <dttm>              <chr>  <dbl>     <dbl>     <dbl> <fct>       
      1 2022-07-28 23:43:35 ER      95.6      10.8      45.2 156 AN2C 156
      2 2022-07-28 23:47:22 GPP     33.8      10.7      45.3 74 WN2C 155 
      3 2022-07-28 23:47:22 NEE     52.4      10.7      45.3 74 WN2C 155 
      4 2022-07-28 23:52:10 ER      18.6      10.7      45.4 74 WN2C 155 
      5 2022-07-28 23:59:32 GPP    -20.5      10.8      46.0 109 AN3C 109
      6 2022-07-28 23:59:32 NEE     69.4      10.8      46.0 109 AN3C 109
      7 2022-07-29 00:03:10 ER      89.9      10.6      45.9 109 AN3C 109
      8 2022-07-29 00:06:35 GPP     NA        12.2      45.9 29 WN3C 106 
      9 2022-07-29 00:06:35 NEE     26.2      12.2      45.9 29 WN3C 106 

# GPP calculation works with several id cols

    Code
      flux_gpp(fluxes, type, datetime, flux, id_cols = c("turfid", "campaign"))
    Output
      # A tibble: 9 x 5
        datetime            type   flux turfid campaign
        <chr>               <chr> <dbl> <chr>     <dbl>
      1 2024-02-11 10:00:00 GPP      -2 A             1
      2 2024-02-11 10:00:00 NEE       3 A             1
      3 2024-02-11 10:00:10 ER        5 A             1
      4 2024-02-11 10:00:20 GPP      -5 A             2
      5 2024-02-11 10:00:20 NEE       2 A             2
      6 2024-02-11 10:00:30 ER        7 A             2
      7 2024-02-11 10:00:40 GPP      -2 B             3
      8 2024-02-11 10:00:40 NEE       9 B             3
      9 2024-02-11 10:00:50 ER       11 B             3

# missing NEE and several id cols

    Code
      flux_gpp(fluxes, type, datetime, flux, id_cols = c("turfid", "campaign"))
    Condition
      Warning in `flux_gpp()`:
      
       NEE missing for measurement turfid: C, campaign: 3
       NEE missing for measurement turfid: D, campaign: 4
       NEE missing for measurement turfid: A, campaign: 4
    Output
      # A tibble: 12 x 5
         datetime            type   flux turfid campaign
         <chr>               <chr> <dbl> <chr>     <dbl>
       1 2024-02-11 10:00:00 GPP      -2 A             1
       2 2024-02-11 10:00:00 NEE       3 A             1
       3 2024-02-11 10:00:10 ER        5 A             1
       4 2024-02-11 10:00:20 GPP      -5 A             2
       5 2024-02-11 10:00:20 NEE       2 A             2
       6 2024-02-11 10:00:30 ER        7 A             2
       7 2024-02-11 10:00:40 GPP      -2 B             3
       8 2024-02-11 10:00:40 NEE       9 B             3
       9 2024-02-11 10:00:50 ER       11 B             3
      10 2024-02-11 10:01:00 ER       10 C             3
      11 2024-02-11 10:01:10 ER       13 D             4
      12 2024-02-11 10:01:20 ER        8 A             4

# option to keep all the cols

    Code
      select(flux_gpp(test_df, type, f_start, id_cols = "turfID", cols_keep = "all"),
      !c(f_start, PAR, type, f_flux))
    Condition
      Warning in `flux_gpp()`:
      
       NEE missing for measurement turfID: 156 AN2C 156
    Output
      # A tibble: 9 x 8
        f_fluxid f_slope_tz f_temp_air_ave temp_soil temp_fahr temp_kelvin treatment
        <fct>         <dbl>          <dbl>     <dbl>     <dbl>       <dbl> <chr>    
      1 1             1.56            7.31      10.8      45.2        280. A        
      2 2             0.853           7.38      10.7      45.3        281. A        
      3 2             0.853           7.38      10.7      45.3        281. A        
      4 3             0.303           7.46      10.7      45.4        281. A        
      5 4             1.13            7.77      10.8      46.0        281. B        
      6 4             1.13            7.77      10.8      46.0        281. B        
      7 5             1.46            7.71      10.6      45.9        281. C        
      8 6             0.426           7.75      12.2      45.9        281. C        
      9 6             0.426           7.75      12.2      45.9        281. C        
      # i 1 more variable: turfID <fct>

# cols keep takes values from NEE

    Code
      select(flux_gpp(test_df, type, f_start, id_cols = "turfID", cols_keep = "all"),
      turfID, type, test_keep)
    Condition
      Warning in `flux_gpp()`:
      
       NEE missing for measurement turfID: 156 AN2C 156
    Output
      # A tibble: 9 x 3
        turfID       type  test_keep
        <fct>        <chr> <chr>    
      1 156 AN2C 156 ER    ER_val   
      2 74 WN2C 155  GPP   <NA>     
      3 74 WN2C 155  NEE   <NA>     
      4 74 WN2C 155  ER    ER_val   
      5 109 AN3C 109 GPP   NEE_val  
      6 109 AN3C 109 NEE   NEE_val  
      7 109 AN3C 109 ER    ER_val   
      8 29 WN3C 106  GPP   NEE_val  
      9 29 WN3C 106  NEE   NEE_val  

# GPP calculation works with several id cols, and extra fluxes

    Code
      flux_gpp(fluxes, type, datetime, flux, id_cols = c("turfid", "campaign"))
    Output
      # A tibble: 8 x 5
        datetime            type   flux turfid campaign
        <chr>               <chr> <dbl> <chr>     <dbl>
      1 2024-02-11 10:00:00 GPP      -2 A             1
      2 2024-02-11 10:00:00 NEE       3 A             1
      3 2024-02-11 10:00:10 ER        5 A             1
      4 2024-02-11 10:00:20 GPP      -5 A             2
      5 2024-02-11 10:00:20 NEE       2 A             2
      6 2024-02-11 10:00:30 ER        7 A             2
      7 2024-02-12 08:00:10 soilR     3 B             3
      8 2024-02-12 09:00:15 soilR     6 A             4

