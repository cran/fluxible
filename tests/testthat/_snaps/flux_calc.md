# averaging works

    Code
      output
    Output
      # A tibble: 6 x 12
        f_fluxID   PAR temp_soil f_slope_calc chamber_volume tube_volume atm_pressure
           <dbl> <dbl>     <dbl>        <dbl>          <dbl>       <dbl>        <dbl>
      1        1  1.95      10.8        1.56            24.5       0.075            1
      2        2  2.11      10.7        0.853           24.5       0.075            1
      3        3  2.04      10.7        0.303           24.5       0.075            1
      4        4  1.84      10.8        1.13            24.5       0.075            1
      5        5  1.66      10.6        1.46            24.5       0.075            1
      6        6  1.78      12.2        0.426           24.5       0.075            1
      # i 5 more variables: temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>,
      #   flux <dbl>, model <chr>

# keeping works

    Code
      flux_calc(slopes0, slope_col = "f_slope", cols_keep = c("turfID", "type",
        "f_start"))
    Message
      Averaging air temperature for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Calculating fluxes...
    Output
      # A tibble: 6 x 13
        turfID       type  f_start             f_fluxID f_slope_calc chamber_volume
        <chr>        <chr> <dttm>                 <dbl>        <dbl>          <dbl>
      1 156 AN2C 156 ER    2022-07-28 23:43:35        1        1.56            24.5
      2 74 WN2C 155  NEE   2022-07-28 23:47:22        2        0.853           24.5
      3 74 WN2C 155  ER    2022-07-28 23:52:10        3        0.303           24.5
      4 109 AN3C 109 NEE   2022-07-28 23:59:32        4        1.13            24.5
      5 109 AN3C 109 ER    2022-07-29 00:03:10        5        1.46            24.5
      6 29 WN3C 106  NEE   2022-07-29 00:06:35        6        0.426           24.5
      # i 7 more variables: tube_volume <dbl>, atm_pressure <dbl>,
      #   temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>, flux <dbl>,
      #   model <chr>

# keeping and averaging work together

    Code
      flux_calc(slopes0, slope_col = "f_slope", cols_keep = c("turfID", "type",
        "f_start"), cols_ave = c("PAR", "temp_soil"))
    Message
      Averaging air temperature for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Creating a df with the columns from 'cols_ave' argument...
      Calculating fluxes...
    Output
      # A tibble: 6 x 15
        f_fluxID   PAR temp_soil turfID       type  f_start             f_slope_calc
           <dbl> <dbl>     <dbl> <chr>        <chr> <dttm>                     <dbl>
      1        1  1.95      10.8 156 AN2C 156 ER    2022-07-28 23:43:35        1.56 
      2        2  2.11      10.7 74 WN2C 155  NEE   2022-07-28 23:47:22        0.853
      3        3  2.04      10.7 74 WN2C 155  ER    2022-07-28 23:52:10        0.303
      4        4  1.84      10.8 109 AN3C 109 NEE   2022-07-28 23:59:32        1.13 
      5        5  1.66      10.6 109 AN3C 109 ER    2022-07-29 00:03:10        1.46 
      6        6  1.78      12.2 29 WN3C 106  NEE   2022-07-29 00:06:35        0.426
      # i 8 more variables: chamber_volume <dbl>, tube_volume <dbl>,
      #   atm_pressure <dbl>, temp_air_ave <dbl>, datetime <dttm>,
      #   volume_setup <dbl>, flux <dbl>, model <chr>

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope", temp_air_col = "temp_fahr",
        temp_air_unit = "fahrenheit")
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
    Output
      # A tibble: 6 x 10
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        1.56            24.5       0.075            1         45.2
      2        2        0.853           24.5       0.075            1         45.3
      3        3        0.303           24.5       0.075            1         45.4
      4        4        1.13            24.5       0.075            1         46.0
      5        5        1.46            24.5       0.075            1         45.9
      6        6        0.426           24.5       0.075            1         45.9
      # i 4 more variables: datetime <dttm>, volume_setup <dbl>, flux <dbl>,
      #   model <chr>

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope", temp_air_col = "temp_kelvin",
        temp_air_unit = "kelvin")
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
    Output
      # A tibble: 6 x 10
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        1.56            24.5       0.075            1         280.
      2        2        0.853           24.5       0.075            1         281.
      3        3        0.303           24.5       0.075            1         281.
      4        4        1.13            24.5       0.075            1         281.
      5        5        1.46            24.5       0.075            1         281.
      6        6        0.426           24.5       0.075            1         281.
      # i 4 more variables: datetime <dttm>, volume_setup <dbl>, flux <dbl>,
      #   model <chr>

# calculating fluxes on dataset with cuts

    Code
      flux_calc(slopes30_flag, slope_col = "f_slope_corr", cut_col = "f_cut",
        keep_arg = "keep")
    Message
      Cutting data according to 'keep_arg'...
      Averaging air temperature for each flux...
      Calculating fluxes...
    Output
      # A tibble: 6 x 10
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        0.775           24.5       0.075            1         7.29
      2        2        0.504           24.5       0.075            1         7.37
      3        3        0.337           24.5       0.075            1         7.45
      4        4        0.676           24.5       0.075            1         7.77
      5        5        1.12            24.5       0.075            1         7.70
      6        6        0.425           24.5       0.075            1         7.74
      # i 4 more variables: datetime <dttm>, volume_setup <dbl>, flux <dbl>,
      #   model <chr>

# volume can be a variable instead of a constant

    Code
      flux_calc(slopes0_vol, slope_col = "f_slope_tz", chamber_volume = "volume")
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
    Output
      # A tibble: 6 x 9
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        1.56              18       0.075            1         7.31
      2        2        0.853             28       0.075            1         7.38
      3        3        0.303             20       0.075            1         7.46
      4        4        1.13              24       0.075            1         7.77
      5        5        1.46               4       0.075            1         7.71
      6        6        0.426             35       0.075            1         7.75
      # i 3 more variables: datetime <dttm>, volume_setup <dbl>, flux <dbl>

# volume can be a variable instead of a constant (volume)

    Code
      select(flux_calc(slopes0_vol_tube, slope_col = "f_slope_tz", chamber_volume = "volume",
        tube_volume = "tube_vol"), !c(chamber_volume, tube_volume))
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
    Output
      # A tibble: 6 x 7
        f_fluxID f_slope_calc atm_pressure temp_air_ave datetime           
           <dbl>        <dbl>        <dbl>        <dbl> <dttm>             
      1        1        1.56             1         7.31 2022-07-28 23:43:35
      2        2        0.853            1         7.38 2022-07-28 23:47:22
      3        3        0.303            1         7.46 2022-07-28 23:52:10
      4        4        1.13             1         7.77 2022-07-28 23:59:32
      5        5        1.46             1         7.71 2022-07-29 00:03:10
      6        6        0.426            1         7.75 2022-07-29 00:06:35
      # i 2 more variables: volume_setup <dbl>, flux <dbl>

# Fluxible workflow works from start to finish

    Code
      str(fluxes_test)
    Output
      tibble [6 x 10] (S3: tbl_df/tbl/data.frame)
       $ f_fluxID      : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6
       $ f_slope_calc  : num [1:6] 1.555 0.853 0.303 1.13 1.463 ...
       $ chamber_volume: num [1:6] 24.5 24.5 24.5 24.5 24.5 24.5
       $ tube_volume   : num [1:6] 0.075 0.075 0.075 0.075 0.075 0.075
       $ atm_pressure  : num [1:6] 1 1 1 1 1 1
       $ temp_air_ave  : num [1:6] 7.31 7.38 7.46 7.77 7.71 ...
       $ datetime      : POSIXct[1:6], format: "2022-07-28 23:43:35" "2022-07-28 23:47:22" ...
       $ volume_setup  : num [1:6] 24.6 24.6 24.6 24.6 24.6 ...
       $ flux          : num [1:6] 95.6 52.4 18.6 69.4 89.9 ...
       $ model         : chr [1:6] "exponential" "exponential" "exponential" "exponential" ...

