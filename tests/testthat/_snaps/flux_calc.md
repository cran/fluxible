# averaging works

    Code
      output
    Output
      # A tibble: 6 x 6
        f_fluxid f_temp_air_ave datetime            f_flux   PAR temp_soil
        <fct>             <dbl> <dttm>               <dbl> <dbl>     <dbl>
      1 1                  7.31 2022-07-28 23:43:35   95.6  1.95      10.8
      2 2                  7.38 2022-07-28 23:47:22   52.4  2.11      10.7
      3 3                  7.46 2022-07-28 23:52:10   18.6  2.04      10.7
      4 4                  7.77 2022-07-28 23:59:32   69.4  1.84      10.8
      5 5                  7.71 2022-07-29 00:03:10   89.9  1.66      10.6
      6 6                  7.75 2022-07-29 00:06:35   26.2  1.78      12.2

# keeping works

    Code
      dplyr::select(flux_calc(slopes0, f_slope, datetime, temp_air, conc_unit = "ppm",
        flux_unit = "mmol", cols_keep = c("turfID", "type", "f_start"),
        chamber_volume = 24.5, tube_volume = 0.075, atm_pressure = 1, plot_area = 0.0625,
        cut = FALSE), f_fluxid, f_flux, turfID, type, f_start, f_slope)
    Message
      Averaging air temperature for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 6
        f_fluxid f_flux turfID       type  f_start             f_slope
        <fct>     <dbl> <fct>        <fct> <dttm>                <dbl>
      1 1          95.6 156 AN2C 156 ER    2022-07-28 23:43:35   1.56 
      2 2          52.4 74 WN2C 155  NEE   2022-07-28 23:47:22   0.853
      3 3          18.6 74 WN2C 155  ER    2022-07-28 23:52:10   0.303
      4 4          69.4 109 AN3C 109 NEE   2022-07-28 23:59:32   1.13 
      5 5          89.9 109 AN3C 109 ER    2022-07-29 00:03:10   1.46 
      6 6          26.2 29 WN3C 106  NEE   2022-07-29 00:06:35   0.426

# keeping and averaging work together

    Code
      dplyr::select(flux_calc(slopes0, f_slope, datetime, temp_air, conc_unit = "ppm",
        flux_unit = "mmol", cols_keep = c("turfID", "type", "f_start"), cols_ave = c(
          "PAR", "temp_soil"), chamber_volume = 24.5, tube_volume = 0.075,
        atm_pressure = 1, plot_area = 0.0625, cut = FALSE), f_fluxid, f_flux, turfID,
      type, f_start, PAR, temp_soil)
    Message
      Averaging air temperature for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Creating a df with the columns from 'cols_ave' argument...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 7
        f_fluxid f_flux turfID       type  f_start               PAR temp_soil
        <fct>     <dbl> <fct>        <fct> <dttm>              <dbl>     <dbl>
      1 1          95.6 156 AN2C 156 ER    2022-07-28 23:43:35  1.95      10.8
      2 2          52.4 74 WN2C 155  NEE   2022-07-28 23:47:22  2.11      10.7
      3 3          18.6 74 WN2C 155  ER    2022-07-28 23:52:10  2.04      10.7
      4 4          69.4 109 AN3C 109 NEE   2022-07-28 23:59:32  1.84      10.8
      5 5          89.9 109 AN3C 109 ER    2022-07-29 00:03:10  1.66      10.6
      6 6          26.2 29 WN3C 106  NEE   2022-07-29 00:06:35  1.78      12.2

# fahrenheit conversion works

    Code
      dplyr::select(flux_calc(slopes0_temp, f_slope, datetime, temp_fahr, conc_unit = "ppm",
        flux_unit = "mmol", temp_air_unit = "fahrenheit", chamber_volume = 24.5,
        tube_volume = 0.075, atm_pressure = 1, plot_area = 0.0625, cut = FALSE),
      f_fluxid, f_temp_air_ave, datetime, f_flux, f_volume_setup)
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 5
        f_fluxid f_temp_air_ave datetime            f_flux f_volume_setup
        <fct>             <dbl> <dttm>               <dbl>          <dbl>
      1 1                  45.2 2022-07-28 23:43:35   95.6           24.6
      2 2                  45.3 2022-07-28 23:47:22   52.4           24.6
      3 3                  45.4 2022-07-28 23:52:10   18.6           24.6
      4 4                  46.0 2022-07-28 23:59:32   69.4           24.6
      5 5                  45.9 2022-07-29 00:03:10   89.9           24.6
      6 6                  45.9 2022-07-29 00:06:35   26.2           24.6

# kelvin conversion works

    Code
      dplyr::select(flux_calc(slopes0_temp, f_slope, datetime, temp_kelvin,
        conc_unit = "ppm", flux_unit = "mmol", temp_air_unit = "kelvin",
        chamber_volume = 24.5, tube_volume = 0.075, atm_pressure = 1, plot_area = 0.0625,
        cut = FALSE), f_fluxid, f_temp_air_ave, datetime, f_flux, f_volume_setup)
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 5
        f_fluxid f_temp_air_ave datetime            f_flux f_volume_setup
        <fct>             <dbl> <dttm>               <dbl>          <dbl>
      1 1                  280. 2022-07-28 23:43:35   95.6           24.6
      2 2                  281. 2022-07-28 23:47:22   52.4           24.6
      3 3                  281. 2022-07-28 23:52:10   18.6           24.6
      4 4                  281. 2022-07-28 23:59:32   69.4           24.6
      5 5                  281. 2022-07-29 00:03:10   89.9           24.6
      6 6                  281. 2022-07-29 00:06:35   26.2           24.6

# calculating fluxes on dataset with cuts

    Code
      dplyr::select(flux_calc(slopes30_flag, f_slope_corr, datetime, temp_air,
        conc_unit = "ppm", flux_unit = "mmol", keep_arg = "keep", chamber_volume = 24.5,
        tube_volume = 0.075, atm_pressure = 1, plot_area = 0.0625), f_fluxid,
      f_temp_air_ave, datetime, f_flux, f_volume_setup)
    Message
      Cutting data according to 'keep_arg'...
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 5
        f_fluxid f_temp_air_ave datetime            f_flux f_volume_setup
        <fct>             <dbl> <dttm>               <dbl>          <dbl>
      1 1                  7.29 2022-07-28 23:43:35   47.7           24.6
      2 2                  7.37 2022-07-28 23:47:22   31.0           24.6
      3 3                  7.45 2022-07-28 23:52:10   20.7           24.6
      4 4                  7.77 2022-07-28 23:59:32   41.5           24.6
      5 5                  7.70 2022-07-29 00:03:10    0             24.6
      6 6                  7.74 2022-07-29 00:06:35   26.1           24.6

# volume can be a variable instead of a constant

    Code
      dplyr::select(flux_calc(slopes0_vol, f_slope, datetime, temp_air, volume,
        conc_unit = "ppm", flux_unit = "mmol", tube_volume = 0.075, atm_pressure = 1,
        plot_area = 0.0625), f_fluxid, f_temp_air_ave, datetime, f_flux,
      f_volume_setup)
    Message
      Cutting data according to 'keep_arg'...
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 5
        f_fluxid f_temp_air_ave datetime            f_flux f_volume_setup
        <fct>             <dbl> <dttm>               <dbl>          <dbl>
      1 1                  7.31 2022-07-28 23:43:35   70.4          18.1 
      2 2                  7.38 2022-07-28 23:47:22   59.9          28.1 
      3 3                  7.46 2022-07-28 23:52:10   15.2          20.1 
      4 4                  7.77 2022-07-28 23:59:32   68.0          24.1 
      5 5                  7.71 2022-07-29 00:03:10   14.9           4.08
      6 6                  7.75 2022-07-29 00:06:35   37.3          35.1 

# Fluxible workflow works from start to finish

    Code
      str(fluxes_test)
    Output
      tibble [6 x 7] (S3: tbl_df/tbl/data.frame)
       $ f_fluxid      : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6
       $ f_slope_corr  : num [1:6] 0.785 0.503 0.344 0.693 0 ...
       $ f_temp_air_ave: num [1:6] 7.28 7.37 7.45 7.77 7.69 ...
       $ datetime      : POSIXct[1:6], format: "2022-07-28 23:43:35" "2022-07-28 23:47:22" ...
       $ f_volume_setup: num [1:6] 24.6 24.6 24.6 24.6 24.6 ...
       $ f_flux        : num [1:6] 48.3 30.9 21.1 42.5 0 ...
       $ f_model       : chr [1:6] "exp_zhao18" "exp_zhao18" "exp_zhao18" "exp_zhao18" ...

# Stupeflux works with slope_correction = FALSE

    Code
      stupeflux(raw_conc = co2_df_short, field_record = record_short, datetime_col = datetime,
        start_col = start, conc_col = conc, startcrop = 10, measurement_length = 180,
        fit_type = "exp_zhao18", temp_air_col = temp_air, conc_unit = "ppm",
        flux_unit = "mmol", chamber_volume = 24.5, tube_volume = 0.075, atm_pressure = 1,
        plot_area = 0.0625, slope_correction = FALSE)
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
      
       Total number of measurements: 6
      
       ok 	 5 	 83 %
       zero 	 1 	 17 %
       discard 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
      Cutting data according to 'keep_arg'...
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 7
        f_fluxid f_slope f_temp_air_ave datetime            f_volume_setup f_flux
        <fct>      <dbl>          <dbl> <dttm>                       <dbl>  <dbl>
      1 1          0.785           7.28 2022-07-28 23:43:35           24.6   48.3
      2 2          0.503           7.37 2022-07-28 23:47:22           24.6   30.9
      3 3          0.344           7.45 2022-07-28 23:52:10           24.6   21.1
      4 4          0.693           7.77 2022-07-28 23:59:32           24.6   42.5
      5 5          1.20            7.69 2022-07-29 00:03:10           24.6   74.0
      6 6          0.433           7.74 2022-07-29 00:06:35           24.6   26.6
      # i 1 more variable: f_model <chr>

# Fluxible workflow works with kappamax

    Code
      fluxes_test
    Output
      # A tibble: 6 x 5
        f_model f_temp_air_ave datetime            f_volume_setup f_flux
        <chr>            <dbl> <dttm>                       <dbl>  <dbl>
      1 exp_hm            7.29 2022-07-28 23:43:35           24.6   44.7
      2 exp_hm            7.37 2022-07-28 23:47:22           24.6   25.7
      3 exp_hm            7.45 2022-07-28 23:52:10           24.6   23.0
      4 exp_hm            7.77 2022-07-28 23:59:32           24.6   44.5
      5 linear            7.70 2022-07-29 00:03:10           24.6    0  
      6 exp_hm            7.74 2022-07-29 00:06:35           24.6   24.8

# Working with two gases

    Code
      str(fluxes_twogases)
    Output
      tibble [12 x 8] (S3: tbl_df/tbl/data.frame)
       $ f_quality_flag: chr [1:12] "ok" "ok" "ok" "ok" ...
       $ f_fluxid      : Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
       $ f_temp_air_ave: num [1:12] 13.4 16.5 17.1 14.4 15 ...
       $ datetime      : POSIXct[1:12], format: "2024-06-18 10:04:37" "2024-06-18 11:12:52" ...
       $ f_volume_setup: num [1:12] 6.31 6.31 6.31 6.31 6.31 6.31 6.31 6.31 6.31 6.31 ...
       $ flux_co2      : num [1:12] 0.08292 0.38505 0.43518 0.00108 0.06371 ...
       $ f_model       : chr [1:12] "exp_zhao18" "exp_zhao18" "exp_zhao18" "exp_zhao18" ...
       $ flux_ch4      : num [1:12] -0.04873 0.01165 0 -0.00649 0 ...
       - attr(*, "fit_type")= chr "exp_zhao18"

# sum and median works

    Code
      output
    Output
      # A tibble: 6 x 6
        f_fluxid f_temp_air_ave datetime            f_flux   PAR temp_soil
        <fct>             <dbl> <dttm>               <dbl> <dbl>     <dbl>
      1 1                  7.31 2022-07-28 23:43:35   95.6  40.9      10.8
      2 2                  7.38 2022-07-28 23:47:22   52.4  44.2      10.7
      3 3                  7.46 2022-07-28 23:52:10   18.6  42.7      10.7
      4 4                  7.77 2022-07-28 23:59:32   69.4  38.6      10.8
      5 5                  7.71 2022-07-29 00:03:10   89.9  33.3      10.6
      6 6                  7.75 2022-07-29 00:06:35   26.2  37.4      12.2

