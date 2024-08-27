# matching works

    Code
      flux_match(co2_df_short, record_short)
    Output
      # A tibble: 1,251 x 13
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:35    NA         NA     447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9   447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA     448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA     449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA     449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA     450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA     451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA     451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA     453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA     453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 6 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   f_n_conc <int>, f_ratio <dbl>, f_flag_match <chr>

# time_diff works

    Code
      flux_match(co2_df_short_180, record_short, time_diff = 180)
    Output
      # A tibble: 1,251 x 13
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:35    NA         NA     447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9   447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA     448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA     449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA     449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA     450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA     451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA     451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA     453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA     453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 6 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   f_n_conc <int>, f_ratio <dbl>, f_flag_match <chr>

# renaming variables works

    Code
      flux_match(co2_df_short, record_short, datetime_col = "date_time", conc_col = "CO2_conc",
        start_col = "starting")
    Output
      # A tibble: 1,251 x 13
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:35    NA         NA     447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9   447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA     448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA     449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA     449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA     450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA     451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA     451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA     453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA     453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 6 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   f_n_conc <int>, f_ratio <dbl>, f_flag_match <chr>

# flags on nb of data

    Code
      suppressWarnings(flux_match(co2_df_missing, record_short))
    Output
      # A tibble: 668 x 13
         f_datetime          temp_air temp_soil f_conc   PAR turfID       type 
         <dttm>                 <dbl>     <dbl>  <dbl> <dbl> <chr>        <chr>
       1 2022-07-28 23:43:35       NA        NA    NA     NA 156 AN2C 156 ER   
       2 2022-07-28 23:45:37       NA        NA   514.    NA 156 AN2C 156 ER   
       3 2022-07-28 23:45:38       NA        NA   513.    NA 156 AN2C 156 ER   
       4 2022-07-28 23:45:39       NA        NA   513.    NA 156 AN2C 156 ER   
       5 2022-07-28 23:45:40       NA        NA   514.    NA 156 AN2C 156 ER   
       6 2022-07-28 23:45:41       NA        NA   514.    NA 156 AN2C 156 ER   
       7 2022-07-28 23:45:42       NA        NA   515.    NA 156 AN2C 156 ER   
       8 2022-07-28 23:45:43       NA        NA   515.    NA 156 AN2C 156 ER   
       9 2022-07-28 23:45:44       NA        NA   515.    NA 156 AN2C 156 ER   
      10 2022-07-28 23:45:45       NA        NA   515.    NA 156 AN2C 156 ER   
      # i 658 more rows
      # i 6 more variables: f_start <dttm>, f_end <dttm>, f_fluxID <fct>,
      #   f_n_conc <int>, f_ratio <dbl>, f_flag_match <chr>

