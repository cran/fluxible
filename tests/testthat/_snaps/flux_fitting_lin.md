# renaming works

    Code
      flux_fitting(co2_conc_names, co2, date_time, f_start, finish, fit_type = "lin")
    Condition
      Warning in `flux_fitting_lin()`:
      
       fluxID 5 : slope was estimated on 205 points out of 210 seconds
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Output
      # A tibble: 1,251 x 20
         date_time           temp_air temp_soil   co2   PAR turfID       type 
         <dttm>                 <dbl>     <dbl> <dbl> <dbl> <fct>        <fct>
       1 2022-07-28 23:43:35    NA         NA    447. NA    156 AN2C 156 ER   
       2 2022-07-28 23:43:36     7.22      10.9  447.  1.68 156 AN2C 156 ER   
       3 2022-07-28 23:43:37    NA         NA    448. NA    156 AN2C 156 ER   
       4 2022-07-28 23:43:38    NA         NA    449. NA    156 AN2C 156 ER   
       5 2022-07-28 23:43:39    NA         NA    449. NA    156 AN2C 156 ER   
       6 2022-07-28 23:43:40    NA         NA    450. NA    156 AN2C 156 ER   
       7 2022-07-28 23:43:41    NA         NA    451. NA    156 AN2C 156 ER   
       8 2022-07-28 23:43:42    NA         NA    451. NA    156 AN2C 156 ER   
       9 2022-07-28 23:43:43    NA         NA    453. NA    156 AN2C 156 ER   
      10 2022-07-28 23:43:44    NA         NA    453. NA    156 AN2C 156 ER   
      # i 1,241 more rows
      # i 13 more variables: f_start <dttm>, finish <dttm>, f_fluxid <fct>,
      #   f_ratio <dbl>, f_flag_match <chr>, f_time <dbl>, f_cut <fct>,
      #   f_rsquared <dbl>, f_adj_rsquared <dbl>, f_slope <dbl>, f_intercept <dbl>,
      #   f_pvalue <dbl>, f_fit <dbl>

