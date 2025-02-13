# quadratic fit works

    Code
      flux_fitting(co2_conc, conc, datetime, f_start, f_end, f_fluxid, fit_type = "quadratic",
        t_zero = 10, end_cut = 30)
    Output
      # A tibble: 1,251 x 24
         datetime            temp_air temp_soil  conc   PAR turfID       type 
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
      # i 17 more variables: f_start <dttm>, f_end <dttm>, f_fluxid <fct>,
      #   f_ratio <dbl>, f_flag_match <chr>, f_time <dbl>, f_cut <fct>,
      #   f_param1 <dbl>, f_param2 <dbl>, f_rsquared <dbl>, f_adj_rsquared <dbl>,
      #   f_intercept <dbl>, f_pvalue <dbl>, f_slope <dbl>, f_fit <dbl>,
      #   f_fit_slope <dbl>, f_start_z <dttm>

