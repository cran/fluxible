# works for exponential fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes0, conc), f_fluxid,
      f_quality_flag, f_RMSE, f_cor_coef, f_ratio, f_gfactor))
    Message
      
       Total number of measurements: 6
      
       discard 	 2 	 33 %
       ok 	 3 	 50 %
       zero 	 1 	 17 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
    Output
      # A tibble: 6 x 6
        f_fluxid f_quality_flag f_RMSE f_cor_coef f_ratio f_gfactor
        <fct>    <chr>           <dbl>      <dbl>   <dbl>     <dbl>
      1 1        discard         23.5       0.211   1         13.8 
      2 2        ok              14.0       0.336   1          7.74
      3 3        ok               2.51      0.949   1          2.63
      4 4        zero            15.0       0.112   1         26.2 
      5 5        discard         12.0      -0.315   0.976    -14.0 
      6 6        ok               6.19      0.640   0.981      3.65

# works for linear fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes30lin, conc), f_fluxid,
      f_quality_flag, f_pvalue, f_rsquared))
    Message
      
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
    Output
      # A tibble: 6 x 4
        f_fluxid f_quality_flag  f_pvalue f_rsquared
        <fct>    <chr>              <dbl>      <dbl>
      1 1        ok             1.23e-166    0.986  
      2 2        ok             1.43e-207    0.995  
      3 3        ok             3.06e- 96    0.913  
      4 4        ok             1.04e-108    0.937  
      5 5        zero           2.84e-  1    0.00646
      6 6        ok             1.39e-114    0.946  

# works for quadratic fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes30qua, conc), f_fluxid,
      f_quality_flag, f_pvalue, f_rsquared, f_gfactor))
    Message
      
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
    Output
      # A tibble: 6 x 5
        f_fluxid f_quality_flag  f_pvalue f_rsquared f_gfactor
        <fct>    <chr>              <dbl>      <dbl>     <dbl>
      1 1        ok             9.51e-297      1.00       1.45
      2 2        ok             1.08e-292      0.999      1.26
      3 3        ok             2.44e-173      0.989      2.11
      4 4        ok             8.52e-217      0.996      1.97
      5 5        zero           9.68e- 55      0.755    -40.5 
      6 6        ok             5.13e-191      0.993      1.86

# kappamax with HM model

    Code
      dplyr::distinct(dplyr::select(dplyr::filter(flux_quality(slopeshm, conc,
        f_pvalue = f_pvalue_lm, f_rsquared = f_rsquared_lm, kappamax = TRUE), f_cut ==
        "keep"), f_fluxid, f_quality_flag, f_slope_corr, f_model))
    Message
      
       Number of measurements with linear fit: 1
      
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
    Output
      # A tibble: 6 x 4
        f_fluxid f_quality_flag f_slope_corr f_model
        <fct>    <chr>                 <dbl> <chr>  
      1 1        ok                    0.727 exp_hm 
      2 2        ok                    0.418 exp_hm 
      3 3        ok                    0.374 exp_hm 
      4 4        ok                    0.725 exp_hm 
      5 5        zero                  0     linear 
      6 6        ok                    0.404 exp_hm 

# kappamax with zhao18 model

    Code
      dplyr::distinct(dplyr::select(dplyr::filter(flux_quality(slopesexp, conc,
        f_pvalue = f_pvalue_lm, f_rsquared = f_rsquared_lm, kappamax = TRUE), f_cut ==
        "keep"), f_fluxid, f_quality_flag, f_slope_corr, f_model))
    Message
      
       Number of measurements with linear fit: 1
      
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
    Output
      # A tibble: 6 x 4
        f_fluxid f_quality_flag f_slope_corr f_model   
        <fct>    <chr>                 <dbl> <chr>     
      1 1        ok                    0.775 exp_zhao18
      2 2        ok                    0.504 exp_zhao18
      3 3        ok                    0.337 exp_zhao18
      4 4        ok                    0.676 exp_zhao18
      5 5        zero                  0     linear    
      6 6        ok                    0.425 exp_zhao18

