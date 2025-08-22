# works for exponential fitting

    Code
      dplyr::distinct(dplyr::select(flux_quality(slopes0, conc), f_fluxid,
      f_quality_flag, f_RMSE, f_cor_coef, f_ratio, f_gfactor))
    Message
      
       Total number of measurements: 6
      
       ok 	 3 	 50 %
       discard 	 2 	 33 %
       zero 	 1 	 17 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
       no_slope 	 0 	 0 %
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
       no_slope 	 0 	 0 %
    Output
      # A tibble: 12 x 4
         f_fluxid f_quality_flag  f_pvalue f_rsquared
         <fct>    <chr>              <dbl>      <dbl>
       1 1        ok             1.23e-166    0.986  
       2 1        <NA>           1.23e-166    0.986  
       3 2        ok             1.43e-207    0.995  
       4 2        <NA>           1.43e-207    0.995  
       5 3        ok             3.06e- 96    0.913  
       6 3        <NA>           3.06e- 96    0.913  
       7 4        ok             1.04e-108    0.937  
       8 4        <NA>           1.04e-108    0.937  
       9 5        zero           2.84e-  1    0.00646
      10 5        <NA>           2.84e-  1    0.00646
      11 6        ok             1.39e-114    0.946  
      12 6        <NA>           1.39e-114    0.946  

# works for quadratic fitting

    Code
      dplyr::distinct(dplyr::mutate(dplyr::select(flux_quality(slopes30qua, conc),
      f_fluxid, f_quality_flag, f_pvalue, f_rsquared, f_gfactor), f_rsquared = round(
        f_rsquared, digits = 3)))
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
       no_slope 	 0 	 0 %
    Output
      # A tibble: 12 x 5
         f_fluxid f_quality_flag  f_pvalue f_rsquared f_gfactor
         <fct>    <chr>              <dbl>      <dbl>     <dbl>
       1 1        ok             9.51e-297      1          1.45
       2 1        <NA>           9.51e-297      1         NA   
       3 2        ok             1.08e-292      0.999      1.26
       4 2        <NA>           1.08e-292      0.999     NA   
       5 3        ok             2.44e-173      0.989      2.11
       6 3        <NA>           2.44e-173      0.989     NA   
       7 4        ok             8.52e-217      0.996      1.97
       8 4        <NA>           8.52e-217      0.996     NA   
       9 5        zero           9.68e- 55      0.755    -40.5 
      10 5        <NA>           9.68e- 55      0.755     NA   
      11 6        ok             5.13e-191      0.993      1.86
      12 6        <NA>           5.13e-191      0.993     NA   

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
       no_slope 	 0 	 0 %
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
       no_slope 	 0 	 0 %
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

# works in a pipeline

    Code
      dplyr::distinct(dplyr::select(flux_quality(flux_fitting(co2_conc, conc,
        datetime, fit_type = "exp_hm"), f_conc = conc, f_pvalue = f_pvalue_lm,
      f_rsquared = f_rsquared_lm, kappamax = TRUE), f_fluxid, f_quality_flag, f_RMSE,
      f_cor_coef, f_ratio, f_gfactor))
    Message
      Cutting measurements...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting()`:
      
       fluxID 5 : slope is NA, most likely an issue with the model optimization.
              Check your data or use a different model.
       fluxID 6 : slope was estimated on 206 points out of 210 seconds
    Message
      
       Number of measurements with linear fit: 4
      
       Total number of measurements: 6
      
       discard 	 3 	 50 %
       ok 	 2 	 33 %
       zero 	 1 	 17 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
       force_lm 	 0 	 0 %
       no_slope 	 0 	 0 %
    Output
      # A tibble: 6 x 6
        f_fluxid f_quality_flag f_RMSE f_cor_coef f_ratio f_gfactor
        <fct>    <chr>           <dbl>      <dbl>   <dbl>     <dbl>
      1 1        discard        NA         NA       1         NA   
      2 2        discard        NA         NA       1         NA   
      3 3        ok              0.550      0.949   1          3.10
      4 4        zero           NA         NA       1         NA   
      5 5        discard        NA         NA       0.976     NA   
      6 6        ok              7.02       0.640   0.981      5.50

