# flux LRC works

    Code
      output
    Output
      # A tibble: 231 x 6
           PAR_ave type  datetime            f_flux warming PAR_corrected_flux
             <dbl> <chr> <dttm>               <dbl> <chr>                <dbl>
       1 1158.     NEE   2020-08-08 16:31:00  0.489 control              22.7 
       2    0.0941 ER    2020-08-22 10:56:45 22.5   control              22.5 
       3    0.119  ER    2020-08-22 11:00:15 29.9   control              29.9 
       4    0.131  ER    2020-08-22 11:03:30 26.3   control              26.3 
       5   81.9    NEE   2020-08-22 11:07:00 13.4   control             -13.3 
       6   87.1    NEE   2020-08-22 11:10:15 13.1   control             -13.3 
       7   88.3    NEE   2020-08-22 11:14:45 23.6   control              -2.69
       8    0.218  ER    2020-08-22 11:19:45 67.2   warming              67.2 
       9    0.252  ER    2020-08-22 11:22:45 48.8   warming              48.8 
      10    0.324  ER    2020-08-22 11:25:45 65.7   warming              65.7 
      # i 221 more rows

# flux LRC works without groups

    Code
      output
    Output
      # A tibble: 231 x 6
           PAR_ave type  datetime            f_flux warming PAR_corrected_flux
             <dbl> <chr> <dttm>               <dbl> <chr>                <dbl>
       1 1158.     NEE   2020-08-08 16:31:00  0.489 control             18.9  
       2    0.0941 ER    2020-08-22 10:56:45 22.5   control             22.5  
       3    0.119  ER    2020-08-22 11:00:15 29.9   control             29.9  
       4    0.131  ER    2020-08-22 11:03:30 26.3   control             26.3  
       5   81.9    NEE   2020-08-22 11:07:00 13.4   control             -9.73 
       6   87.1    NEE   2020-08-22 11:10:15 13.1   control             -9.75 
       7   88.3    NEE   2020-08-22 11:14:45 23.6   control              0.807
       8    0.218  ER    2020-08-22 11:19:45 67.2   warming             67.2  
       9    0.252  ER    2020-08-22 11:22:45 48.8   warming             48.8  
      10    0.324  ER    2020-08-22 11:25:45 65.7   warming             65.7  
      # i 221 more rows

