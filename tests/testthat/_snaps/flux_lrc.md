# flux LRC works

    Code
      output
    Output
      # A tibble: 488 x 6
           PAR_ave type  datetime             f_flux warming par_correction
             <dbl> <chr> <dttm>                <dbl> <chr>   <lgl>         
       1 1158.     NEE   2020-08-08 16:31:00  22.7   control TRUE          
       2 1158.     NEE   2020-08-08 16:31:00   0.489 control NA            
       3    0.0941 ER    2020-08-22 10:56:45  22.5   control TRUE          
       4    0.0941 ER    2020-08-22 10:56:45  22.5   control NA            
       5    0.119  ER    2020-08-22 11:00:15  29.9   control TRUE          
       6    0.119  ER    2020-08-22 11:00:15  29.9   control NA            
       7    0.131  ER    2020-08-22 11:03:30  26.3   control TRUE          
       8    0.131  ER    2020-08-22 11:03:30  26.3   control NA            
       9   81.9    NEE   2020-08-22 11:07:00 -13.3   control TRUE          
      10   81.9    NEE   2020-08-22 11:07:00  13.4   control NA            
      # i 478 more rows

# flux LRC works without groups

    Code
      output
    Output
      # A tibble: 488 x 6
           PAR_ave type  datetime            f_flux warming par_correction
             <dbl> <chr> <dttm>               <dbl> <chr>   <lgl>         
       1 1158.     NEE   2020-08-08 16:31:00 18.9   control TRUE          
       2 1158.     NEE   2020-08-08 16:31:00  0.489 control NA            
       3    0.0941 ER    2020-08-22 10:56:45 22.5   control TRUE          
       4    0.0941 ER    2020-08-22 10:56:45 22.5   control NA            
       5    0.119  ER    2020-08-22 11:00:15 29.9   control TRUE          
       6    0.119  ER    2020-08-22 11:00:15 29.9   control NA            
       7    0.131  ER    2020-08-22 11:03:30 26.3   control TRUE          
       8    0.131  ER    2020-08-22 11:03:30 26.3   control NA            
       9   81.9    NEE   2020-08-22 11:07:00 -9.73  control TRUE          
      10   81.9    NEE   2020-08-22 11:07:00 13.4   control NA            
      # i 478 more rows

