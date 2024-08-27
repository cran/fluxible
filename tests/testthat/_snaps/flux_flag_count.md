# quality flags count works

    Code
      flux_flag_count(slopes30lin_flag)
    Output
      # A tibble: 7 x 3
        f_quality_flag     n ratio
        <fct>          <int> <dbl>
      1 ok                 5 0.833
      2 zero               1 0.167
      3 discard            0 0    
      4 weird_flux         0 0    
      5 start_error        0 0    
      6 no_data            0 0    
      7 force_ok           0 0    

