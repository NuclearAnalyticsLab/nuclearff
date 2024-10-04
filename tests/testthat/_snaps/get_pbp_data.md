# get_pbp_data snapshot test

    Code
      pbp_output
    Message
      -- nflverse play by play data --------------------------------------------------
      i Data updated: 2024-08-19 09:50:07 EDT
    Output
      # A tibble: 2,816 x 372
         play_id game_id     old_game_id home_team away_team season_type  week posteam
           <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
       1       1 2023_01_AR~ 2023091007  WAS       ARI       REG             1 <NA>   
       2      39 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       3      55 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       4      77 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       5     102 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       6     124 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       7     147 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       8     172 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
       9     197 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
      10     220 2023_01_AR~ 2023091007  WAS       ARI       REG             1 WAS    
      # i 2,806 more rows
      # i 364 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
      #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
      #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
      #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
      #   down <dbl>, goal_to_go <int>, time <chr>, yrdln <chr>, ydstogo <dbl>,
      #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, ...

