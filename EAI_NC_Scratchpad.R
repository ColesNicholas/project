############################
# Saturday: pricing sensitivity analysis
############################
# load sensitivity analysis
sat.load.sens <-
  lapply(X = c((-15000),(-10000), (-5000),
               15000,     10000,    5000
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-17") %>% 
      
      # replace with counterfactual value
      mutate(rto_combined_bidclose_load_forecast_average =
               rto_combined_bidclose_load_forecast_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(load_delta = x) %>% 
      select(load_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )
sat.load.sens <- bind_rows(sat.load.sens) %>% 
  arrange(width, load_delta) %>% 
  tableGrob(rows = NULL)

# gas sensitivity analysis
sat.gas.sens <-
  lapply(X = c((-1.5),(-1.0), (-0.5),
               1.5,   1.0,    0.5
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-17") %>% 
      
      # replace with counterfactual value
      mutate(tetco_m3_gasprice_average =
               tetco_m3_gasprice_average + x,
             henry_gasprice_average =
               henry_gasprice_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(gas_delta = x) %>% 
      select(gas_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )

sat.gas.sens <- bind_rows(sat.gas.sens) %>% 
  arrange(width, gas_delta) %>% 
  tableGrob(rows = NULL)

# solar sensitivity analysis
sat.solar.sens <-
  lapply(X = c((-1500),(-1000), (-500),
               1500,   1000,    500
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-17") %>% 
      
      # replace with counterfactual value
      mutate(rto_combined_bidclose_solarfcst_hourly_average =
               rto_combined_bidclose_solarfcst_hourly_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(solar_delta = x) %>% 
      select(solar_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )

sat.solar.sens <- bind_rows(sat.solar.sens) %>% 
  arrange(width, solar_delta) %>% tableGrob(rows = NULL)

# wind sensitivity analysis
sat.wind.sens <-
  lapply(X = c((-3000),(-2000), (-1000),
               1000,   2000,    3000
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-17") %>% 
      
      # replace with counterfactual value
      mutate(rto_combined_bidclose_winddata_stf_average =
               rto_combined_bidclose_winddata_stf_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(wind_delta = x) %>% 
      select(wind_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )

sat.wind.sens <- bind_rows(sat.wind.sens) %>% 
  arrange(width, wind_delta) %>% tableGrob(rows = NULL)

############################
# Sunday: pricing sensitivity analysis
############################
# load sensitivity analysis
sun.load.sens <-
  lapply(X = c((-15000),(-10000), (-5000),
               15000,     10000,    5000
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-18") %>% 
      
      # replace with counterfactual value
      mutate(rto_combined_bidclose_load_forecast_average =
               rto_combined_bidclose_load_forecast_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(load_delta = x) %>% 
      select(load_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )
sun.load.sens <- bind_rows(sun.load.sens) %>% 
  arrange(width, load_delta) %>% 
  tableGrob(rows = NULL)

# gas sensitivity analysis
sun.gas.sens <-
  lapply(X = c((-1.5),(-1.0), (-0.5),
               1.5,   1.0,    0.5
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-18") %>% 
      
      # replace with counterfactual value
      mutate(tetco_m3_gasprice_average =
               tetco_m3_gasprice_average + x,
             henry_gasprice_average =
               henry_gasprice_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(gas_delta = x) %>% 
      select(gas_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )

sun.gas.sens <- bind_rows(sun.gas.sens) %>% 
  arrange(width, gas_delta) %>% 
  tableGrob(rows = NULL)

# solar sensitivity analysis
sun.solar.sens <-
  lapply(X = c((-1500),(-1000), (-500),
               1500,   1000,    500
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-18") %>% 
      
      # replace with counterfactual value
      mutate(rto_combined_bidclose_solarfcst_hourly_average =
               rto_combined_bidclose_solarfcst_hourly_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(solar_delta = x) %>% 
      select(solar_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )

sun.solar.sens <- bind_rows(sun.solar.sens) %>% 
  arrange(width, solar_delta) %>% tableGrob(rows = NULL)

# wind sensitivity analysis
sun.wind.sens <-
  lapply(X = c((-3000),(-2000), (-1000),
               1000,   2000,    3000
  ),
  function(x){
    
    c.test_df <-
      df_lag %>% 
      # filter by target date
      filter(date_time == "2026-01-18") %>% 
      
      # replace with counterfactual value
      mutate(rto_combined_bidclose_winddata_stf_average =
               rto_combined_bidclose_winddata_stf_average + x)
    
    c.test_h2o  <- as.h2o(c.test_df)
    
    # re-compile predictions
    c.pred_point <- h2o.predict(dl_point, c.test_h2o) %>% as.data.frame()
    
    c.pred_50 <- h2o.predict(dl_50, c.test_h2o) %>% as.data.frame()
    
    c.pred_99_lo <- h2o.predict(dl_99_lo, c.test_h2o) %>% as.data.frame()
    c.pred_99_hi <- h2o.predict(dl_99_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_95_lo <- h2o.predict(dl_95_lo, c.test_h2o) %>% as.data.frame()
    c.pred_95_hi <- h2o.predict(dl_95_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_85_lo <- h2o.predict(dl_85_lo, c.test_h2o) %>% as.data.frame()
    c.pred_85_hi <- h2o.predict(dl_85_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_75_lo <- h2o.predict(dl_75_lo, c.test_h2o) %>% as.data.frame()
    c.pred_75_hi <- h2o.predict(dl_75_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_65_lo <- h2o.predict(dl_65_lo, c.test_h2o) %>% as.data.frame()
    c.pred_65_hi <- h2o.predict(dl_65_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_55_lo <- h2o.predict(dl_55_lo, c.test_h2o) %>% as.data.frame()
    c.pred_55_hi <- h2o.predict(dl_55_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_45_lo <- h2o.predict(dl_45_lo, c.test_h2o) %>% as.data.frame()
    c.pred_45_hi <- h2o.predict(dl_45_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_35_lo <- h2o.predict(dl_35_lo, c.test_h2o) %>% as.data.frame()
    c.pred_35_hi <- h2o.predict(dl_35_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_25_lo <- h2o.predict(dl_25_lo, c.test_h2o) %>% as.data.frame()
    c.pred_25_hi <- h2o.predict(dl_25_hi, c.test_h2o) %>% as.data.frame()
    
    c.pred_15_lo <- h2o.predict(dl_15_lo, c.test_h2o) %>% as.data.frame()
    c.pred_15_hi <- h2o.predict(dl_15_hi, c.test_h2o) %>% as.data.frame()
    
    c.results <- c.test_df %>%
      select(date_time, western_hub_dalmp_average) %>%
      bind_cols(
        yhat   = c.pred_point$predict,
        lo99   = c.pred_99_lo$predict,
        hi99   = c.pred_99_hi$predict,
        lo95   = c.pred_95_lo$predict,
        hi95   = c.pred_95_hi$predict,
        lo85   = c.pred_85_lo$predict,
        hi85   = c.pred_85_hi$predict,
        lo75   = c.pred_75_lo$predict,
        hi75   = c.pred_75_hi$predict,
        lo65   = c.pred_65_lo$predict,
        hi65   = c.pred_65_hi$predict,
        lo55   = c.pred_55_lo$predict,
        hi55   = c.pred_55_hi$predict,
        lo45   = c.pred_45_lo$predict,
        hi45   = c.pred_45_hi$predict,
        lo35   = c.pred_35_lo$predict,
        hi35   = c.pred_35_hi$predict,
        lo25   = c.pred_25_lo$predict,
        hi25   = c.pred_25_hi$predict,
        lo15   = c.pred_15_lo$predict,
        hi15   = c.pred_15_hi$predict,
        med = c.pred_50$predict,
      )
    
    # prepare prediction band summaries
    c.summary <-
      c.results %>% 
      pivot_longer(lo95 : hi65) %>% 
      extract(
        col   = name,                 # column to split
        into  = c("bound", "width"),     # new columns: hi/lo and numeric width
        regex = "^(hi|lo)(\\d+)$",       # capture 'hi' or 'lo' then digits
        convert = TRUE                   # converts width to integer
      ) %>% 
      pivot_wider(names_from = 'bound',
                  values_from = 'value') %>% 
      mutate(wind_delta = x) %>% 
      select(wind_delta, width, lo, hi) %>% 
      mutate(lo = round(lo, 2),
             hi = round(hi, 2))
    
    return(c.summary)
    
  }
  )

sun.wind.sens <- bind_rows(sun.wind.sens) %>% 
  arrange(width, wind_delta) %>% tableGrob(rows = NULL)

### Labels
sat.title <-
  ggdraw() +
  draw_label(paste0("2026-01-17",
                    " sensitivity analyses"),
             fontface = 'bold', 
             x = 0.5, hjust = 0.5)

sun.title <-
  ggdraw() +
  draw_label(paste0("2026-01-18",
                    " sensitivity analyses"),
             fontface = 'bold', 
             x = 0.5, hjust = 0.5)

plot_grid(
  sat.title,
  plot_grid(sat.load.sens, sat.gas.sens, 
            sat.solar.sens, sat.wind.sens,
            ncol = 4),
  nrow = 2,
  rel_heights = c(1, 25))

plot_grid(
  sun.title,
  plot_grid(sun.load.sens, sun.gas.sens, 
            sun.solar.sens, sun.wind.sens,
            ncol = 4),
  nrow = 2,
  rel_heights = c(1, 25))