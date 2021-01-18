estimate_cost_sum <- function(df_firm){
  # This funciton computes c_NP and c_BBT, and then summarises
  #  the data for firm level
  # The input `df_firm` should be the same form as the return of `simulate_data.R`
  
  df_cost <- df_firm %>% 
    mutate(c_NP  = estimate_c_NP(.),
           c_BBT = estimate_c_BBT(.)) %>% 
    group_by(firm) %>% 
    summarise(across(.cols = c(starts_with("c"), p_hat),
                     .fns = first),
              .groups = "drop") %>% 
    group_by(c) %>% 
    summarise(bias_NP   = mean(c_NP - c, na.rm = TRUE),
              bias_BBT  = mean(c_BBT - c, na.rm = TRUE),
              error_NP  = mean(abs(c_NP - c), na.rm = TRUE),
              error_BBT = mean(abs(c_BBT - c), na.rm = TRUE),
              p         = mean(p_hat),
              .groups   = "drop")
  
  return(df_cost)
}
