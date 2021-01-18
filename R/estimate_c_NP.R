estimate_c_NP <- function(df){
  df_cost <- df %>% 
    group_by(firm) %>% 
    mutate(tau_hat = min(disclosure, na.rm = TRUE),
           m_hat = mean(disclosure, na.rm = TRUE)) %>% 
    mutate(c_NP = max(0, tau_hat + p_hat/(1 - p_hat) * m_hat)) %>% 
    ungroup()
  
  c_NP <- df_cost %>% pull(c_NP) 
  
  return(c_NP)
}
