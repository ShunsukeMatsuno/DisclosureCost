estimate_c_BBT <- function(df){
  df_cost <- df %>% 
    group_by(firm) %>% 
    mutate(c_BBT = qnorm(1 - p_hat) + dnorm(qnorm(1 - p_hat)) / (1 - p_hat)) %>% 
    ungroup()
  
  c_BBT <- df_cost %>% pull(c_BBT) 
  
  return(c_BBT)
}