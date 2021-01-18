simulate_data <- function(dist, num_firms, periods){
  # this function simulate data according to specified distributions
  # dist == 1 : normal dist.
  #         2 : uniform dist. over [-3, 3]
  #         3 : exponential dist. with rate = 1
  #         4 : t-dist with df 5
  
  # simulation primitives
  c_vec <- seq(0.4, 2.1, by = .1)
  
  # raw data
  df_firm <- tibble(
    firm = rep(1:(num_firms * length(c_vec)), each = periods),
    time = rep(1:periods, times = num_firms * length(c_vec)),
    c    = rep(c_vec, each = num_firms * periods),
    x = case_when(
      dist == 1 ~ rnorm(num_firms * periods * length(c_vec)),
      dist == 2 ~ runif(num_firms * periods * length(c_vec), -3, 3),
      dist == 3 ~ rexp(num_firms * periods * length(c_vec)),
      dist == 4 ~ rt(num_firms * periods * length(c_vec), 5)
    )) 
  
  # tau
  df_tau <- df_firm %>% 
    group_by(firm) %>% 
    summarise(tau = compute_threshold(x, c),
              .groups = "drop") %>% 
    ungroup()
  
  # disclosure
  df_firm <-left_join(df_firm, df_tau, by = "firm") %>% 
    mutate(disclosure = if_else(x >= tau, x, NA_real_)) %>% 
    group_by(firm) %>% 
    mutate(p_hat = mean(!is.na(disclosure))) %>% 
    ungroup()
  
  # drop firm with p = 0,1
  df_firm <- df_firm %>% 
    filter(p_hat >0 & p_hat < 1)
  
  return(df_firm) 
}
  