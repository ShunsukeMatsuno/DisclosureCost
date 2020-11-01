bias_error_plot_distribution <- function(dist, num_firms, time){
  # this function computes the error and bias according to specified distributions
  # dist == 1 : normal dist.
  #         2 : uniform dist. over [0, 1]
  #         3 : exponential dist. with lambda = 1
  #         4 : t-dist with df 10

  # simulation primitives
  c <- seq(0.4, 2.1, by = .1)
  N <- length(c) * num_firms   # 10,000 firms for each cost
  
  # disclosure behavior
  df_firm <- tibble(
    firm = rep(1:N, each = time),
    time = rep(1:time, times = N),
    x = case_when(
      dist == 1 ~ rnorm(N*time),
      dist == 2 ~ runif(N*time),
      dist == 3 ~ rexp(N*time),
      dist == 4 ~ rt(N*time, 5)
      )
    ) %>% 
    group_by(firm) %>% 
    mutate(c = sample(c, 1)) %>% 
    mutate(tau = compute_threshold(x, c)) %>% 
    ungroup() %>% 
    mutate(disclosure = if_else(x >= tau, x, NA_real_)) %>% 
    group_by(firm) %>% 
    mutate(remain_firm = if_else(
      sum(!is.na(disclosure)) == 0 | sum(is.na(disclosure)) == 0,
      FALSE,
      TRUE)) %>% 
    ungroup() %>% 
    filter(remain_firm) %>% 
    select(-remain_firm)
  
  # cost
  df_cost <- df_firm %>% 
    mutate(c_NP = estimate_c_NP(.),
           c_BBT = estimate_c_BBT(.)) %>% 
    mutate(bias_NP = c_NP - c,
           bias_BBT = c_BBT - c,
           error_NP = abs(c_NP - c),
           error_BBT = abs(c_BBT - c))
  
  # bias
  df_cost_plot_bias <- df_cost %>% 
    group_by(c) %>% 
    summarise(across(bias_NP:bias_BBT, mean)) %>% 
    pivot_longer(-c, names_to = 'type', values_to = 'bias')
  
  g1 <- ggplot(df_cost_plot_bias, aes(x = c, y = bias, color = type)) +
    geom_line(aes(linetype = type), size = 1) +
    theme_bw() +
    theme(legend.position = 'top')
  
  # error
  df_cost_plot_error <- df_cost %>% 
    group_by(c) %>% 
    summarise(across(error_NP:error_BBT, mean)) %>% 
    pivot_longer(-c, names_to = 'type', values_to = 'error')
  
  g2 <- ggplot(df_cost_plot_error, aes(x = c, y = error, color = type)) +
    geom_line(aes(linetype = type), size = 1) +
    theme_bw() +
    theme(legend.position = 'top')
  
  p <- g1 + g2 + plot_layout(ncol = 1) + 
    plot_annotation(title = case_when(
      dist == 1 ~ "Standard Normal",
      dist == 2 ~ "Uniform over [0,1]",
      dist == 3 ~ "Exponential with rate 1",
      dist == 4 ~ "t-dist. with df 5"
    ))
    
  return(p)
  
}
