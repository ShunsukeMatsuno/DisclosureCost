bias_error_plot_distribution <- function(dist, num_firms, periods){
  # this function computes the error and bias according to specified distributions
  # dist == 1 : normal dist.
  #         2 : uniform dist. over [-3, 3]
  #         3 : exponential dist. with rate = 1
  #         4 : t-dist with df 10

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
              .groups = "drop")
  
  # disclosure
  df_firm <-left_join(df_firm, df_tau, by = "firm") %>% 
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
    mutate(c_NP  = estimate_c_NP(.),
           c_BBT = estimate_c_BBT(.)) %>% 
    group_by(firm) %>% 
    summarise(across(starts_with("c"), .fns = first),
              .groups = "drop") %>% 
    group_by(c) %>% 
    summarise(bias_NP   = mean(c_NP - c, na.rm = TRUE),
              bias_BBT  = mean(c_BBT - c, na.rm = TRUE),
              error_NP  = mean(abs(c_NP - c), na.rm = TRUE),
              error_BBT = mean(abs(c_BBT - c), na.rm = TRUE),
              .groups   = "drop")
  
  # bias
  df_cost_plot_bias <- df_cost %>% 
    group_by(c) %>% 
    summarise(across(bias_NP:bias_BBT, mean),
              .groups = "drop") %>% 
    pivot_longer(-c, names_to = 'type', values_to = 'bias')
  
  g1 <- ggplot(df_cost_plot_bias, aes(x = c, y = bias, color = type)) +
    geom_line(aes(linetype = type), size = 1,
              show.legend = FALSE) +
    geom_point(aes(colour = type),
               show.legend = FALSE) +
    geom_hline(yintercept = 0, alpha = .3) +
    theme_bw() 
  
  # error
  df_cost_plot_error <- df_cost %>% 
    group_by(c) %>% 
    summarise(across(error_NP:error_BBT, mean),
              .groups = "drop") %>% 
    pivot_longer(-c, names_to = 'type', values_to = 'error')
  
  g2 <- ggplot(df_cost_plot_error, aes(x = c, y = error, color = type)) +
    geom_line(aes(linetype = type), size = 1) +
    geom_point(aes(colour = type),
               show.legend = FALSE) +
    theme_bw() 
  
  p <- g1 + g2 +
    plot_layout(ncol = 1, guides = "collect") &theme(legend.position = "bottom") 
  p <- p +
    plot_annotation(title = case_when(
      dist == 1 ~ "Standard Normal",
      dist == 2 ~ "Uniform over [-3,3]",
      dist == 3 ~ "Exponential with rate 1",
      dist == 4 ~ "t-dist. with df 5"
    ))
    
  return(p)
  
}
