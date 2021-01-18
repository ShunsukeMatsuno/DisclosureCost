bias_error_plot_distribution <- function(dist, num_firms, periods){
  # this function computes the error and bias according to specified distributions
  # dist == 1 : normal dist.
  #         2 : uniform dist. over [-3, 3]
  #         3 : exponential dist. with rate = 1
  #         4 : t-dist with df 10

  # simulate data
  df_firm <- simulate_data(dist, num_firms, periods)
  
  # cost
  df_cost <- compute_cost_sum(df_firm)
  
  # bias
  g1 <- df_cost %>% 
    select(c, starts_with("bias")) %>% 
    pivot_longer(-c, names_to = 'type', values_to = 'bias') %>% 
    ggplot(aes(x = c, y = bias, color = type)) +
    geom_line(aes(linetype = type), size = 1,
              show.legend = FALSE) +
    geom_point(aes(colour = type),
               show.legend = FALSE) +
    geom_hline(yintercept = 0, alpha = .3) +
    theme_bw() 
  
  # error
  g2 <- df_cost %>% 
    select(c, starts_with("error")) %>% 
    pivot_longer(-c, names_to = 'type', values_to = 'error') %>% 
    ggplot(aes(x = c, y = error, color = type)) +
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
