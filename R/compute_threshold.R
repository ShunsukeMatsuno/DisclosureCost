compute_threshold <- function(x, c){
  # x and c are private signals and cost of disclosure for each firm.
  # x and cis T-dimensional vector, where T is the observed number of periods of the firm.
  
  c <- unique(c)
  
  obj <- function(tau){
    if(is.na(mean(x[x <= tau]))){
      m <- 0
    }else{
      m <- mean(x[x <= tau])
    }
    abs(m - (tau - c))
  }
  
  opt_res <- optim(par = .3, 
                   fn = obj, 
                   method = 'Brent',
                   lower = -3,
                   upper = 3)
  
  return(opt_res$par)
  
}