skupin.function <- function(x, alpha, beta) {
  if (x >= .7) {
    adjusted <- alpha / ((1 - x) ^ beta)
  } else{
    adjusted <- 1 / ((1 - x) ^ (beta - (log(alpha) / log(.3))))
  }
  
  return (adjusted)
}

growthFunction <- function(function.selected, rh, alpha, beta) {
  if (function.selected == 'haehnel') {
    adjusted <- ((1 - rh) ^ beta)
  }
  else if (function.selected == 'soneja') {
    adjusted <- 1 + (alpha * rh ^ 2) / (1 - rh)
  }
  
  else if (function.selected == 'combo') {
    adjusted <- 1 + (alpha * rh ^ 2) / ((1 - rh) ^ beta)
  } else if (function.selected == 'skupin') {
    adjusted <- mapply(
      FUN = skupin.function,
      x = rh,
      alpha = alpha,
      beta = beta
    )
    
  }
  return(adjusted)
}