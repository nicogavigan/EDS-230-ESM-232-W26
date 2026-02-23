
#'  Simple population growth
#' @param time time
#' @param P initial population
#' @param r intrinsic growth rate 
#' @return derivative of population with time 
#' @examples use with ode solver
#' ode(y=1,time=c(1;100),dexppop, parms=c(0.012))

dexppop_play = function(time, P, parms) {
  dexpop <- parms$r*P
  
  # set rate of change to 0 if P is greater than carrying capacity 
  dexpop <- ifelse(P > parms$carry_capactiy, 0, dexpop)
  return(list(dexpop))
}