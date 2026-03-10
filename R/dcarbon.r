# Nico Gavigan
# Feb 26, 2026
# Assignment 6 - Sobol ODE
#------------------------------------------------------------------------------

# Forest Growth Model

dcarbon <- function(t, C, parms){
  with(as.list(c(C, parms)),{
    if(C < thresh){
      dC = r * C
    } else {
      dC = g * (1 - C/K)
    } 
    return(list(dC))
  })
}