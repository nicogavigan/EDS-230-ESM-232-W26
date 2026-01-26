compute_almond_yield <- function(clim, Tmincoeff1 = -0.015, Tmincoeff2 = -0.0){
  
}

tmp <- clim |>
  group_by(month, year) |>
  dplyr::summarise(tmin_c = min(tmin_c), .groups = "drop")

Feb_minT <- (tmp |> subset(month == 2$tmin_c))



