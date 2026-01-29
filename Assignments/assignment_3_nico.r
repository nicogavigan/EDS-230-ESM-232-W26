# Assignment 3: Almond Yield Anomaly
# Nico Gavigan
# Jan 21 2026
# -------------------------------------------
# CALCULATING ALMOND YIELD ANOMALY
# -------------------------------------------

# Equation:
#         Y = -0.015Tn - 0.0046Tn^2 - 0:07P + 0.0043P^2 + 0.28
# Y   : yield anomaly (ton/acre) - departure from the expected/average yield, not total yield 
# Tn  : average minimum February temperature
# P   : total January precipitation 

# INPUTS: daily time series containing
#   daily tmin
#   daily tmax 
#   daily precip

# OUTPUTS: 
#   max yield anomaly
#   min yield anomaly
#   mean yield anomaly

# -------------------------------------------
# LOAD LIBRARIES 
library(tidyverse)
library(here)

# LOAD DATA
clim <- read_table(here("Assignments", "clim.txt")) |>
  janitor::clean_names()

# -------------------------------------------
# STEP 1: PRE-PROCESS DATA

# CALCULATE FEBRUARY MEAN MINIMUM TEMPERATURE (tmin_c)
temp <- clim |>
  filter(month == 2) |>
  group_by(year) |>
  summarize(avg_tmin_c = mean(tmin_c)) |>
  pull(avg_tmin_c)

# CREATE DATAFRAME FOR TOTAL JANUARY PRECIPITATION (sum_precip)
precip <- clim |>
  filter(month == 1) |>
  group_by(year) |>
  summarize(sum_precip_mm = sum(precip)) |>
  pull(sum_precip_mm)


# STEP 2: CREATE A FUNCTION TO CALCULATE MIN, MAX, MEAN YIELD ANOMOLY
almond_model <- function(T, P, year = NULL, B1 = -0.015, B2 = -0.0046, 
                         B3 = -0.07, B4 = 0.0043, B0 = 0.28,
                         return_type = c("summary", "yearly")){
  
  return_type <- match.arg(return_type)
  
  Y = B1*T + B2*T^2 + B3*P + B4*P^2 + B0
  
  # SET RETURN TO YIELD MIN, MAX, MEAN AND YEARLY DISTRIBUTION OF ANOMALIES
  if(return_type == "summary") {
    return(data.frame(
      statistic = c("min", "max", "mean"),
      value = c(min(Y), max(Y), mean(Y))
    ))
  }
  if(!is.null(year)){
    return(data.frame(year = year, Ya = Y))
  } else{
    return(Y)
  }
}

# STEP 3: RUN MODEL
years <- clim |>
  distinct(year) |>
  arrange(year) |>
  filter(year != "1988") |>
  pull(year)


yearly_Ya <- almond_model(T = temp, P = precip, year = years, return_type = "yearly")


