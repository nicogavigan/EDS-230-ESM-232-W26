# Assignment 3
# Nico Gavigan
# Jan 21 2026
# -------------------------------------------
#' @parameter 



#Y = -0.015Tn,2 - 0.0046T2n,2 - 0:07P,1 + 0.0043P^2,1 + 0.28

# Y: Yield anomaly (ton/acre) - departure from the expected/average yield, not total yield 
# Tn,2 : minimum monthly temperature for Feb.
# P1 = precipitation for January


# LOAD LIBRARIES 
library(tidyverse)
library(here)

# LOAD DATA
clim <- read_table(here("Assignments", "clim.txt")) |>
  janitor::clean_names()

# INPUTS: daily time series containing
#   daily tmin
#   daily tmax 
#   daily precip

# OUTPUTS: 
#   max yield anomaly
#   min yield anomaly
#   mean yield anomaly


# STEP 1: CONVERT DAILY INTO THE SPECIFIC MONTHLY METRICS THE EQUATION REQUIRES
# PRE-PROCESS DATA

# CREATE DATAFRAME FOR FEBRUARY MEAN MINIMUM TEMPERATURE (tmin_c)
Tn2 <- clim |>
  filter(month == 2) |>
  group_by(year) |>
  summarize(avg_tmin_c = mean(tmin_c))

# CREATE DATAFRAME FOR TOTAL JANUARY PRECIPITATION (sum_precip)
P1 <- clim |>
  filter(month == 1) |>
  group_by(year) |>
  summarize(sum_precip_mm = sum(precip))

# BIND TABLES TO CREATE ONE DATAFRAME OF SELECTED CLIMATE VARIABLES
clim_var <- cbind.data.frame(P1, avg_tmin_c = Tn2$avg_tmin_c)


# STEP 2: CREATE A FUNCTION TO CALCULATE ALMOND YIELD ANOMOLY


# WORKING ALMOND MODEL FUNCTION

almond_model <- function(Tn2, P1, B1 = -0.015, B2 = -0.0046, 
                         B3 = -0.07, B4 = 0.0043, B0 = 0.28){
  
  Y = B1*Tn2 + B2*Tn2^2 + B3*P1 + B4*P1^2 + B0
  
  return(Y)
}

almond_model(Tn2 = Tn2, P1 = P1, B1 = -0.015, B2 = -0.0046, 
             B3 = -0.07, B4 = 0.0043, B0 = 0.28)

















