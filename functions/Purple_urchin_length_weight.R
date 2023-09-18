


################################################################################
# About
# This is the processing script used to derive purple sea urchin length-weight
# conversion parameters. The conversion parameters were obtained using 
#  132 purple sea urchins (strongylocentrotus purpurtus) collected by J. Smith. 
# Sea urchins were brought to the lab, measured, and dissected. 
# three individuals had no gonads and those values are reported as 0. 

################################################################################
#metadata

# date = year/month/day
# site = location of sample collection
# test_dia_mm = diameter of the sea urchin test not including spines
# test_hei_mm = diameter from the jaw to the aboral surface
# wet_mass_g = measured wet mass of the animal in g
# volume_disp_ml = volume displaced in ml by submersion. 
# gonad_index = gonad mass / animal mass

################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here)

#read urchin data
urch_dat_orig <- read_csv(file.path(here::here("output"),"purple_urchin_biomass.csv"))


################################################################################
# derive parameters

# Initial estimates based on data
a_init <- -20
b_init <- 10
c_init <- 0.03

# Fit the biomass_fun model to the data with initial estimates

set.seed(1985)

fit <- nls(wet_mass_g ~ a + b * exp(c * test_dia_mm), 
           data = urch_dat_orig,
           start = list(a = a_init, b = b_init, c = c_init))

# Extract the estimated parameters
a_est <- coef(fit)["a"]
b_est <- coef(fit)["b"]
c_est <- coef(fit)["c"]

# Print the estimated parameters
cat("Estimated Parameters:\n")
cat("a:", a_est, "\n")
cat("b:", b_est, "\n")
cat("c:", c_est, "\n")

#test the model

a_est + b_est * exp(c_est * 40)


################################################################################
#determine fit

# Calculate the predicted values from the model
predicted_values <- fitted(fit)

# Calculate the residuals
residuals <- urch_dat_orig$wet_mass_g - predicted_values

# Calculate the RSS (Residual Sum of Squares)
rss <- sum(residuals^2)

# Calculate the TSS (Total Sum of Squares)
mean_wet_mass <- mean(urch_dat_orig$wet_mass_g)
tss <- sum((urch_dat_orig$wet_mass_g - mean_wet_mass)^2)

# Calculate R-squared
r_squared <- 1 - (rss / tss)

# Print the R-squared value
cat("R-squared:", r_squared, "\n")


################################################################################
#plot

ggplot(urch_dat_orig, aes(x = test_dia_mm, y = wet_mass_g)) +
  geom_point() +
  geom_line(aes(y = a_est + b_est * exp(c_est * test_dia_mm)), color = "purple", size = 1) +
  labs(x = "Test Diameter (mm)", y = "Wet Mass (g)") 


################################################################################
# these are the units defined by the same model fit with JMP statistical software. 
# the units are reported in Smith et al. 2021 PNAS. 

a <- -22.45586
b <- 12.231189
c <- 0.0346918

biomass_fun < a + b * exp(c * test_dia_mm)










