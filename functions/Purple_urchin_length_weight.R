


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
a_est + b_est * exp(c_est * test_mm)


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


################################################################################
# build function to estimate biomass per unit area


# Function to calculate total biomass for the full area in kg
total_biomass <- function(average_density, total_area, urch_dat_orig) {
  # Calculate the total number of urchins in the area
  total_urchins = average_density * total_area
  
  # Create a size frequency distribution dataframe from the real data
  size_distribution_df <- data.frame(
    test_mm = urch_dat_orig$test_dia_mm,
    counts = 1 # Each row in urch_dat_orig represents a single count
  )
  
  # Calculate total biomass for the full area by applying the single urchin biomass function
  total_biomass_grams = sum(single_urchin_biomass(size_distribution_df$test_mm) * size_distribution_df$counts)
  
  # Convert the biomass from grams to kilograms
  total_biomass_kg = total_biomass_grams / 1000
  
  # Multiply the calculated biomass by the total_area to get the total for the entire area
  total_biomass_kg = total_biomass_kg * total_area
  
  return(total_biomass_kg)
}

# Example usage
average_density <- 1 # per m^2
total_area <- 1 # square meters

# Example dataframe with real data
urch_dat_orig <- data.frame(
  test_dia_mm = c(10, 20, 30, 20, 30, 10, 40, 25, 15) # Example size frequencies in mm
)

total_area_biomass <- total_biomass(average_density, total_area, urch_dat_orig)
cat("Total Biomass for the full area:", total_area_biomass, "kg\n")


################################################################################
# build Shiny

# Install and load the Shiny library if not already installed
# install.packages("shiny")
library(shiny)

# Function to calculate the biomass of a single urchin in grams
single_urchin_biomass_grams <- function(test_mm) {
  biomass_grams = -19.94355 + 10.71374 * exp(0.03670476 * test_mm)
  return(biomass_grams)
}

# Function to calculate total biomass for the full area in kg and lbs
total_biomass <- function(average_density, total_area, size_distribution_df) {
  # Calculate the total number of urchins in the area
  total_urchins = average_density * total_area
  
  # Calculate total biomass for the full area by applying the single urchin biomass function to each individual
  total_biomass_grams = sum(sapply(size_distribution_df$test_mm, single_urchin_biomass_grams))
  
  # Convert the total biomass from grams to kilograms
  total_biomass_kg = total_biomass_grams / 1000
  
  # Multiply the total biomass in kilograms by the total number of urchins to get the total for the entire area
  total_biomass_kg = total_biomass_kg * total_urchins
  
  # Convert biomass to pounds
  total_biomass_lbs = total_biomass_kg * 2.20462
  
  return(c(kg = total_biomass_kg, lbs = total_biomass_lbs))
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Urchin Biomass Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Select Size Data CSV File:"),
      numericInput("density", "Average Density (per m^2):", value = 10),
      numericInput("area", "Total Area (square meters):", value = 100),
      selectInput("size_units", "Size Units:", choices = c("mm", "cm"), selected = "mm"),
      selectInput("size_column", "Select Size Column:", choices = NULL)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {
  data_loaded <- reactive({
    req(input$datafile)
    
    # Load the selected CSV file
    df <- read.csv(input$datafile$datapath)
    
    # Update the choices for the size column dropdown based on the columns in the CSV file
    updateSelectInput(session, "size_column", choices = names(df))
    
    return(df)
  })
  
  total_biomass_result <- reactive({
    req(data_loaded(), input$density, input$area, input$size_units, input$size_column)
    
    df <- data_loaded()
    
    # Check if the data frame is empty
    if (is.null(input$datafile$datapath) || !file.exists(input$datafile$datapath)) {
      return(c(kg = 0, lbs = 0))
    }
    
    # Extract the size data from the selected column and convert to numeric
    size_data <- as.numeric(df[, input$size_column])
    
    # Check for missing or non-numeric values
    if (any(is.na(size_data)) || any(!is.numeric(size_data))) {
      return(c(kg = 0, lbs = 0))
    }
    
    # Convert size data to mm if size units are cm
    if (input$size_units == "cm") {
      size_data <- size_data * 10 # Convert cm to mm
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(
      test_mm = size_data
    )
    
    # Calculate total biomass for the full area
    total_biomass_kg_lbs <- total_biomass(input$density, input$area, size_distribution_df)
    
    return(total_biomass_kg_lbs)
  })
  
  output$result <- renderText({
    paste("Total Biomass for the full area:", total_biomass_result()["kg"], "kg (", total_biomass_result()["lbs"], "lbs)")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
