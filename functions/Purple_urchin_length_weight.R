


################################################################################
# About
# This is the processing script used to derive purple sea urchin length-weight
# conversion parameters. The conversion parameters were obtained using 
#  132 purple sea urchins (strongylocentrotus purpuratus) collected by J. Smith. 
# Sea urchins were brought to the lab, measured, weighed, and dissected. 
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
# Function to calculate the biomass of a single urchin

biomass_conversion <- function(test_mm) {
  biomass = -19.94355 + 10.71374 * exp(0.03670476 * test_mm)
  return(biomass)
}

# Function to calculate total biomass for the full area in kg
total_biomass <- function(average_density, total_area, urch_dat_orig) {
  # Calculate the total number of urchins in the area
  total_urchins = average_density * total_area
  
  # Create a size frequency distribution dataframe from the real data
  size_distribution_df <- data.frame(
    test_mm = urch_dat_orig$test_dia_mm,
    counts = 1 # Each row in urch_dat_orig represents a single count
  )
  
  # Calculate the relative proportions of each size class
  size_class_proportions <- table(size_distribution_df$test_mm) / length(size_distribution_df$test_mm)
  
  # Estimate the number of individuals for each size class in the focal area
  size_class_counts_in_sample <- round(size_class_proportions * total_urchins)
  
  #expand to long
  size_class_long <- vcdExtra::expand.dft(size_class_counts_in_sample, freq="Freq")
  
  # Apply the biomass conversion function to the size column in size_class_long
  size_class_long$Biomass <- biomass_conversion(size_class_long$Var1)
  
  # Calculate the total biomass for all individuals in the sample
  total_biomass_in_sample <- sum(size_class_long$Biomass) / 1000 # convert to kg
  
  return(total_biomass_in_sample)
}

# Example usage
average_density <- 11 # per m^2
total_area <- 20 # square meters

total_area_biomass <- total_biomass(average_density, total_area, urch_dat_orig)

cat("Total Biomass for the full area:", total_area_biomass, "kg\n")



################################################################################
# build Shiny 

# Load required libraries
library(shiny)
library(vcdExtra)
library(readr)

# Function to load sample data
load_sample_data <- function() {
  urch_dat_orig <- read_csv(file.path(here::here("output"), "purple_urchin_biomass.csv"))
  return(urch_dat_orig)
}

# Function to calculate the biomass of a single urchin in grams
single_urchin_biomass_grams <- function(test_mm) {
  biomass_grams = -19.94355 + 10.71374 * exp(0.03670476 * test_mm)
  return(biomass_grams)
}

# Function to calculate total biomass for the full area in kg and lbs
total_biomass <- function(average_density, total_area, size_distribution_df, size_units) {
  # Calculate the total number of urchins in the area
  total_urchins = average_density * total_area
  
  # Apply the conversion from cm to mm if size units are cm
  if (size_units == "cm") {
    size_distribution_df$test_mm <- size_distribution_df$test_mm * 10
  }
  
  # Calculate the relative proportions of each size class
  size_class_proportions <- table(size_distribution_df$test_mm) / nrow(size_distribution_df)
  
  # Estimate the number of individuals for each size class in the focal area
  size_class_counts_in_sample <- round(size_class_proportions * total_urchins)
  
  # Expand to long
  size_class_long <- vcdExtra::expand.dft(size_class_counts_in_sample, freq = "Freq")
  
  # Apply the biomass conversion function to the size column in size_class_long
  size_class_long$Biomass <- single_urchin_biomass_grams(size_class_long$Var1)
  
  # Calculate total biomass for the full area by summing up individual biomass
  total_biomass_grams <- round(sum(size_class_long$Biomass), digits = 2)
  
  # Calculate the total biomass for all individuals in the sample
  total_biomass_kg <- round(total_biomass_grams / 1000, digits = 2) # convert to kg
  
  # Convert biomass to pounds
  total_biomass_lbs = round(total_biomass_kg * 2.20462, digits = 2)
  
  return(c(g = total_biomass_grams, kg = total_biomass_kg, lbs = total_biomass_lbs))
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Urchin Biomass Calculator"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Data Source:", choices = c("Sample Data", "Upload Data"), selected = "Sample Data"),
      conditionalPanel(
        condition = "input.data_source == 'Upload Data'",
        fileInput("datafile", "Select Size Data CSV File:")
      ),
      numericInput("density", "Average Density (per m^2):", value = 10),
      numericInput("target_density", "Target Density (per m^2):", value = 10), # Added target density input
      numericInput("area", "Total Area (square meters):", value = 100),
      selectInput("size_units", "Size Units:", choices = c("mm", "cm"), selected = "mm"),
      uiOutput("default_size_column")  # Dynamic UI for setting the default size column
    ),
    mainPanel(
      htmlOutput("result_average_density"),  # Use HTMLOutput instead of textOutput
      plotOutput("histogram", width = "500px", height = "400px"),  # Set the plot dimensions
      htmlOutput("target_density_difference")  # Use HTMLOutput instead of textOutput
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {
  data_loaded <- reactive({
    if (input$data_source == "Sample Data") {
      df <- load_sample_data()
    } else {
      req(input$datafile)
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      
      # Convert the selected column to numeric, replacing non-numeric values with NA
      df[[input$size_column]] <- as.numeric(df[[input$size_column]])
    }
    
    # Update the choices for the size column dropdown based on the columns in the selected data
    updateSelectInput(session, "size_column", choices = names(df))
    
    return(df)
  })
  
  # Dynamically set the default size column based on the data source
  output$default_size_column <- renderUI({
    if (input$data_source == "Sample Data") {
      selected_column <- "test_dia_mm"  # Default size column for Sample Data
    } else {
      selected_column <- input$size_column
    }
    
    selectInput("size_column", "Select Size Column:", choices = names(df), selected = selected_column)
  })
  
  # Calculate total biomass result for average density
  total_biomass_result_average_density <- reactive({
    req(data_loaded(), input$density, input$area, input$size_units, input$size_column)
    
    df <- data_loaded()
    
    # Check if the data frame is empty
    if (nrow(df) == 0) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Extract the size data from the selected column
    size_data <- df[[input$size_column]]
    
    # Replace non-numeric values with NA
    size_data[is.na(as.numeric(size_data))] <- NA
    
    # Check for missing or non-numeric values
    if (all(is.na(size_data))) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(test_mm = size_data)
    
    # Calculate total biomass for the full area
    total_biomass_grams_kg_lbs <- total_biomass(input$density, input$area, size_distribution_df, input$size_units)
    
    return(total_biomass_grams_kg_lbs)
  })
  
  # Calculate total biomass result for target density
  total_biomass_result_target_density <- reactive({
    req(data_loaded(), input$target_density, input$area, input$size_units, input$size_column)
    
    df <- data_loaded()
    
    # Check if the data frame is empty
    if (nrow(df) == 0) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Extract the size data from the selected column
    size_data <- df[[input$size_column]]
    
    # Replace non-numeric values with NA
    size_data[is.na(as.numeric(size_data))] <- NA
    
    # Check for missing or non-numeric values
    if (all(is.na(size_data))) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(test_mm = size_data)
    
    # Calculate total biomass for the full area for target density
    total_biomass_grams_kg_lbs <- total_biomass(input$target_density, input$area, size_distribution_df, input$size_units)
    
    return(total_biomass_grams_kg_lbs)
  })
  
  # Calculate the difference between total biomass for average and target density
  total_biomass_difference <- reactive({
    average_biomass <- total_biomass_result_average_density()
    target_biomass <- total_biomass_result_target_density()
    
    # Calculate the difference for each unit (g, kg, lbs)
    difference <- average_biomass - target_biomass
    
    return(difference)
  })
  
  # Render the average density output in red
  output$result_average_density <- renderUI({
    result_text <- paste(
      "Total Biomass in Kg (Average Density):", total_biomass_result_average_density()["kg"], "kg", 
      "<br>Total Biomass in Grams (Average Density):", total_biomass_result_average_density()["g"], "g", 
      "<br>Total Biomass in Pounds (Average Density):", total_biomass_result_average_density()["lbs"], "lbs"
    )
    
    HTML(paste("<span style='color:red;'>", result_text, "</span>"))
  })
  
  # Render the histogram
  output$histogram <- renderPlot({
    df <- data_loaded()
    size_column <- input$size_column
    
    # Check if the data frame is empty
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    # Extract the size data from the selected column
    size_data <- df[[size_column]]
    
    # Replace non-numeric values with NA
    size_data[is.na(as.numeric(size_data))] <- NA
    
    # Check for missing or non-numeric values
    if (all(is.na(size_data))) {
      return(NULL)
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(test_mm = size_data)
    
    hist(size_distribution_df$test_mm, main = "Urchin Size Distribution", xlab = "Size (mm)", col = "purple")
  })
  
  # Render the target density difference output in red
  output$target_density_difference <- renderUI({
    target_difference_text <- paste(
      "Difference in Total Biomass (Kg):", total_biomass_difference()["kg"], "kg", 
      "<br>Difference in Total Biomass (Grams):", total_biomass_difference()["g"], "g", 
      "<br>Difference in Total Biomass (Pounds):", total_biomass_difference()["lbs"], "lbs"
    )
    
    HTML(paste("<span style='color:red;'>", target_difference_text, "</span>"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


################################################################################
# build Shiny 2

# Load required libraries
library(shiny)
library(vcdExtra)
library(readr)

# Function to load sample data
load_sample_data <- function() {
  urch_dat_orig <- read_csv(file.path(here::here("output"), "purple_urchin_biomass.csv"))
  return(urch_dat_orig)
}

# Function to calculate the biomass of a single urchin in grams
single_urchin_biomass_grams <- function(test_mm) {
  biomass_grams = -19.94355 + 10.71374 * exp(0.03670476 * test_mm)
  return(biomass_grams)
}

# Function to calculate total biomass for the full area in kg and lbs
total_biomass <- function(average_density, total_area, size_distribution_df, size_units) {
  # Calculate the total number of urchins in the area
  total_urchins = average_density * total_area
  
  # Apply the conversion from cm to mm if size units are cm
  if (size_units == "cm") {
    size_distribution_df$test_mm <- size_distribution_df$test_mm * 10
  }
  
  # Calculate the relative proportions of each size class
  size_class_proportions <- table(size_distribution_df$test_mm) / nrow(size_distribution_df)
  
  # Estimate the number of individuals for each size class in the focal area
  size_class_counts_in_sample <- round(size_class_proportions * total_urchins)
  
  # Expand to long
  size_class_long <- vcdExtra::expand.dft(size_class_counts_in_sample, freq = "Freq")
  
  # Apply the biomass conversion function to the size column in size_class_long
  size_class_long$Biomass <- single_urchin_biomass_grams(size_class_long$Var1)
  
  # Calculate total biomass for the full area by summing up individual biomass
  total_biomass_grams <- round(sum(size_class_long$Biomass), digits = 2)
  
  # Calculate the total biomass for all individuals in the sample
  total_biomass_kg <- round(total_biomass_grams / 1000, digits = 2) # convert to kg
  
  # Convert biomass to pounds
  total_biomass_lbs = round(total_biomass_kg * 2.20462, digits = 2)
  
  return(c(g = total_biomass_grams, kg = total_biomass_kg, lbs = total_biomass_lbs))
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Urchin Biomass Calculator"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Data Source:", choices = c("Sample Data", "Upload Data"), selected = "Sample Data"),
      conditionalPanel(
        condition = "input.data_source == 'Upload Data'",
        fileInput("datafile", "Select Size Data CSV File:")
      ),
      numericInput("density", "Average Density (per m^2):", value = 10),
      numericInput("target_density", "Target Density (per m^2):", value = 10), # Added target density input
      numericInput("area", "Total Area (square meters):", value = 100),
      selectInput("size_units", "Size Units:", choices = c("mm", "cm"), selected = "mm"),
      uiOutput("default_size_column")  # Dynamic UI for setting the default size column
    ),
    mainPanel(
      htmlOutput("result_average_density"),  # Use htmlOutput instead of HTMLOutput
      plotOutput("histogram", width = "500px", height = "400px"),  # Set the plot dimensions
      htmlOutput("target_density_difference")  # Use htmlOutput instead of HTMLOutput for target density difference
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {
  data_loaded <- reactive({
    if (input$data_source == "Sample Data") {
      df <- load_sample_data()
    } else {
      req(input$datafile)
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      
      # Convert the selected column to numeric, replacing non-numeric values with NA
      df[[input$size_column]] <- as.numeric(df[[input$size_column]])
    }
    
    # Update the choices for the size column dropdown based on the columns in the selected data
    updateSelectInput(session, "size_column", choices = names(df))
    
    return(df)
  })
  
  # Dynamically set the default size column based on the data source
  output$default_size_column <- renderUI({
    if (input$data_source == "Sample Data") {
      return(selectInput("size_column", "Select Size Column:", choices = colnames(load_sample_data())))
    } else {
      return(NULL)
    }
  })
  
  # Calculate total biomass result for average density
  total_biomass_result_average_density <- reactive({
    req(data_loaded(), input$density, input$area, input$size_units, input$size_column)
    
    df <- data_loaded()
    
    # Check if the data frame is empty
    if (nrow(df) == 0) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Extract the size data from the selected column
    size_data <- df[[input$size_column]]
    
    # Replace non-numeric values with NA
    size_data[is.na(as.numeric(size_data))] <- NA
    
    # Check for missing or non-numeric values
    if (all(is.na(size_data))) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(test_mm = size_data)
    
    # Calculate total biomass for the full area for average density
    total_biomass_grams_kg_lbs <- total_biomass(input$density, input$area, size_distribution_df, input$size_units)
    
    return(total_biomass_grams_kg_lbs)
  })
  
  # Calculate total biomass result for target density
  total_biomass_result_target_density <- reactive({
    req(data_loaded(), input$target_density, input$area, input$size_units, input$size_column)
    
    df <- data_loaded()
    
    # Check if the data frame is empty
    if (nrow(df) == 0) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Extract the size data from the selected column
    size_data <- df[[input$size_column]]
    
    # Replace non-numeric values with NA
    size_data[is.na(as.numeric(size_data))] <- NA
    
    # Check for missing or non-numeric values
    if (all(is.na(size_data))) {
      return(c(g = 0, kg = 0, lbs = 0))
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(test_mm = size_data)
    
    # Calculate total biomass for the full area for target density
    total_biomass_grams_kg_lbs <- total_biomass(input$target_density, input$area, size_distribution_df, input$size_units)
    
    return(total_biomass_grams_kg_lbs)
  })
  
  # Calculate the difference between total biomass for average and target density
  total_biomass_difference <- reactive({
    average_biomass <- total_biomass_result_average_density()
    target_biomass <- total_biomass_result_target_density()
    
    # Calculate the difference for each unit (g, kg, lbs)
    difference <- average_biomass - target_biomass
    
    return(difference)
  })
  
  # Render the average density output in red
  output$result_average_density <- renderUI({
    result_text <- paste(
      "Total Biomass in Kg (Average Density):", total_biomass_result_average_density()["kg"], "kg", 
      "<br>Total Biomass in Grams (Average Density):", total_biomass_result_average_density()["g"], "g", 
      "<br>Total Biomass in Pounds (Average Density):", total_biomass_result_average_density()["lbs"], "lbs"
    )
    
    HTML(result_text)
  })
  
  # Render the histogram
  output$histogram <- renderPlot({
    df <- data_loaded()
    size_column <- input$size_column
    
    # Check if the data frame is empty
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    # Extract the size data from the selected column
    size_data <- df[[size_column]]
    
    # Replace non-numeric values with NA
    size_data[is.na(as.numeric(size_data))] <- NA
    
    # Check for missing or non-numeric values
    if (all(is.na(size_data))) {
      return(NULL)
    }
    
    # Create a size frequency distribution dataframe
    size_distribution_df <- data.frame(test_mm = size_data)
    
    hist(size_distribution_df$test_mm, main = "Urchin Size Distribution", xlab = "Size (mm)", col = "purple")
  })
  
  # Render the target density difference output in red
  output$target_density_difference <- renderUI({
    target_difference_text <- paste(
      "Difference in Total Biomass (Kg):", total_biomass_difference()["kg"], "kg", 
      "<br>Difference in Total Biomass (Grams):", total_biomass_difference()["g"], "g", 
      "<br>Difference in Total Biomass (Pounds):", total_biomass_difference()["lbs"], "lbs"
    )
    
    HTML(paste("<span style='color:red;'>", target_difference_text, "</span>"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
