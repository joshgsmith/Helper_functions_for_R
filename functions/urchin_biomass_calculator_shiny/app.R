
rm(list=ls())


# Load required libraries
library(shiny)
library(vcdExtra)
library(readr)

#setwd(here::here("functions","urchin_biomass_calculator_shiny"))

# Function to load sample data
load_sample_data <- function() {
  urch_dat_orig <- read_csv("sample_data.csv")
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
  
  # Add the description 
  helpText("This app calculates the total biomass of purple sea urchins per focal area and the removal biomass required to achieve a target threshold density. The calculator uses the average density per m^2 and a user-defined size frequency distribution to infer the total biomass of urchins at a focal area. The size frequency, average density, target density, and restoration area should be defined by the user in a system-specific context. User-defined size frequency data should be in long format (rows as individuals with size estimates); size can be in either cm or mm, but the calculator will apply mm to the biomass conversion."),
  

  helpText("Created by: Joshua G. Smith, Monterey Bay Aquarium, jossmith@mbayaq.org"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Data Source:", choices = c("Sample Data", "Upload Data"), selected = "Sample Data"),
      conditionalPanel(
        condition = "input.data_source == 'Upload Data'",
        fileInput("datafile", "Select Size Data CSV File:")
      ),
      numericInput("density", "Average Density (per m^2):", value = 10),
      numericInput("target_density", "Target Density (per m^2):", value = 2), 
      numericInput("area", "Restoration Area (square meters):", value = 100),
      uiOutput("size_column_select"),  
      selectInput("size_units", "Size Units:", choices = c("mm", "cm"), selected = "mm"),
      helpText(HTML("Biomass (g) = a + b * exp(c * d) <br> a = -19.94355 <br> b = 10.71374 <br> c = 0.03670476 <br> d = test diameter (mm)"))
    ),
    mainPanel(
      htmlOutput("result_average_density"),  
      plotOutput("histogram", width = "500px", height = "400px"),  # Set the plot dimensions
      htmlOutput("target_density_difference")  
    )
  )
)



# Define the server logic 
server <- function(input, output, session) {
  data_loaded <- reactive({
    if (input$data_source == "Sample Data") {
      df <- load_sample_data()
    } else if (input$data_source == "Upload Data") {
      req(input$datafile)
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      # Update the choices for the size column dropdown based on the columns in the selected data
      choices <- names(df)
      updateSelectInput(session, "size_column", choices = choices)
    }
    
    return(df)
  })
  
  # Dynamically set the default size column based on the data source
  output$size_column_select <- renderUI({
    if (input$data_source == "Upload Data") {
      selectInput("size_column", "Select Size Column:", choices = names(data_loaded()))
    } else if (input$data_source == "Sample Data") {
      df <- load_sample_data()
      selectInput("size_column", "Select Size Column:", choices = names(df))
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
  
  # Render the average density output in red for removal biomass
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
    
    hist(size_distribution_df$test_mm, main = "Purple Urchin Sample Distribution", xlab = "Size (mm)", col = "purple")
  })
  
  # Render the target density difference output in red
  output$target_density_difference <- renderUI({
    target_difference_text <- paste(
      "Biomass removal to achieve target (Kg):", total_biomass_difference()["kg"], "kg", 
      "<br>Biomass removal to achieve target (Grams):", total_biomass_difference()["g"], "g", 
      "<br>Biomass removal to achieve target (Pounds):", total_biomass_difference()["lbs"], "lbs"
    )
    
    HTML(paste("<span style='color:red;'>", target_difference_text, "</span>"))
  })
}


shinyApp(ui = ui, server = server)
