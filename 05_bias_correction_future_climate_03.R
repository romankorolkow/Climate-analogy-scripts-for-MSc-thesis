# Creator: Roman Korolkov
# Date: 18 Dec 2024
# Version: 03
# Description: The script corrects bias of future climate data.
# Duration: Approximately 3 hours
# Saved files size: ~101 MB

if (!requireNamespace("raster")) install.packages("raster") # Installing raster package
if (!requireNamespace("sf")) install.packages("sf") # Installing sf package
library(raster) # Loading raster package
library(sf) # Loading sf package

setwd("D:/Climate_Research") # Setting working directory
tmp_folder <- "D:/Climate_Research/temp_raster" # Path for temporary raster folder
dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE) # Creating temporary folder
rasterOptions(tmpdir = tmp_folder) # Configuring raster package to use temporary folder

info_file <- "output/Task1/info.RData" # Path to info file
observed_climate_file <- "output/Task2/observed_climate.RData" # Path to observed climate data
predicted_reference_climate_file <- "output/Task3/predicted_reference_climate.RData" # Path to predicted reference climate
predicted_future_climate_file <- "output/Task4/predicted_future_climate.RData" # Path to predicted future climate
load(file = info_file) # Loading info object
load(file = observed_climate_file) # Loading observed climate object
load(file = predicted_reference_climate_file) # Loading predicted reference climate object
load(file = predicted_future_climate_file) # Loading predicted future climate object
file_name <- "input/countries/CNTR_RG_01M_2020_4326.shp" # Path to shapefile
countries <- st_read(file_name) # Loading shapefile
hungary <- st_geometry(countries[countries$CNTR_NAME == "Magyarország", ]) # Extracting Hungary geometry

create_divergent_palette <- function(min_value, max_value, center_value) { # Creating divergent palette
  if (min_value >= center_value) { # If all values are above the center value
    n_colors <- 100 # Defining total number of colors
    n_white_to_green <- n_colors # All colors will be in the white-to-green gradient
    white_to_green <- colorRampPalette(c("white", "green"))(n_white_to_green) # Generating white-to-green gradient
    return(white_to_green) # Returning the white-to-green palette
  } else if (max_value <= center_value) { # If all values are below the center value
    n_colors <- 100 # Defining total number of colors
    n_red_to_white <- n_colors # All colors will be in the red-to-white gradient
    red_to_white <- colorRampPalette(c("red", "white"))(n_red_to_white) # Generating red-to-white gradient
    return(red_to_white) # Returning the red-to-white palette
  } else { # Values are below and above the center value
    n_colors <- 100 # Defining total number of colors
    n_red_to_white <- round((center_value - min_value) / (max_value - min_value) * n_colors) # Calculating number of colors for the red-to-white gradient
    n_white_to_green <- n_colors - n_red_to_white # Calculating number of colors for the white-to-green gradient
    red_to_white <- colorRampPalette(c("red", "white"))(n_red_to_white) # Generating red-to-white gradient
    white_to_green <- colorRampPalette(c("white", "green"))(n_white_to_green) # Generating white-to-green gradient
    return(c(red_to_white, white_to_green)) # Combining red-to-white and white-to-green gradients into a single palette
  }
}

biases <- setNames(lapply(X = info$model_combinations$combination, FUN = function(combination) {
  parameter_stacks <- lapply(X = info$climate_parameters, FUN = function(parameter) {
    monthly_layers <- lapply(X = 1:12, FUN = function(month) {
      layer_name <- paste0(parameter, formatC(x = month, width = 2, flag = "0")) # Creating layer name
      obs_layer <- observed_climate[[layer_name]] # Extracting observed layer
      ref_layer <- predicted_reference_climate[[combination]][[layer_name]] # Extracting reference layer
      if (info$parameter_types[[parameter]] == "temperature") { 
        ref_layer - obs_layer # Calculating bias as difference for temperature
      } else {
        ref_layer / obs_layer # Calculating bias as ratio for precipitation
      } # if
    }) # monthly_layers
    stack(monthly_layers) # Combining monthly layers into RasterStack
  }) # parameter_stacks
  raster_stack <- do.call(what = stack, args = parameter_stacks) # Combining parameter stacks into RasterStack
  names(raster_stack) <- names(predicted_reference_climate[[combination]]) # Naming layers
  raster_stack # Return RasterStack
}), nm = info$model_combinations$combination) # Naming list elements by model combinations

output_folder <- "output/task5" # Defining output folder
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE) # Creating output folder
biases <- lapply(X = biases, FUN = readAll) # Ensuring all biases are loaded into RAM
save(biases, file = file.path(output_folder, "biases.RData")) # Saving biases to file

lapply(X = names(biases), FUN = function(combination) {
  combination_folder <- file.path(output_folder, "maps_for_checking", "biases", combination) # Defining output path
  dir.create(combination_folder, showWarnings = FALSE, recursive = TRUE) # Creating folder
  lapply(X = names(biases[[combination]]), FUN = function(layer_name) {
    png_filename <- file.path(combination_folder, paste0(layer_name, ".png")) # Defining PNG file name
    png(filename = png_filename, width = 1500, height = 1600, res = 300) # Opening PNG device
    layer <- biases[[combination]][[layer_name]] # Extracting bias layer
    min_value <- minValue(layer) # Minimum value in the layer
    max_value <- maxValue(layer) # Maximum value in the layer
    if (grepl("Prec", layer_name)) { # Determining color scale based on parameter type
      bias_colors <- create_divergent_palette(min_value, max_value, 1) # For precipitation: center at 1
    } else {
      bias_colors <- create_divergent_palette(min_value, max_value, 0) # For temperature: center at 0
    }
    plot(layer, # Plotting layer
         main = layer_name, 
         col = bias_colors, 
         legend = TRUE, 
         axes = TRUE, 
         colNA = "blue", 
         legend.width = 1.5,
         axis.args = list(at = c(min_value, if (grepl("Prec", layer_name)) 1 else 0, max_value),
                          labels = round(c(min_value, if (grepl("Prec", layer_name)) 1 else 0, max_value), 2)))
    plot(st_geometry(countries), add = TRUE, border = "black", lwd = 0.5) # Adding country borders
    plot(hungary, add = TRUE, border = "darkred", lwd = 1.5) # Adding Hungary border
    dev.off() # Closing PNG device
  }) # lapply for layers
}) # lapply for combinations

bias_corrected_future_climate <- setNames(lapply(X = info$model_combinations$combination, FUN = function(combination) {
  parameter_stacks <- lapply(X = info$climate_parameters, FUN = function(parameter) {
    monthly_layers <- lapply(X = 1:12, FUN = function(month) {
      layer_name <- paste0(parameter, formatC(x = month, width = 2, flag = "0")) # Creating layer name
      future_layer <- predicted_future_climate[[combination]][[layer_name]] # Extracting future layer
      bias_layer <- biases[[combination]][[layer_name]] # Extracting bias layer
      if (info$parameter_types[[parameter]] == "temperature") { 
        future_layer - bias_layer # Subtracting bias for temperature
      } else {
        future_layer / bias_layer # Dividing by bias for precipitation
      } # if
    }) # monthly_layers
    stack(monthly_layers) # Combining monthly layers into RasterStack
  }) # parameter_stacks
  raster_stack <- do.call(what = stack, args = parameter_stacks) # Combining parameter stacks
  names(raster_stack) <- names(predicted_future_climate[[combination]]) # Naming layers
  raster_stack # Return RasterStack
}), nm = info$model_combinations$combination) # Naming list elements

bias_corrected_future_climate <- lapply(X = bias_corrected_future_climate, FUN = readAll) # Loading corrected data into RAM
save(bias_corrected_future_climate, file = file.path(output_folder, "bias_corrected_future_climate.RData")) # Saving data to file

lapply(X = names(bias_corrected_future_climate), FUN = function(combination) {
  combination_folder <- file.path(output_folder, "maps_for_checking", "bias_corrected_future_climate", combination) 
  dir.create(combination_folder, showWarnings = FALSE, recursive = TRUE) # Creating folder for combination
  lapply(X = names(bias_corrected_future_climate[[combination]]), FUN = function(layer_name) {
    png_filename <- file.path(combination_folder, paste0(layer_name, ".png")) # Defining PNG file name
    png(filename = png_filename, width = 1500, height = 1600, res = 300) # Opening PNG device
    plot(bias_corrected_future_climate[[combination]][[layer_name]], # Plotting corrected layers
         main = layer_name, 
         colNA = "blue", # Color for NA values
         axes = TRUE) # Adding axes
    plot(st_geometry(countries), add = TRUE, border = "black", lwd = 0.5) # Adding country borders
    plot(hungary, add = TRUE, border = "darkred", lwd = 1.5) # Adding Hungary border
    dev.off() # Closing PNG device
  }) # lapply for layers
}) # lapply for combinations

unlink(tmp_folder, recursive = TRUE) # Deleting temporary folder
