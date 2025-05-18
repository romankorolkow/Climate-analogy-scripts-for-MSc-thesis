# Creator: Roman Korolkov
# Date: 22 Dec 2024
# Version: 04
# Description: The script processes predicted climate data of the reference period.
# Duration: Approximately 2 hours
# Saved files size: ~60 MB

if (!requireNamespace("raster")) install.packages("raster") # Installing raster package
if (!requireNamespace("ncdf4")) install.packages("ncdf4") # Installing ncdf4 package
if (!requireNamespace("sf")) install.packages("sf") # Installing sf package
library(raster) # Loading raster package
library(ncdf4) # Loading ncdf4 package
library(sf) # Loading sf package

setwd("D:/Climate_Research") # Setting working directory
tmp_folder <- "D:/Climate_Research/temp_raster" 
dir.create(tmp_folder, showWarnings = FALSE, recursive = TRUE) # Creating a temporary folder for raster processing
rasterOptions(tmpdir = tmp_folder) # Setting raster package to use temp folder
info_file <- "output/Task1/info.RData" # Path to info file
load(file = info_file) # Loading info object
empty_raster_file <- "output/Task2/empty_raster.RData" # Path to empty raster file
load(file = empty_raster_file)  # Loading the empty raster object
model_combinations <- as.character(info$model_combinations$combination) # Extracting model combinations
reference_folder <- "input/predicted_climate/reference_period/" # Defining path to reference climate data
files_in_folder <- list.files(path = reference_folder, full.names = TRUE) # List all files in reference folder

file_name <- "input/countries/CNTR_RG_01M_2020_4326.shp" # path to shapefile
countries <- st_read(file_name) # loading shapefile
hungary <- st_geometry(countries[countries$CNTR_NAME == "Magyarország", ]) # extracting Hungary geometry
study_extent_geom <- st_as_sf(info$study_extent) # converting study extent to sf object

predicted_reference_climate <- setNames(lapply( # Processing predicted reference climate data
  X = model_combinations,
  FUN = function(combination) {
    global_model <- as.character(info$model_combinations$global_model[info$model_combinations$combination == combination]) # Getting global model name
    regional_model <- as.character(info$model_combinations$regional_model[info$model_combinations$combination == combination]) # Getting regional model name
    global_institute <- info$developer_institutes[global_model] # Getting the global model institute
    regional_developer <- info$regional_developers[regional_model] # Getting regional developer information
    model_version <- info$model_combinations$version[info$model_combinations$combination == combination] # Extracting model version
    reference_start_year <- info$reference_start_year[[regional_model]] # Getting reference start year
    reference_end_year <- info$reference_end_year[[regional_model]] # Getting reference end year
    climate_parameter_stacks <- lapply( # Iterating through climate parameters
      X = info$climate_parameters,
      FUN = function(parameter) {
        parameter_abbreviation <- info$predicted_abbreviations[[parameter]] # Getting abbreviation for the parameter
        relevant_files <- grep(
          pattern = paste0(parameter_abbreviation, "_EUR-11_", global_institute, "-", global_model, 
                           "_historical_r1i1p1_", regional_developer, "-", regional_model, 
                           "_v", model_version, "_mon_"), # Finding relevant files for each parameter
          x = files_in_folder,
          value = TRUE
        )
        period_stacks <- lapply(
          X = relevant_files,
          FUN = function(netcdf_file) {
            raster_stack <- stack(netcdf_file) # Reading and stacking all relevant files
            return(raster_stack)
          }
        )
        combined_stack <- do.call(what = stack, args = period_stacks) # Combining all raster stacks into one
        names(combined_stack) <- gsub(pattern = "_day[0-9]+", replacement = "", x = names(combined_stack)) # Cleaning layer names
        days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) # Number of days in each month
        monthly_layers <- lapply( # Extracting monthly data and processing each month
          X = 1:12,
          FUN = function(month) {
            layer_years <- as.integer(substr(x = names(combined_stack), start = 2, stop = 5)) # Extracting years from layer names 
            layer_months <- as.integer(substr(x = names(combined_stack), start = 7, stop = 8)) # Extracting months from layer names
            valid_layers <- which( # Filtering layers based on month and reference period
              layer_months == month &  # Filtering by months
                layer_years >= info$reference_period[1] & # Filtering by start of reference period
                layer_years <= info$reference_period[2] # Filtering by end of reference period
            )
            month_layers <- combined_stack[[valid_layers]] # Extracting layers based on criteria
            monthly_data <- calc(x = month_layers, fun = mean, na.rm = TRUE) # Calculating mean for each month
            if (info$parameter_types[[parameter]] == "precipitation") {
              monthly_data <- calc(x = monthly_data, fun = function(x) x * 86400 * days_in_month[month]) # Converting from mm/s to mm/month
            } else if (info$parameter_types[[parameter]] == "temperature") {
              monthly_data <- calc(x = monthly_data, fun = function(x) x - 273.15) # Converting from Kelvin to Celsius
            }
            return(monthly_data)
          }
        )
        monthly_stack <- stack(monthly_layers) # Combining monthly layers into a RasterStack
        return(monthly_stack)
      }
    )
    predicted_climate <- do.call(what = stack, args = climate_parameter_stacks) # Combining climate parameters into a single RasterStack
    parameters <- info$climate_parameters # Defining parameter names
    months <- formatC(x = 1:12, width = 2, flag = "0") # Defining month names
    layer_names <- as.vector(outer(X = months, Y = parameters, FUN = function(month, parameter) paste0(parameter, month)))
    apply(X = expand.grid(months, parameters)[, 2:1], MARGIN = 1, FUN = paste, collapse = "") # Generating layer names
    names(predicted_climate) <- layer_names # Assigning names to layers
    crs(predicted_climate) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Assigning CRS to raster
    projected_raster <- projectRaster(from = predicted_climate, crs = "+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +to_meter=0.01745329") # Reprojecting raster
    crs(projected_raster) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Setting CRS to WGS84
    cropped_raster <- crop(x = projected_raster, y = extent(empty_raster)) # Cropping raster
    resampled_raster <- projectRaster(from = cropped_raster, to = empty_raster, method = "ngb") # Resampling raster with nearest neighbor method
    observed_climate_file <- "output/Task2/observed_climate.RData" # Path to observed climate file
    load(file = observed_climate_file)  # Loading observed climate object
    masked_raster <- mask(x = resampled_raster, mask = observed_climate[[1]]) # Masking raster
    return(masked_raster)
  }
), nm = model_combinations) # Applying function to each model combination

output_folder <- "output/Task3" # Defining output folder path
if (!dir.exists(paths = output_folder)) dir.create(path = output_folder, recursive = TRUE) # Creating output folder
for (i in seq_along(predicted_reference_climate)) {
  predicted_reference_climate[[i]] <- readAll(predicted_reference_climate[[i]]) # Ensuring all data is read into memory
}
save_file <- paste0(output_folder, "/predicted_reference_climate.RData") # Defining output file path
save(predicted_reference_climate, file = save_file) # Saving processed climate data

lapply(
  X = model_combinations,
  FUN = function(combination) {
    combination_folder <- paste0(output_folder, "/maps_for_checking/", combination) # Defining folder for maps
    if (!dir.exists(paths = combination_folder)) {
      dir.create(path = combination_folder, recursive = TRUE) # Creating folder
    }
    lapply(
      X = names(predicted_reference_climate[[combination]]), # Creating maps for checking
      FUN = function(layer_name) {
        x = predicted_reference_climate[[combination]][[layer_name]] # Getting layer name
        png_filename <- paste0(combination_folder, "/", layer_name, ".png") # Defining PNG file path
        png(filename = png_filename, width = 1500, height = 1600, res = 300) # Opening PNG device
        plot(
          x = predicted_reference_climate[[combination]][[layer_name]], # Plotting the layer
          main = layer_name, # Setting title
          axes = TRUE, # Drawing axes
          colNA = "blue" # Setting color
        )
        plot(
          x = st_geometry(countries),
          add = TRUE,
          border = "black",
          lwd = 0.5
        ) # plotting countries geometry
        plot(
          x = hungary, 
          add = TRUE, 
          border = "darkred", 
          lwd = 1.5, 
          lty = 1
        ) # plotting Hungary
        dev.off()
      }
    )
  }
)
unlink(tmp_folder, recursive = TRUE) # Deleting temporary folder