# Creator: Roman Korolkov
# Date: 23 Nov 2024
# Version: 04
# Description: This script processes observed climate data for Hungary and calculates long-term climate parameters.
# Duration: Approximately 2,5 hours
# Saved files size: ~5.1 MB

if (!requireNamespace("sf")) install.packages("sf")
if (!requireNamespace("raster")) install.packages("raster")
if (!requireNamespace("ncdf4")) install.packages("ncdf4")
library(sf)
library(raster)
library(ncdf4)#loading packages

setwd("D:/Climate_Research") # setting working directory
info_file <- "output/Task1/info.RData" # path to info file
load(file = info_file) # loading the info list

file_name <- "input/countries/CNTR_RG_01M_2020_4326.shp" # path to shapefile
countries <- st_read(file_name) # loading shapefile
hungary <- st_geometry(countries[countries$CNTR_NAME == "Magyarország", ]) # extracting Hungary geometry
study_extent_geom <- st_as_sf(x = as(x = info$study_extent, Class = "Spatial")) # converting study extent to sf object

climate_parameter_stacks <- lapply(X = info$climate_parameters, FUN = function(parameter) { # processing observed climate data
  netcdf_file <- paste0("input/observed_climate/", info$observed_abbreviations[[parameter]], "_ens_mean_0.1deg_reg_v24.0e.nc") # creating file name for the NetCDF file
  if (!file.exists(netcdf_file)) { # checking if file exists
    stop(paste("File not found:", netcdf_file)) # stopping execution if file is not found
    }
  raster_stack <- stack(netcdf_file) # loading NetCDF file as Rasterstack
  years <- as.integer(substr(x = names(raster_stack), start = 2, stop = 5)) # getting years form layer names
  reference_stack <- raster_stack[[which(years >= info$reference_period[1] & years <= info$reference_period[2])]] # subset layers for ref. period
  if (nlayers(reference_stack) == 0) {
    stop(paste("No valid layers found for reference period for parameter:", parameter))
    } # checking if reference stack has valid layers
  cropped_stack <- crop(x = reference_stack, y = as(info$study_extent, "Spatial")) # cropping raster
  if (nlayers(cropped_stack) == 0) {
    stop(paste("No valid layers found after cropping for parameter:", parameter))
    } # checking if cropped stack has valid layers
  months <- as.integer(substr(x = names(cropped_stack), start = 7, stop = 8)) # getting months from layer names
  monthly_layers <- lapply(X = 1:12, FUN = function(month) { # creating unnamed 12-elements list
    monthly_layer <- cropped_stack[[which(months == month)]] # selecting layers for this month
    if (nlayers(monthly_layer) == 0) {
      stop(paste("No valid layers found for month", month, "for parameter:", parameter))
      } # checking if monthly layer has valid layers
    if (info$parameter_types[[parameter]] == "temperature") {
      return(calc(x = monthly_layer, fun = mean)) # calculating long-term mean for temperature
      } else {
        return(calc(x = monthly_layer, fun = function(x) sum(x) / 30)) # calculating long-term sum for precipitation
      }
    }
    ) # ending lapply for months
  monthly_stack <- stack(x = monthly_layers)
  return(monthly_stack) # creating a RasterStack from the monthly layers
}) # ending lapply for climate parameters 

observed_climate <- stack(climate_parameter_stacks) # creating 48-layers RasterStack
parameters <- info$climate_parameters # creating climate parameters
months <- formatC(x = 1:12, width = 2, flag = "0") # formatting months 
layer_names <- unlist(lapply(X = parameters, FUN = function(param) {
  paste0(param, months)
}))
names(observed_climate) <- layer_names # setting layers names
empty_raster <- raster(observed_climate)# creating empty raster
observed_climate <- readAll(observed_climate) # loading data to RAM
empty_raster <- readAll(empty_raster) # loading empty raster into RAM
output_folder <- "output/Task2" # setting output folder
if (!dir.exists(paths = output_folder)) dir.create(path = output_folder, recursive = TRUE) # creating subfolder Task 2
file_name_observed <- paste0(output_folder, "/observed_climate.RData") # path for observed climate
file_name_empty <- paste0(output_folder, "/empty_raster.RData") # path for empty raster
save(observed_climate, file = file_name_observed) # saving observed_climate
save(empty_raster, file = file_name_empty) # saving empty_raster

maps_folder <- paste0(output_folder, "/maps_for_checking") # setting folder for maps
if (!dir.exists(paths = maps_folder)) {
  dir.create(path = maps_folder, recursive = TRUE) # creating subfolder for maps
}
lapply(X = 1:nlayers(x = observed_climate), FUN = function(layer_index) { # iterating through layers of observed climate
  layer_name <- names(observed_climate)[layer_index] # getting layer name
  file_name <- paste0(maps_folder, "/", layer_name, ".png") # file path for PNG
  climate_extent <- extent(observed_climate) # setting extent
  png(filename = file_name, width = 1500, height = 1600, res = 300) # creating PNG
  plot( 
    x = observed_climate[[layer_index]],
    main = layer_name,
    axes = TRUE,
    colNA = "blue"
  ) # plotting observed climate
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
  dev.off() # finishing PNG
  }
) # ending of lapply for maps