# Creator: Roman Korolkov
# Date: 25 Feb 2025
# Version: 02
# Description: This script determines the interannual variability based on the bioclimatic variables.
# Duration: Approximately 3 hours
# Saved files size: ~ 255 MB

if (!requireNamespace("sf")) install.packages("sf") # Installing all needed packages
if (!requireNamespace("raster")) install.packages("raster")
if (!requireNamespace("ncdf4")) install.packages("ncdf4")
if (!requireNamespace("future")) install.packages("future")
if (!requireNamespace("future.apply")) install.packages("future.apply")
if (!requireNamespace("dplyr")) install.packages("dplyr")
library(sf)  
library(raster) 
library(ncdf4)
library(future)
library(future.apply)
library(dplyr)

setwd("D:/Climate_Research") # Setting working directory
info_file <- "output/Task1/info.RData" # loading info file
load(file = info_file)

study_extent_sp <- as(object = info$study_extent, Class = "Spatial") # Transforming study extent to spatial for crop() function

cropped_climate <- setNames( # Creating cropped climate list
  object = lapply( # Iterating through climate parameters
    X = info$climate_parameters,
    FUN = function(parameter) {
      observed_abbreviation <- info$observed_abbreviations[ parameter ]  # getting observed abbreviation for the parameter
      netcdf_file <- paste0("input/observed_climate/", observed_abbreviation, "_ens_mean_0.1deg_reg_v24.0e.nc") # relative path to files
      climate_stack <- stack(x = netcdf_file)  # loading NetCDF file as RasterStack
      layer_names <- names(x = climate_stack)
      layer_years <- as.integer(x = substr(x = layer_names, start = 2, stop = 5))  # extracting years from layer names
      keep <- layer_years >= info$reference_period[1] & 
        layer_years <= info$reference_period[2] # subsetting layers within the reference period
      climate_stack <- climate_stack[[ which(keep) ]]
      climate_stack <- crop(x = climate_stack, y = study_extent_sp)  # cropping RasterStack with study extent
      return(climate_stack)  # returning RasterStack
    }
  ),  
  nm = info$climate_parameters  # naming climate parameters
)

plan(strategy = "multisession")  # Enabling multicore processing
options(future.globals.maxSize = Inf)  # Removing memory size limit

reference_years <- seq(from = info$reference_period[1], to = info$reference_period[2])  # vector of reference years
yearly_climate_raster <- setNames( # Creating yearly climate raster list
  object = future_lapply(
    X = reference_years,  # Iterating through each year of reference period
    FUN = function(year) {
      parameter_stacks <- lapply( # Creating list for climate parameters
        X = info$climate_parameters,  # Iterating through each climate parameter
        FUN = function(parameter) {
          climate_stack <- cropped_climate[[ parameter ]] # Subsetting RasterStack to retain only studied year layers
          layer_names <- names(x = climate_stack)
          layer_years <- as.integer(x = substr(x = layer_names, start = 2, stop = 5))
          idx <- which(layer_years == year) # Searching indexes
          year_stack <- climate_stack[[idx]]# Subsetting layers for each year 
          layer_months <- as.integer(x = substr(x = names(x = year_stack), start = 7, stop = 8))  # Extracting months
          monthly_layers <- lapply( # Creating unnamed list to process each month
            X = 1:12,  # Iterating through months 1 to 12
            FUN = function(month) {
              month_indices <- which(layer_months == month)  # Finding layers for current month
              agg_fun <- if (info$parameter_types[ parameter ] == "temperature") mean else sum  # Selecting aggregation function
              month_layer <- calc(x = year_stack[[ month_indices ]], fun = agg_fun, na.rm = FALSE)  # Aggregating cellwise
              return(month_layer)  # Returning aggregated RasterLayer
            } 
          )
          monthly_stack <- stack(x = monthly_layers)  # Creating monthly RasterStack
          return(monthly_stack)  # Returning monthly RasterStack
        }
      )
      combined_stack <- stack(x = parameter_stacks) # Combining 4 RasterStacks
      new_names <- unlist(lapply(X = info$climate_parameters, FUN =  function(p) {
        paste0(p, formatC(x = 1:12, width = 2, flag = "0"))
      })) # Creating month names
      names(x = combined_stack) <- new_names # Assigning layer names to RasterStack
      return(combined_stack)  # Returning RasterStack
    }
  ),
  nm = as.character(x = reference_years)  # Setting names
)

output_folder <- "output/Task8"  # Defining output folder
dir.create(path = output_folder, showWarnings = FALSE, recursive = TRUE)  # Creating folder
yearly_climate_raster <- lapply(
  X = yearly_climate_raster,
  FUN = readAll # Loading data into memory
)
file_name <- paste0(output_folder, "/yearly_climate_raster.RData")  # Defining output file name
save(yearly_climate_raster, file = file_name)  # Saving yearly climate raster file

yearly_climate_points <- setNames( # Creating yearly climate points list
  object = future_lapply(
    X = reference_years,  # Iterating through each reference year
    FUN = function(year) {
      raster_points <- rasterToPoints(x = yearly_climate_raster[[ as.character(year) ]], spatial = TRUE)  # Converting RasterStack to points
      points_sf <- st_as_sf(x = raster_points)  # Converting to sf object
      return(points_sf)  # Returning sf object
    }
  ),
  nm = as.character(x = reference_years)  # Setting names
)

file_name <- paste0(output_folder, "/yearly_climate_points.RData")  # Defining output file name
save(yearly_climate_points, file = file_name)  # Saving yearly climate points file

points <- st_as_sf(x = yearly_climate_points[[1]][, 0])  # Copying geometry only from first year's points

yearly_specific_climate_periods <- setNames( # Creating yearly specific climate periods file
  object = future_lapply(
    X = reference_years,  # Iterating through each reference year
    FUN = function(year) {
      specific_periods <- points  # Copying points
      year_points <- yearly_climate_points[[ as.character(year) ]]  # Getting sf points with data for year
      col_names <- names(x = year_points) # Getting the column names
      prec_cols  <- grep(pattern = "^Prec", x = col_names) # Columns for precipitation
      tmean_cols <- grep(pattern = "^Tmean", x = col_names) # Columns for Tmean
      precip_data <- as.matrix(x = st_drop_geometry(year_points)[, prec_cols])  # Precipitation data matrix
      tmean_data <- as.matrix(x = st_drop_geometry(year_points)[, tmean_cols])  # Tmean data matrix
      specific_periods$wettestM <- apply(X = precip_data, MARGIN = 1, FUN = which.max)  # Determining wettest month
      specific_periods$driestM <- apply(X = precip_data, MARGIN = 1, FUN = which.min)  # Determining driest month
      specific_periods$warmestM <- apply(X = tmean_data, MARGIN = 1, FUN = which.max)  # Determining warmest month
      specific_periods$coldestM <- apply(X = tmean_data, MARGIN = 1, FUN = which.min)  # Determining coldest month
      temperature_of_quarters <- as.data.frame( # Creating dataframe for temperature of quarters
        x = t(apply(X = tmean_data, MARGIN = 1, FUN = function(x) {
          sapply(X = 1:12, FUN = function(i) mean(x[c(i, (i %% 12) + 1, ((i + 1) %% 12) + 1)], na.rm = TRUE)) # Calculating quarter averages
        }))
      )
      precipitation_of_quarters <- as.data.frame( # Creating dataframe for precipitation of quarters
        x = t(apply(X = precip_data, MARGIN = 1, FUN = function(x) {
          sapply(X = 1:12, FUN = function(i) sum(x[c(i, (i %% 12) + 1, ((i + 1) %% 12) + 1)], na.rm = TRUE)) # Calculating quarter sums
        }))
      )
      specific_periods$wettestQ <- apply(X = precipitation_of_quarters, MARGIN = 1, FUN = which.max)  # Determining wettest quarter
      specific_periods$driestQ <- apply(X = precipitation_of_quarters, MARGIN = 1, FUN = which.min)  # Determining driest quarter
      specific_periods$warmestQ <- apply(X = temperature_of_quarters, MARGIN = 1, FUN = which.max)  # Determining warmest quarter
      specific_periods$coldestQ <- apply(X = temperature_of_quarters, MARGIN = 1, FUN = which.min)  # Determining coldest quarter
      specific_periods <- specific_periods[, info$climate_periods_abbr]  # Reordering columns
      return(specific_periods)  # Returning sf object
    }
  ),
  nm = as.character(x = reference_years)  # Setting names
)

file_name <- paste0(output_folder, "/yearly_specific_climate_periods.RData")  # Defining output file name
save(yearly_specific_climate_periods, file = file_name)  # Saving file

yearly_bioclimatic_variables <- setNames( # Creating yearly bioclimatic variables list
  object = future_lapply(
    X = reference_years,  # Iterating through each reference year
    FUN = function(year) {
      bioclim_sf <- points  # Copying points
      year_points <- yearly_climate_points[[ as.character(year) ]]  # Getting sf points with data
      Tmean <- st_drop_geometry(year_points) %>% dplyr::select(matches = starts_with(match = "Tmean")) %>% as.matrix() # Extracting mean temperature as matrix
      Tmax <- st_drop_geometry(year_points) %>% dplyr::select(matches = starts_with(match = "Tmax")) %>% as.matrix() # Extracting max temperature as matrix
      Tmin <- st_drop_geometry(year_points) %>% dplyr::select(matches = starts_with(match = "Tmin")) %>% as.matrix() # Extracting min temperature as matrix
      Prec <- st_drop_geometry(year_points) %>% dplyr::select(matches = starts_with(match = "Prec")) %>% as.matrix() # Extracting precipitation as matrix
      data <- st_drop_geometry(yearly_specific_climate_periods[[ as.character(year) ]]) # Using previously calculated data
      wettestQ <- (data$wettestQ) # Applying values to quarters and periods
      driestQ <- (data$driestQ)
      warmestQ <- (data$warmestQ)
      coldestQ <- (data$coldestQ)
      wettestM <- (data$wettestM)
      driestM <- (data$driestM)
      warmestM <- (data$warmestM)
      coldestM <- (data$coldestM)
      bioclim_sf <- bioclim_sf %>% # Adding new columns to sf object
        mutate(
          bcv1 = rowMeans(x = Tmean, na.rm = TRUE), # Calculating mean temperature
          bcv2 = rowMeans(x = Tmax - Tmin, na.rm = TRUE), # Calculating mean diurnal range
          bcv4 = 100 * apply(X = Tmean, MARGIN = 1,FUN = function(x) sqrt(mean((x - mean(x, na.rm = TRUE)) ^ 2, na.rm = TRUE))), # Calculating temperature seasonality
          bcv5 = bcv5 <- Tmax[cbind(seq_len(nrow(Tmax)), warmestM)], # Calculating Tmax of the warmest month
          bcv6 = bcv6 <- Tmin[cbind(seq_len(nrow(Tmin)), coldestM)], # Calculating Tmin of the coldest month
          bcv7 = bcv5 - bcv6, # Calculating temperature annual range
          bcv3 = bcv2 / bcv7 * 100, # Calculating diurnal range proportion
          bcv8 = rowMeans(x = t(sapply(X = seq_len(nrow(x = Tmean)), FUN = function(i) {Tmean[i, c(wettestQ[i], (wettestQ[i] %% 12) + 1, ((wettestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Mean temperature of wettest quarter
          bcv9 = rowMeans(x = t(sapply(X = seq_len(nrow(x = Tmean)), FUN = function(i) {Tmean[i, c(driestQ[i], (driestQ[i] %% 12) + 1, ((driestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Mean temperature of driest quarter
          bcv10 = rowMeans(x = t(sapply(X = seq_len(nrow(x = Tmean)), FUN = function(i) {Tmean[i, c(warmestQ[i], (warmestQ[i] %% 12) + 1, ((warmestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Mean temperature of warmest quarter
          bcv11 = rowMeans(x = t(sapply(X = seq_len(nrow(x = Tmean)), FUN = function(i) {Tmean[i, c(coldestQ[i], (coldestQ[i] %% 12) + 1, ((coldestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Mean temperature of coldest quarter
          bcv12 = rowSums(x = Prec, na.rm = TRUE), # Annual precipitation
          bcv13 = Prec[cbind(seq_len(nrow(Prec)), wettestM)], # Max monthly precipitation
          bcv14 = Prec[cbind(seq_len(nrow(Prec)), driestM)], # Min monthly precipitation
          bcv15 = apply(X = Prec, MARGIN = 1, FUN = function(x) sqrt(mean((x - mean(x, na.rm = TRUE)) ^ 2, na.rm = TRUE))) / rowMeans(x = Prec, na.rm = TRUE), # Precipitation seasonality
          bcv16 = rowSums(x = t(sapply(X = seq_len(nrow(x = Prec)), FUN = function(i) {Prec[i, c(wettestQ[i], (wettestQ[i] %% 12) + 1, ((wettestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Precipitation of wettest quarter
          bcv17 = rowSums(x = t(sapply(X = seq_len(nrow(x = Prec)), FUN = function(i) {Prec[i, c(driestQ[i], (driestQ[i] %% 12) + 1, ((driestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Precipitation of driest quarter
          bcv18 = rowSums(x = t(sapply(X = seq_len(nrow(x = Prec)), FUN = function(i) {Prec[i, c(warmestQ[i], (warmestQ[i] %% 12) + 1, ((warmestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE), # Precipitation of warmest quarter
          bcv19 = rowSums(x = t(sapply(X = seq_len(nrow(x = Prec)), FUN = function(i) {Prec[i, c(coldestQ[i], (coldestQ[i] %% 12) + 1, ((coldestQ[i] + 1) %% 12) + 1)]})), na.rm = TRUE) # Precipitation of coldest quarter
        )
      bioclim_sf <- bioclim_sf[, info$bioclim_variables_abbr] # Reordering columns
      return(bioclim_sf) # Returning sf object
    }
  ),
  nm = as.character(x = reference_years) # Setting names
)

file_name <- paste0(output_folder, "/yearly_bioclimatic_variables.RData") # Defining output file
save(yearly_bioclimatic_variables, file = file_name) # Saving file

point_ids <- as.character(x = seq_len(nrow(x = points))) # Creating point IDs

interannual_variability <- setNames( # Creating interannual variability list
  object = future_lapply(
    X = point_ids,  # Iterating through each point ID
    FUN = function(point_id) {
      BCVs <- do.call( # Creating data frame with bioclimatic variables
        what = rbind,
        args = lapply(
          X = reference_years,
          FUN = function(year) {
            year_data <- yearly_bioclimatic_variables[[ as.character(year) ]]  # Getting sf object for year
            year_data_df <- st_drop_geometry(x = year_data)
            point_data <- year_data_df[as.integer(x = point_id), info$bioclim_variables_abbr, drop = FALSE]  # Extracting bioclimatic variables for point
            point_data <- as.data.frame(x = point_data)
            return(point_data) # Returning data frame row
          }
        )
      )
      sd_of_BCVs <- apply(X = BCVs, MARGIN = 2, FUN = sd, na.rm = FALSE)  # Calculating standard deviations
      standardized_BCVs <- sweep(x = BCVs, MARGIN = 2, STATS = sd_of_BCVs, FUN = "/")  # Standardizing BCVs
      PCA <- prcomp(x = standardized_BCVs)  # Calculating PCA
      no_of_important_PCs <- sum(PCA$sdev > 0.1)  # Counting important PCs
      sd_of_important_PCs <- PCA$sdev[PCA$sdev > 0.1]  # Extracting sdev of important PCs
      return(list( 
        sd_of_BCVs = sd_of_BCVs,  # Interannual SD of BCVs
        PCA = PCA,  # PCA result
        no_of_important_PCs = no_of_important_PCs,  # Number of important PCs
        sd_of_important_PCs = sd_of_important_PCs  # Sdev of important PCs
      ))
    }
  ),
  nm = point_ids  # Setting names
)

file_name <- paste0(output_folder, "/interannual_variability.RData")  # Defining output file
save(interannual_variability, file = file_name)  # Saving interannual variability file
file_name <- paste0(output_folder, "/point_ids.RData")  # Defining output file
save(point_ids, file = file_name)  # Saving point ids file
plan(strategy = "sequential")  # Reverting processing plan to sequential
writeLines("Script execution completed.") # Completion message