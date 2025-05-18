# Creator: Roman Korolkov
# Date: 18 Dec 2024
# Version: 02
# Description: The script converts rasters to simple features.
# Duration: Approximately 2 hours
# Saved files size: ~25 MB

if (!requireNamespace("raster")) install.packages("raster") # Installing raster package
if (!requireNamespace("sf")) install.packages("sf") # Installing sf package
library(raster) # Loading raster package
library(sf) # Loading sf package

setwd("D:/Climate_Research") # Setting working directory
info_file <- "output/Task1/info.RData" # Path to info file
empty_raster_file <- "output/Task2/empty_raster.RData" # Path to empty raster file
hungary_file <- "output/Task1/hungary.RData" # Path to Hungary file
observed_climate_file <- "output/Task2/observed_climate.RData" # Path to observed climate file
bias_corrected_future_climate_file <- "output/Task5/bias_corrected_future_climate.RData" # Path to bias corrected future climate file
load(file = info_file) # Loading info object
load(file = empty_raster_file) # Loading empty raster object
load(file = hungary_file) # Loading Hungary geometry
load(file = observed_climate_file) # Loading observed climate
load(file = bias_corrected_future_climate_file) # Loading bias-corrected future climate
countries <- st_read("input/countries/CNTR_RG_01M_2020_4326.shp") # Loading shapefile

monthly_climate <- setNames( # Creating monthly climate list with sf objects
  lapply(X = info$climate_targets, FUN = function(target) {
    if (target == "reference") {
      st_as_sf(x = rasterToPoints(x = observed_climate, spatial = TRUE)) # Reference climate
    } else {
      st_as_sf(x = rasterToPoints(x = bias_corrected_future_climate[[target]], spatial = TRUE)) # Future climate
    }
  }),
  nm = info$climate_targets
)

terrestrial_mask <- !is.na(values(x = observed_climate[[1]])) # Creating mask for terrestrial cells with TRUE for land and FALSE for sea
terrestrial_raster <- empty_raster # Copying empty raster
values(x = empty_raster) <- NA # Filling empty raster with NA values
values(x = empty_raster)[terrestrial_mask] <- 1 # Assigning 1 to terrestrial cell
terrestrial_cell_IDs <- which(x = terrestrial_mask) # Extracting IDs for terrestrial cells
all_points <- st_geometry(monthly_climate$reference) # Extracting geometry from the reference target
mask_of_hungarian_points <- st_within(x = all_points, y = hungary, sparse = FALSE)[, 1] # Subsetting the first column
mask_of_foreign_points <- !mask_of_hungarian_points # Creating mask for foreign points
coordinates <- st_coordinates(x = all_points) # Extracting coordinates matrix
hungarian_cell_IDs <- cellFromXY(object = observed_climate[[1]], xy = coordinates[mask_of_hungarian_points, ]) # Hungarian cells
foreign_cell_IDs <- cellFromXY(object = observed_climate[[1]], xy = coordinates[mask_of_foreign_points, ]) # Foreign cells

output_folder <- "output/Task6" # Creating output folder
dir.create(path = output_folder, showWarnings = FALSE, recursive = TRUE)
save(object = terrestrial_mask, file = file.path(output_folder, "terrestrial_mask.RData")) # Saving all created objects
save(object = terrestrial_raster, file = file.path(output_folder, "terrestrial_raster.RData"))
save(object = terrestrial_cell_IDs, file = file.path(output_folder, "terrestrial_cell_IDs.RData"))
save(object = mask_of_hungarian_points, file = file.path(output_folder, "mask_of_hungarian_points.RData"))
save(object = mask_of_foreign_points, file = file.path(output_folder, "mask_of_foreign_points.RData"))
save(object = coordinates, file = file.path(output_folder, "coordinates.RData"))
save(object = hungarian_cell_IDs, file = file.path(output_folder, "hungarian_cell_IDs.RData"))
save(object = foreign_cell_IDs, file = file.path(output_folder, "foreign_cell_IDs.RData"))
save(object = monthly_climate, file = file.path(output_folder, "monthly_climate.RData"))

lapply(X = names(monthly_climate), FUN = function(target) { # Creating maps for checking
  target_folder <- file.path(output_folder, "maps_for_checking", target) # Creating subfolders for climate targets
  dir.create(target_folder, showWarnings = FALSE, recursive = TRUE)
  lapply(X = names(monthly_climate[[target]])[!names(monthly_climate[[target]]) %in% "geometry"], FUN = function(column_name) {
    png_filename <- file.path(target_folder, paste0(column_name, ".png")) # Defining file name
    png(filename = png_filename, width = 1500, height = 1600, res = 300) # Opening PNG device
    plot(x = monthly_climate[[target]][, column_name], # Plotting sf objects with column data
         pch = 15, # Filled square
         cex = 0.6, # Adjusted size
         main = column_name, # Adding title
         axes = TRUE, # Adding axes
         graticule = TRUE, # Adding graticule
         key.pos = 4, # Adding legend
         reset = FALSE) # Keeping settings for additional plots
    plot(x = st_geometry(countries), add = TRUE, border = "black", lwd = 0.5) # Adding country borders
    plot(x = hungary, add = TRUE, border = "darkred", lwd = 1.5) # Adding Hungary boundary
    dev.off() # Closing device
  })
})
