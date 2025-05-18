# Creator: Roman Korolkov
# Date: 03 Nov 2024
# Version: 04
# Description: This script loads country shapefiles, extracts Hungary geometry, processes climate models, and creates a map.
# Duration: Approximately 5 minutes
# Saved files size: ~15 MB

# Loading necessary libraries
if (!requireNamespace("sf")) install.packages("sf")
if (!requireNamespace("prettymapr")) install.packages("prettymapr")
library(sf)
library(prettymapr)
# Setting the working directory
setwd("D:/Climate_Research")

if (!dir.exists("output/Task1")) {
  dir.create("output/Task1", recursive = TRUE) # creating output folder
}

file_name <- "input/countries/CNTR_RG_01M_2020_4326.shp" # path to shapefile
countries <- st_read(file_name) # loading shapefile
hungary <- st_geometry(countries[countries$CNTR_NAME == "Magyarország", ]) # extracting Hungary geometry
file_name <- "output/Task1/hungary.RData" # file name
save(hungary, file = file_name) # saving file
info <- list() # creating empty list
info$global_climate_models <- c("CNRM-CM5", "EC-EARTH", "IPSL-CM5A-MR", "NorESM1-M") # adding global climate models
info$developer_institutes <- setNames(object = c("CNRM-CERFACS", "ICHEC", "IPSL", "NCC"), nm = info$global_climate_models) # developer institutes for GCMs
info$regional_climate_models <- c("RACMO22E", "RCA4") # regional climate models
info$regional_developers <- setNames(object = c("KNMI", "SMHI"), nm = info$regional_climate_models) # developer institutes for RCMs
model_combinations <- expand.grid(
  global_model = info$global_climate_models, 
  regional_model = info$regional_climate_models
) # creating model combinations
model_combinations$combination <- paste(model_combinations$global_model, model_combinations$regional_model, sep = "_") # adding combination names
model_combinations$version <- ifelse(test = model_combinations$combination == "CNRM-CM5_RACMO22E", yes = 2, no = 1) # adding combination versions
info$model_combinations <- model_combinations # adding combinations to the list
info$climate_targets <- c("reference", model_combinations$combination) # adding climate targets
info$climate_parameters <- c("Prec", "Tmean", "Tmin", "Tmax") # adding climate parameters
info$observed_abbreviations <- setNames(object = c("rr", "tg", "tn", "tx"), nm = info$climate_parameters) # abbreviations for observed parameters
info$predicted_abbreviations <- setNames(object = c("pr", "tas", "tasmin", "tasmax"), nm = info$climate_parameters) # abbreviations for predicted parameters
info$parameter_types <- setNames(object = c("precipitation", rep("temperature", 3)), nm = info$climate_parameters) # adding data types
info$reference_start_year <- setNames(object = c(1950, 1970), nm = info$regional_climate_models) # adding starting years or reference datasets
info$reference_end_year <- setNames(object = c(2005, 2005), nm = info$regional_climate_models) # adding end year of reference datasets
info$future_start_year <- 2006 # adding starting year of future datasets
info$future_end_year <- 2100 # adding end year of future datasets
info$reference_period <- c(1971, 2000) # adding reference period
info$future_period <- c(2071, 2100) # adding future period
info$longitude_range <- c(12, 24.3) # adding longitudinal range
info$latitude_range <- c(45.6, 54.9) # adding latitudinal range
bbox <- st_bbox(c(xmin = info$longitude_range[1], xmax = info$longitude_range[2], 
                  ymin = info$latitude_range[1], ymax = info$latitude_range[2]), crs = st_crs(4326)) # adding study extent
study_extent <- st_as_sfc(bbox) # creating sfc object
info$study_extent <- study_extent # adding study extent to the list
info$climate_periods_abbr <- c("wettestQ", "driestQ", "warmestQ", "coldestQ", 
                               "wettestM", "driestM", "warmestM", "coldestM") # adding abbreviations of climate periods
info$climate_periods_full <- setNames(object = c("Wettest Quarter", "Driest Quarter", "Warmest Quarter", "Coldest Quarter", 
                                                 "Wettest Month", "Driest Month", "Warmest Month", "Coldest Month"), 
                                      nm = info$climate_periods_abbr) # adding full name of specific climate periods
info$bioclim_variables_abbr <- paste0("bcv", 1:19) # adding abbreviations of climatic variables
info$bioclim_variables_full <- setNames(object = c("annual mean temperature", "mean diurnal range", "isothermality", 
                                                   "temperature seasonality", "maximum temperature of warmest month", 
                                                   "minimum temperature of coldest month", "temperature annual range", 
                                                   "mean temperature of wettest quarter", "mean temperature of driest quarter", 
                                                   "mean temperature of warmest quarter", "mean temperature of coldest quarter", 
                                                   "annual precipitation", "precipitation of wettest month", 
                                                   "precipitation of driest month", "precipitation seasonality", 
                                                   "precipitation of wettest quarter", "precipitation of driest quarter", 
                                                   "precipitation of warmest quarter", "precipitation of coldest quarter"), 
                                        nm = info$bioclim_variables_abbr) # adding full names of bioclimatic variables
file_name <- "output/Task1/info.RData" # file name
save(info, file = file_name) # saving the file

png(filename = "output/Task1/map_for_checking.png", width = 1200, height = 900, res = 150) # creating png file
plot(
  x = st_geometry(countries), # adding countries
  col = "lightgray", 
  border = "black", 
  xlim = c(10, 26), 
  ylim = c(44, 56),
  axes = TRUE, # adding axes
  graticule = TRUE    # adding graticule
)
plot(x = st_geometry(hungary), add = TRUE, border = "blue", lwd = 2, col = NA) # plot Hungary
plot(st_geometry(study_extent), add = TRUE, border = "red", lwd = 2, col = NA, lty = 2) # plot study extent
graticule <- st_graticule(x = par("usr")[c(1, 3, 2, 4)], crs = 4326) # creating graticule
plot(x = st_geometry(graticule), add = TRUE, col = "gray", lty = 3) # adding graticule
prettymapr::addscalebar() # adding scale bar # adding scale bar
prettymapr::addnortharrow() # adding north arrow
legend(
  "bottomright", # Position of the legend
  legend = c("Research Area", "Hungary"), # Legend labels
  col = c("red", "blue"), # Colors for the borders
  lty = c("dashed", "solid"), # Line types for borders
  lwd = 2, # Line width
  cex = 0.8, # Text size
  bg = "white" # Background color of the legend box
)
dev.off() # finishing png