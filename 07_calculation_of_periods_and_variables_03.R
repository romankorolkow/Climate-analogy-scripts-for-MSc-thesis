# Creator: Roman Korolkov
# Date: 21 Jan 2025
# Version: 03
# Description: The script calculates specific climate periods and bioclimatic variables and generates composite maps 
# Duration: Approximately 30 minutes
# Saved files size: ~ 42 MB

if (!requireNamespace("sf")) install.packages("sf") # Installing all needed packages 
if (!requireNamespace("gridExtra")) install.packages("gridExtra")
if (!requireNamespace("ggplotify")) install.packages("ggplotify")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("paletteer")) install.packages("paletteer")
if (!requireNamespace("ggthemes")) install.packages("ggthemes")
library(sf)
library(gridExtra)
library(grid)
library(ggplotify)
library(ggplot2)
library(dplyr)
library(paletteer)
library(ggthemes)

setwd("D:/Climate_Research") # Setting working directory
info_file <- "output/Task1/info.RData" # Loading info file
mask_file <- "output/Task6/mask_of_hungarian_points.RData" # Loading Hungarian points mask
monthly_climate_file <- "output/Task6/monthly_climate.RData" # Loading monthly climate data
hungary_file <- "output/Task1/hungary.RData" # Loading Hungary geometry
load(file = info_file) 
load(file = mask_file)
load(file = monthly_climate_file) 
load(file = hungary_file)
countries <- st_read("input/countries/CNTR_RG_01M_2020_4326.shp") # Loading shapefile with countries

info$climate_targets <- c( # Setting the order of climate targets 
  "reference",    
  "CNRM-CM5_RACMO22E", "CNRM-CM5_RCA4",
  "EC-EARTH_RACMO22E", "EC-EARTH_RCA4",
  "IPSL-CM5A-MR_RACMO22E", "IPSL-CM5A-MR_RCA4",
  "NorESM1-M_RACMO22E", "NorESM1-M_RCA4"
)

specific_climate_periods <- setNames( # Creating specific climate periods list
  object = lapply(X = info$climate_targets, FUN = function(target) { # Iterating through 9 climate targets
    sf_obj <- st_as_sf(st_geometry(monthly_climate[[target]])) # Selecting only geometry column
    Tmean <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Tmean")) %>% as.matrix() # Extracting mean temperature data as matrix
    Tmax <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Tmax")) %>% as.matrix() # Extracting max temperature data as matrix
    Tmin <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Tmin")) %>% as.matrix() # Extracting min temperature data as matrix
    Prec <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Prec")) %>% as.matrix() # Extracting precipitation data as matrix
    sf_obj <- sf_obj %>% # Adding 4 new columns of specific periods to sf object
      mutate(wettestM = apply(X = Prec, MARGIN = 1, FUN = which.max), # Identifying the wettest month
             driestM = apply(X = Prec, MARGIN = 1, FUN = which.min), # Identifying the driest month
             warmestM = apply(X = Tmean, MARGIN = 1, FUN = which.max), # Identifying the warmest month
             coldestM = apply(X = Tmean, MARGIN = 1, FUN = which.min)  # Identifying the coldest month
      )
    temperature_of_quarters <- as.data.frame( # Creating dataframe for temperature of quarters
      t(apply(X = Tmean, MARGIN = 1, FUN = function(x) {
        sapply(X = 1:12, FUN = function(i) mean(x[c(i, (i %% 12) + 1, ((i + 1) %% 12) + 1)])) # Calculating quarter averages
      }))
    )
    precipitation_of_quarters <- as.data.frame( # Creating dataframe for precipitation of quarters
      t(apply(X = Prec, MARGIN = 1, FUN = function(x) {
        sapply(X = 1:12, FUN = function(i) sum(x[c(i, (i %% 12) + 1, ((i + 1) %% 12) + 1)])) # Calculating quarter sums
      }))
    )
    sf_obj <- sf_obj %>% # Adding 4 new columns of specific quarters to sf object
      mutate(wettestQ = apply(X = precipitation_of_quarters, MARGIN = 1, FUN = which.max), # Identifying the wettest quarter
             driestQ = apply(X = precipitation_of_quarters, MARGIN = 1, FUN = which.min), # Identifying the driest quarter
             warmestQ = apply(X = temperature_of_quarters, MARGIN = 1, FUN = which.max), # Identifying the warmest quarter
             coldestQ = apply(X = temperature_of_quarters, MARGIN = 1, FUN = which.min)  # Identifying the coldest quarter
      )
    sf_obj <- sf_obj[, info$climate_periods_abbr] # Reordering columns to match abbreviations
    return(sf_obj) # Returning sf object
  }),
  nm = info$climate_targets # Assigning names
)

bioclimatic_variables <- setNames( # Creating bioclimatic variables list
  object = lapply(X = info$climate_targets, FUN = function(target) { # Iterating through climate targets
    sf_obj <- st_as_sf(st_geometry(monthly_climate[[target]])) # Selecting only geometry column
    Tmean <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Tmean")) %>% as.matrix() # Extracting mean temperature as matrix
    Tmax <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Tmax")) %>% as.matrix() # Extracting max temperature as matrix
    Tmin <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Tmin")) %>% as.matrix() # Extracting min temperature as matrix
    Prec <- st_drop_geometry(monthly_climate[[target]]) %>% dplyr::select(matches = starts_with(match = "Prec")) %>% as.matrix() # Extracting precipitation as matrix
    data <- st_drop_geometry(specific_climate_periods[[target]]) # Using previously calculated data
    wettestQ <- data$wettestQ # Applying values to quarters and periods
    driestQ <- data$driestQ
    warmestQ <- data$warmestQ
    coldestQ <- data$coldestQ
    wettestM <- data$wettestM
    driestM <- data$driestM
    warmestM <- data$warmestM
    coldestM <- data$coldestM
    sf_obj <- sf_obj %>% # Adding new columns to sf object
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
    sf_obj <- sf_obj[, info$bioclim_variables_abbr] # Reordering columns to match bioclimatic abbreviations
    return(sf_obj) # Returning sf object
  }),
  nm = info$climate_targets # Naming variables
)

output_folder <- "output/Task7" # Defining output folder
dir.create(path = output_folder, showWarnings = FALSE, recursive = TRUE) # Creating folder
specific_climate_file <- file.path(output_folder, "specific_climate_periods.RData") # File path for specific climate periods
bioclimatic_file <- file.path(output_folder, "bioclimatic_variables.RData") # File path for bioclimatic variables
save(specific_climate_periods, file = specific_climate_file) # Saving specific climate periods
save(bioclimatic_variables, file = bioclimatic_file) # Saving bioclimatic variables
specific_climate_folder <- file.path(output_folder, "specific_climate_periods") # Defining path for specific climate periods
dir.create(path = specific_climate_folder, showWarnings = FALSE, recursive = TRUE) # Creating folder

palette <- colorRampPalette(colors = c("darkgreen", "darkblue", "purple", "red", "yellow", "darkgreen"))(13)[1:12] # Creting 12-color palette
breaks <- seq(from = 0.5, to = 12.5, by = 1) # Defining breaks
month_initials <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D") # Creating vector with month initials
quarter_initials <- c("JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS", "ASO", "SON", "OND", "NDJ", "DJF") # Creating vector with quarter initials
month_legend <- ggplotify::as.grob(function() { # Creating legend for months
  plot.new()  # Opening new plotting device
  legend(
    x = "center", # Legend position
    legend = month_initials, # Month initials
    pch = 21, # Point type: filled circle
    pt.cex = 2, # Circle size
    pt.bg = palette, # Filling colors from palette
    col = NA, # Removing circle borders
    cex = 1.2, # Text size
    bty = "n", # Removing box around legend
    y.intersp = 1.5, # Increasing line spacing
    x.intersp = 2 # Increasing spacing between circle and text
  )
})
quarter_legend <- ggplotify::as.grob(function() { # Creating legend for quarters
  plot.new()  # Opening new plotting device
  legend(
    x = "center", # Legend position (center)
    legend = quarter_initials, # Quarter initials
    pch = 21, # Point type: filled circle
    pt.cex = 2, # Circle size
    pt.bg = palette, # Filling colors from palette
    col = NA, # Removing circle borders
    cex = 1.2, # Text size
    bty = "n", # Removing box around legend
    y.intersp = 1.5, # Increasing line spacing
    x.intersp = 2 # Increasing spacing between circle and text
  )
})
layout_matrix <- matrix( # Creating layout matrix
  data = c(NA, 1, 1, 10,
           2, 2, 3, 3,
           4, 4, 5, 5,
           6, 6, 7, 7,
           8, 8, 9, 9),
  ncol = 4, 
  byrow = TRUE
)

for (period in info$climate_periods_abbr) { # Iterating through climate periods
  subfigures <- lapply(X = info$climate_targets, FUN = function(target) { # Creating list of subfigures
    map_data <- specific_climate_periods[[target]] # Getting data for each target
    if (target == "reference") { # if the target is reference
      map_data <- map_data[mask_of_hungarian_points, ] # Then filter data for Hungary mask
    }
    title_text <- if (target == "reference") { # Creating title, if target is reference
      paste0("E-OBS (", paste(x = info$reference_period, collapse = "\u2013"), ")") # then title for reference target
    } else {
      model_info <- info$model_combinations[info$model_combinations$combination == target, ]
      gcm <- as.character(model_info$global_model) # Extracting GCM name
      rcm <- as.character(model_info$regional_model) # Extracting RCM name
      paste0(gcm, " \u2013 ", rcm, " (", paste(x = info$future_period, collapse = "\u2013"), ")")} # Title for future targets
    map_data$color <- palette[cut(map_data[[period]], breaks = breaks, include.lowest = TRUE, labels = FALSE)] # Adding colors to map data
    map_plot <- ggplot(data = map_data) + # Creating map
      geom_sf(mapping = aes(fill = color), shape = 22, size = 5, stroke = 0) +  # Filling colors
      scale_fill_identity() + # Directly applying colors
      geom_sf(data = countries, fill = NA, color = "black", size = 0.5) + # Adding countries
      geom_sf(data = hungary, fill = NA, color = "darkred", size = 1.5) + # Adding Hungary
      labs(title = title_text) + # Adding title to the plot
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 14,       # Setting title font size
          face = "plain",  # Setting title font face
          hjust = 0.5      # Center-align title
        ),
        panel.border = element_rect( # Adding frame
          color = "black",
          fill = NA,
          linewidth = 0.5
        ),
        legend.position = "none",         # Disabling legend
        axis.title = element_blank(),     # Removing axis titles
        axis.text = element_blank(),      # Removing axis text labels
        axis.ticks = element_blank()      # Removing axis ticks
      ) + 
      coord_sf( # Limiting axes
        xlim = c(st_bbox(map_data)$xmin, st_bbox(map_data)$xmax),
        ylim = c(st_bbox(map_data)$ymin, st_bbox(map_data)$ymax),
        expand = FALSE
      )
  })
  if (period %in% c("wettestQ", "driestQ", "warmestQ", "coldestQ")) { # Checking if period corresponds to quarter
    subfigures <- append(
      x = subfigures,
      values = list(quarter_legend) # Adding quarter legend
    )
  } else { # If period corresponds to month
    subfigures <- append(
      x = subfigures,
      values = list(month_legend) # Adding month legend
    )
  }
  composite_figure <- arrangeGrob( # Creating composite figure
    grobs = subfigures, 
    layout_matrix = layout_matrix, # Using matrix
    widths = c(1, 1, 1, 1),        # Setting column widths
    heights = c(0.8, 1, 1, 1, 1)     # Setting row heights
  )
  filename <- file.path(specific_climate_folder, paste0(period, ".png")) # Defining output filename
  ggsave(
    filename = filename,
    plot = composite_figure,
    width = 10,
    height = 20,
    dpi = 300
  )
} # Saving result
writeLines("Visualization completed.") # Message for completed visualization

bioclim_folder <- file.path(output_folder, "bioclimatic_variables") # Defining path for bioclimatic variables
dir.create(path = bioclim_folder, showWarnings = FALSE, recursive = TRUE) # Creating subfolder

layout_matrix <- matrix( # Creating layout matrix
  data = c(NA, 1, 1, 10,
           2, 2, 3, 3,
           4, 4, 5, 5,
           6, 6, 7, 7,
           8, 8, 9, 9),
  ncol = 4,
  byrow = TRUE
)
for (variable in info$bioclim_variables_abbr) {  # Iterating through climate variables
  min_value <- Inf  # Initializing minimum value
  max_value <- -Inf # Initializing maximum value
  lapply( # Iterating through targets
    X = info$climate_targets, 
    FUN = function(target) {
      data <- bioclimatic_variables[[target]] # Extracting data
      if (target == "reference") {
        data <- data[mask_of_hungarian_points, ] # Filter for Hungary if target is reference
      }
      if (variable %in% colnames(data)) { # Checking variables
        non_na_values <- data[[variable]][!is.na(data[[variable]])] # Extracting non NA values
        if (length(non_na_values) > 0) {
          min_value <<- min(min_value, min(non_na_values, na.rm = TRUE)) # Calculating minimum value
          max_value <<- max(max_value, max(non_na_values, na.rm = TRUE)) # Calculating maximum value
        }
      }
    })
  breaks <- pretty(c(min_value, max_value), n = 10) # Creating breaks
  temperature_variables <- paste0("bcv", as.character(1:11)) # Defining temperature variables
  precipitation_variables <- paste0("bcv", as.character(12:19)) # Defining precipitation variables
  palette <- if (variable %in% temperature_variables) { 
    paletteer::paletteer_c(palette = "ggthemes::Temperature Diverging", n = length(breaks) - 1) # Palette for temperature variables
  } else if (variable %in% precipitation_variables) { 
    paletteer::paletteer_c(palette = "ggthemes::Blue-Green Sequential", n = length(breaks) - 1) # Palette for precipitation variables
  } else {
    stop(paste("Variable", variable, "is neither temperature nor precipitation-related")) # Error for unsupported variables
  }
  colorbar <- grid::grobTree( # Creating colorbar
    grid::rectGrob( # Creating color segments
      x = unit(x = 0.5, units = "npc"),
      y = unit(x = seq(from = 0.2, to = 0.8, length.out = length(x = palette)), units = "npc"),
      width = unit(x = 0.1, units = "npc"),
      height = unit(x = 0.7 / length(x = palette), units = "npc"),
      gp = grid::gpar(fill = palette, col = NA)
    ),
    grid::textGrob( # Creating text labels
      label = round(x = breaks, digits = 1),
      x = unit(x = 0.6, units = "npc"),
      y = unit(x = seq(from = 0.2, to = 0.8, length.out = length(x = breaks)), units = "npc"),
      just = "left", gp = grid::gpar(fontsize = 8)
    )
  )
  subfigures <- lapply( # Creating subfigures for bioclimatic variables
    X = info$climate_targets,
    FUN = function(target) { 
      map_data <- bioclimatic_variables[[target]] # Extracting data for targets
      if (target == "reference") {
        map_data <- map_data[mask_of_hungarian_points, ] # Filter data if target is reference
      }
      title_text <- if (target == "reference") { # Creating title, if target is reference
        paste0("E-OBS (", paste(x = info$reference_period, collapse = "\u2013"), ")") # then title for reference target
      } else {
        model_info <- info$model_combinations[info$model_combinations$combination == target, ]
        gcm <- as.character(model_info$global_model) # Extracting GCM name
        rcm <- as.character(model_info$regional_model) # Extracting RCM name
        paste0(gcm, " \u2013 ", rcm, " (", paste(x = info$future_period, collapse = "\u2013"), ")")} # Title for future targets
      map_plot <- ggplot(data = map_data) + # Creating map plot
        geom_sf(mapping = aes(fill = .data[[variable]]), 
                shape = 22, size = 5, stroke = 0) +  # Filling data
        geom_sf(data = countries, fill = NA, color = "black", size = 0.5) + # Adding countries
        geom_sf(data = hungary, fill = NA, color = "darkred", size = 1.5) + # Adding Hungary
        scale_fill_gradientn(colors = palette, breaks = breaks, limits = c(min_value, max_value), oob = scales::squish) + # Applying colors, breaks and limits
        labs(title = title_text) + # Adding title
        theme_minimal() + # Adding theme
        theme(
          plot.title = element_text(size = 14, hjust = 0.5), # Plotting title
          panel.border = element_rect( # Adding frame
            color = "black",
            fill = NA,
            linewidth = 0.5
          ),
          legend.position = "none", # Disabling legend
          axis.title = element_blank(), # Removing title
          axis.text = element_blank(), # Removing axis labels
          axis.ticks = element_blank() # Removing axis ticks
        ) +
        coord_sf( # Limiting axes
          xlim = c(st_bbox(map_data)$xmin, st_bbox(map_data)$xmax),
          ylim = c(st_bbox(map_data)$ymin, st_bbox(map_data)$ymax),
          expand = FALSE
        )
    })
  subfigures <- append(x = subfigures, values = list(colorbar)) # Adding colorbar
  composite_figure <- arrangeGrob( # Creating composite figure
    grobs = subfigures,
    layout_matrix = layout_matrix,
    widths = c(1, 1, 1, 1),     
    heights = c(0.8, 1, 1, 1, 1),
    top = textGrob(
      label = info$bioclim_variables_full[variable],
      gp = gpar(fontsize = 14, fontface = "plain")
    )
  )
  ggsave( # Saving composite images
    filename = file.path(path = bioclim_folder, paste0(variable, ".png")),
    plot = composite_figure,
    width = 10,
    height = 20,
    dpi = 300
  )}
writeLines("Visualization completed.") # Message for completed visualization
