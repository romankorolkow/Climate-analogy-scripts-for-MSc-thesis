# Creator: Roman Korolkov
# Date: 18 Mar 2025
# Version: 03
# Description: The script visualizes climate analogy
# Duration: Approximately 30 minutes
# Saved files size: ~ 6 MB

if (!requireNamespace("sf")) install.packages("sf") # Installing and loading necessary packages
if (!requireNamespace("gridExtra")) install.packages("gridExtra")
if (!requireNamespace("grid")) install.packages("grid")
if (!requireNamespace("future.apply")) install.packages("future.apply")
if (!requireNamespace("ggplotify")) install.packages("ggplotify")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("paletteer")) install.packages("paletteer")
if (!requireNamespace("fields")) install.packages("fields")
if (!requireNamespace("ggspatial")) install.packages("ggspatial")
if (!requireNamespace("lattice")) install.packages("lattice")
library(sf)
library(gridExtra)
library(grid) 
library(future.apply)
library(ggplotify)
library(ggplot2)
library(paletteer)
library(fields)
library(ggspatial)
library(lattice)

setwd("D:/Climate_Research") # Setting working directory
info_file <- "output/Task1/info.RData"
load(file = info_file) # Loading info object
analogy_ap_file <- "output/Task10/analogy_of_all_points_to_hungarian_points.RData"  
load(file = analogy_ap_file) # Analogy of all points to hungarian points
analogy_hp_file <- "output/Task10/analogy_of_hungarian_points_to_all_points.RData"
load(file = analogy_hp_file) # Analogy of hungarian points to all points
hungary_file <- "output/Task1/hungary.RData"  
load(file = hungary_file) # Hungary file
countries <- st_read(dsn = "input/countries/CNTR_RG_01M_2020_4326.shp") # Countries shapefile
mean_sf_file <- "output/Task10/mean_sf.RData"  
load(file = mean_sf_file) # File with mean values for climate models

info$climate_targets <- c( # Setting the order of climate targets
  "reference",    
  "CNRM-CM5_RACMO22E", "CNRM-CM5_RCA4",
  "EC-EARTH_RACMO22E", "EC-EARTH_RCA4",
  "IPSL-CM5A-MR_RACMO22E", "IPSL-CM5A-MR_RCA4",
  "NorESM1-M_RACMO22E", "NorESM1-M_RCA4"
)
informative_labels <- c( # Setting informative labels
  "ID_of_most_similar_hungarian_point" = "ID of the most similar Hungarian point",
  "similarity_of_most_similar_hungarian_point" = "Similarity of the most similar Hungarian point",
  "geographical_distance_of_most_similar_hungarian_point" = "Geographical distance of the most similar Hungarian point",
  "latitudinal_distance_of_most_similar_hungarian_point" = "Latitudinal distance of the most similar Hungarian point",
  "mean_similarity_of_all_hungarian_points" = "Mean similarity of all Hungarian points",
  "ID_of_most_similar_point" = "ID of the most similar point",
  "similarity_of_most_similar_point" = "Similarity of the most similar point",
  "geographical_distance_of_most_similar_point" = "Geographical distance of the most similar point",
  "latitudinal_distance_of_most_similar_point" = "Latitudinal distance of the most similar point",
  "mean_similarity_of_all_points" = "Mean similarity of all points",
  "country_of_the_most_similar_point" = "Country of the most similar point",
  "ID_of_most_similar_foreign_point" = "ID of the most similar foreign point",
  "similarity_of_most_similar_foreign_point" = "Similarity of the most similar foreign point",
  "geographical_distance_of_most_similar_foreign_point" = "Geographical distance of the most similar foreign point",
  "latitudinal_distance_of_most_similar_foreign_point" = "Latitudinal distance of the most similar foreign point",
  "mean_similarity_of_all_foreign_points" = "Mean similarity of all foreign points",
  "country_of_the_most_similar_foreign_point" = "Country of the most similar foreign point"
)
layout_matrix_ap <- matrix( # Creating layout matrix for all points to Hungarian points
  data = c(
    NA, 2, 4, 6, 8,
    1, 2, 4, 6, 8,
    1, 3, 5, 7, 9,
    10, 3, 5, 7, 9
  ),
  ncol = 5,
  byrow = TRUE
)
layout_matrix <- matrix(  # Creating layout matrix for Hungarian points to all points
  data = c(NA, 1, 1, 10,
           2, 2, 3, 3,
           4, 4, 5, 5,
           6, 6, 7, 7,
           8, 8, 9, 9),
  ncol = 4,
  byrow = TRUE
)
output_folder_1 <- "output/Task11/analogy_of_all_points_to_hungarian_points" # Defining subfolder path
dir.create(path = output_folder_1, showWarnings = FALSE, recursive = TRUE) # Creating subfolder

measure_cols <- setdiff(
  x = colnames(x = analogy_of_all_points_to_hungarian_points[["NorESM1-M_RCA4"]]), # Getting column names from sample sf object
  y = attr(x = analogy_of_all_points_to_hungarian_points[["NorESM1-M_RCA4"]], which = "sf_column") # Excluding geometry
)
plan(strategy = "multisession") # Enabling multisession
options(future.globals.maxSize = Inf) # Removing memory size limit

invisible(x = future.apply::future_lapply( # Iterating through each measure column
  X = measure_cols,
  FUN = function(measure_col_name) {
    if (measure_col_name == "above_threshold") { # If the measure is above threshold
      future_only <- setdiff(x = info$climate_targets, y = "reference") # Excluding reference target
      figure_title <- "Areas with similarity >0.6"# Setting figure title
      subfigures <- lapply( # Creating subfigures
        X = future_only,
        FUN = function(target_name) {
          sf_data <- analogy_of_all_points_to_hungarian_points[[target_name]] # Extracting sf
          model_info <- info$model_combinations[info$model_combinations$combination == target_name, , drop = FALSE]  # Extracting model information
          gcm <- as.character(x = model_info$global_model)
          rcm <- as.character(x = model_info$regional_model)
          title_text <- paste0( # Adding title text
            gcm, " \u2013 ", rcm, " (",
            paste(x = info$future_period, collapse = "\u2013"),
            ")"
          )
          bbox_data <- sf::st_bbox(obj = sf_data) # Getting bounding box of sf data
          map_plot <- ggplot2::ggplot(data = sf_data) + # Initializing ggplot
            ggplot2::geom_sf( # Plotting sf points
              mapping = ggplot2::aes(fill = above_threshold),
              shape = 22,
              color = "NA"
            ) +
            ggplot2::scale_fill_manual( # Manually setting colors
              values = c("TRUE" = "orange", "FALSE" = "white"),
              na.value = "white"
            ) +
            ggplot2::geom_sf( # Adding countries
              data = countries,
              fill = NA,
              color = "black",
              size = 0.5
            ) +
            ggplot2::geom_sf( # Adding Hungary
              data = hungary,
              fill = NA,
              color = "darkred",
              linewidth = 0.7
            ) +
            ggplot2::labs( # Adding title
              title = title_text
            ) +
            ggplot2::theme_minimal() + # Minimal theme
            ggplot2::theme(
              legend.position = "none",
              plot.title = ggplot2::element_text(size = 7, face = "plain", hjust = 0.5)
            ) +
            ggplot2::coord_sf( # Cropping plot to bounding box
              xlim = c(bbox_data$xmin, bbox_data$xmax),
              ylim = c(bbox_data$ymin, bbox_data$ymax),
              expand = FALSE
            ) +
            ggspatial::annotation_scale( # Adding scale bar
              location = "bl", 
              height = grid::unit(x = 1.2, units = "mm"), 
              text_cex = 0.6
            )
          return(map_plot) # Returning map plot
        }
      )
      composite_figure <- gridExtra::arrangeGrob( # Creating composite figure
        grobs = subfigures,
        layout_matrix = matrix(
          data = c(
            1, 3, 5, 7,  
            1, 3, 5, 7, 
            2, 4, 6, 8, 
            2, 4, 6, 8  
          ),
          ncol = 4, 
          byrow = TRUE
        ),
        top = grid::textGrob( # Adding title to composite figure
          label = figure_title,
          gp = gpar(fontsize = 10, fontface = "plain")
        )
      )
      output_file <- file.path( # Output path
        output_folder_1,
        paste0(measure_col_name, ".png")
      )
      grDevices::png( # Opening PNG device 
        filename = output_file, 
        width = 4000, 
        height = 2000, 
        res = 300
      )
      grid::grid.newpage() # Creating grid page
      grid::grid.draw(x = composite_figure) # Drawing composite figure
      grDevices::dev.off() # Closing PNG device and saving file
      return(NULL)
    }
    min_value <- Inf  # Initializing minimum value
    max_value <- -Inf  # Initializing maximum value
    for (target_name in names(x = analogy_of_all_points_to_hungarian_points)) { # Looping over each target
      df <- analogy_of_all_points_to_hungarian_points[[target_name]] # Extracting sf data 
      non_na <- df[[measure_col_name]][!is.na(x = df[[measure_col_name]])] # Extracting non-NA values
      if (length(x = non_na) > 0) { # If there are non-NA values
        min_value <- min(x = min_value, na.rm = FALSE, non_na) # Updating global minimum
        max_value <- max(x = max_value, na.rm = FALSE, non_na) # Updating global maximum
      }
    }
    breaks <- pretty(x = c(min_value, max_value), n = 10) # Creating breaks
    palette_colors <- as.character( # Generating color palette
      paletteer::paletteer_c(palette = "grDevices::Viridis", n = length(x = breaks) - 1)
    )
    colorbar <- ggplotify::as.grob( # Creating colorbar
      function() {
        fields::image.plot( # Plotting colorbar
          x = 1:2, 
          y = 1:2,
          z = matrix(data = 1:4, nrow = 2),
          legend.only = TRUE,
          horizontal = TRUE,
          col = palette_colors,
          breaks = breaks,
          smallplot = c(0.1, 0.9, 0.4, 0.55),
          axis.args = list( 
            cex.axis = 0.45,
            at = breaks,
            labels = round(x = breaks, digits = 2)
          )
        )
      }
    )
    subfigures <- lapply( # Building list of subfigures
      X = info$climate_targets, 
      FUN = function(target_name) { 
        sf_data <- analogy_of_all_points_to_hungarian_points[[target_name]] # Extracting sf data
        sf_data$plot_value <- sf_data[[measure_col_name]] # Setting column for plotting measure value
        title_text <- if (target_name == "reference") { # If target is reference
          paste0( # Composing title text for reference target
            "E-OBS (",
            paste(x = info$reference_period, collapse = "\u2013"),
            ")"
          )
        } else {  # Else for future targets
          model_info <- info$model_combinations[info$model_combinations$combination == target_name, , drop = FALSE] # Extracting model info
          gcm <- as.character(x = model_info$global_model) 
          rcm <- as.character(x = model_info$regional_model) 
          paste0(
            gcm, " \u2013 ", rcm, " (",
            paste(x = info$future_period, collapse = "\u2013"),
            ")"
          )
        }
        bbox_data <- sf::st_bbox(obj = sf_data) # Getting bounding box
        map_plot <- ggplot2::ggplot(data = sf_data) + # Initializing ggplot
          ggplot2::geom_sf( # Plotting sf data
            mapping = ggplot2::aes(fill = .data[[measure_col_name]]),
            shape = 22, 
            size = 1.3, 
            stroke = 0, 
            color = "transparent" 
          ) +
          ggplot2::scale_fill_gradientn( # Applying color scale
            colors = palette_colors,
            breaks = breaks, 
            limits = c(min_value, max_value),
            oob = scales::squish
          ) +
          ggplot2::geom_sf( # Adding countries
            data = countries,
            fill = NA,
            color = "black", 
            size = 0.5
          ) +
          ggplot2::geom_sf( # Adding Hungary
            data = hungary,
            fill = NA, 
            color = "darkred",
            linewidth = 0.7
          ) +
          ggplot2::labs( # Setting plot title
            title = title_text
          ) +
          ggplot2::theme_minimal() + # Minimal theme 
          ggplot2::theme( # Customizing elements
            axis.text = ggplot2::element_text(size = 6),
            plot.title = ggplot2::element_text(size = 7, face = "plain", hjust = 0.5), 
            legend.position = "none"
          ) +
          ggplot2::coord_sf( # Cropping plot to the bounding box
            xlim = c(bbox_data$xmin, bbox_data$xmax),
            ylim = c(bbox_data$ymin, bbox_data$ymax), 
            expand = FALSE 
          ) +
          ggspatial::annotation_scale( # Adding scale bar
            location = "bl",
            height = grid::unit(x = 1.2, units = "mm"), 
            text_cex = 0.6 
          )
        return(map_plot) # Returning map plot
      }
    )
    subfigures <- append( # Appending colorbar to list of subfigures
      x = subfigures, 
      values = list(colorbar)
    )
    figure_title <- informative_labels[measure_col_name] %||% measure_col_name # Setting figure title with informative labels
    composite_figure <- gridExtra::arrangeGrob( # Arranging subfigures into composite figure
      grobs = subfigures,
      layout_matrix = layout_matrix_ap, 
      widths = c(1.2, 1.2, 1.2, 1.2, 1.2),
      heights = c(1.2, 1.2, 1.2, 1.2), 
      top = grid::textGrob( # Adding title
        label = figure_title, 
        gp = grid::gpar(fontsize = 10, fontface = "plain") 
      )
    )
    output_file <- file.path( # Output file path
      output_folder_1,
      paste0(measure_col_name, ".png")
    )
    grDevices::png( # Opening PNG device
      filename = output_file, 
      width = 4000,  
      height = 2000, 
      res = 300 
    )
    grid::grid.newpage() # Creating new grid page
    grid::grid.draw(x = composite_figure) # Drawing composite figure 
    grDevices::dev.off() # Closing PNG device and saving file
  },
  future.seed = TRUE # Ensuring parallel processing uses safe random seeds
))

output_folder_2 <- "output/Task11/analogy_of_hungarian_points_to_all_points" # Output subfolder path 
dir.create(path = output_folder_2, showWarnings = FALSE, recursive = TRUE) # Creating output subfolder

invisible(x = future.apply::future_lapply( # Iterating over measure columns for Hungarian points to all points
  X = setdiff( # Getting column names excluding geometry
    x = colnames(x = analogy_of_hungarian_points_to_all_points[["reference"]]),
    y = attr(x = analogy_of_hungarian_points_to_all_points[["reference"]], which = "sf_column")
  ),
  FUN = function(measure_col_name) {
    if (measure_col_name %in% c("country_of_the_most_similar_point", "country_of_the_most_similar_foreign_point")) { # If measure is categorical
      unique_countries <- unique( # Getting unique country names
        unlist(x = lapply( # Applying function over all targets
          X = analogy_of_hungarian_points_to_all_points, 
          FUN = function(df) df[[measure_col_name]] # Extracting country column
        ))
      )
      color_palette <- RColorBrewer::brewer.pal( # Generating color palette
        n = min(x = length(x = unique_countries), y = 11), # Using minimum of number of unique countries or 10 colors
        name = "Set3" # Palette name
      )
      color_mapping <- setNames(object = color_palette, nm = unique_countries) # Mapping colors 
      country_legend <- lattice::draw.key( # Creating legend
        key = list( # Defining key list
          space = "right",
          columns = 1,
          between = 1,
          rectangles = list( # Defining rectangles 
            col = color_mapping,  
            border = FALSE, 
            size = 1.2
          ),
          text = list( # Defining text for legend
            labels = unique_countries,
            cex = 0.7
          ),
          padding.text = 0.6 # Settng padding between text and rectangles
        ),
        draw = FALSE # Not drawing legend
      )
    } else {  # Else, if measure is numeric
      min_value <- Inf # Initializing minimum value
      max_value <- -Inf # Initializing maximum value
      for (target_name in names(x = analogy_of_hungarian_points_to_all_points)) { # Looping over each target
        df <- analogy_of_hungarian_points_to_all_points[[target_name]] # Extracting sf data
        non_na <- df[[measure_col_name]][!is.na(x = df[[measure_col_name]])] # Getting non-NA values
        if (length(x = non_na) > 0) { # If there are non-NA values
          min_value <- min(x = min_value, non_na) # Updating global minimum
          max_value <- max(x = max_value, non_na) # Updating global maximum
        }
      }
      breaks <- pretty(x = c(min_value, max_value), n = 10) # Creating breaks
      palette_colors <- as.character( # Generating color palette
        paletteer::paletteer_c(palette = "grDevices::Viridis", n = length(x = breaks) - 1)
      )
      colorbar <- ggplotify::as.grob( # Creating colorbar
        function() {
          fields::image.plot( # Plotting colorbar
            x = 1:2,
            y = 1:2,
            z = matrix(data = 1:4, nrow = 2),
            legend.only = TRUE,
            horizontal = FALSE,
            col = palette_colors, 
            breaks = breaks, 
            smallplot = c(0.3, 0.5, 0.2, 0.8),
            axis.args = list(
              cex.axis = 0.8, 
              at = breaks, 
              labels = round(x = breaks, digits = 2)
            )
          )
        }
      )
    }
    subfigures <- lapply( # Creating subfigures 
      X = info$climate_targets, 
      FUN = function(target_name) { 
        sf_data <- analogy_of_hungarian_points_to_all_points[[target_name]] # Extracting sf data 
        sf_data$plot_value <- sf_data[[measure_col_name]] # Setting measure value for plotting
        title_text <- if (target_name == "reference") { # If target is reference
          paste0( # Composing title text 
            "E-OBS (",
            paste(x = info$reference_period, collapse = "\u2013"),
            ")"
          )
        } else { # Else, for future targets
          model_info <- info$model_combinations[info$model_combinations$combination == target_name, , drop = FALSE] # Extracting model info
          gcm <- as.character(x = model_info$global_model) 
          rcm <- as.character(x = model_info$regional_model)
          paste0( # Composing title text
            gcm, " \u2013 ", rcm, " (",
            paste(x = info$future_period, collapse = "\u2013"),
            ")"
          )
        }
        bbox_data <- sf::st_bbox(obj = sf_data) # Getting bounding box
        map_plot <- ggplot2::ggplot(data = sf_data) + # Initializing ggplot
          ggplot2::geom_sf( # Plotting sf data 
            aes(fill = .data[[measure_col_name]]), # Mapping fill aesthetic
            shape = 22, 
            size = 3,
            stroke = 0, 
            color = "transparent"
          ) +
          ggplot2::theme(legend.position = "none") + # Removing legend from plot
          ggplot2::geom_sf( # Adding countries
            data = countries,
            fill = NA,
            color = "black",
            size = 0.5
          ) +
          ggplot2::geom_sf( # Adding Hungary
            data = hungary,
            fill = NA,
            color = "darkred",
            linewidth = 0.7
          ) +
          ggplot2::labs(title = title_text) + # Setting plot title
          ggplot2::theme_minimal() + # Using minimal theme 
          ggplot2::theme( # Customizing theme elements
            axis.text = ggplot2::element_text(size = 6),
            plot.title = ggplot2::element_text(size = 7, face = "plain", hjust = 0.5),
            legend.position = "none"
          ) +
          ggplot2::coord_sf( # Cropping plot
            xlim = c(bbox_data$xmin, bbox_data$xmax),
            ylim = c(bbox_data$ymin, bbox_data$ymax)
          ) +
          ggspatial::annotation_scale( # Adding scale bar 
            location = "br", 
            height = grid::unit(x = 1.2, units = "mm"), 
            text_cex = 0.6 
          )
        if (measure_col_name %in% c("country_of_the_most_similar_point", "country_of_the_most_similar_foreign_point")) {  # If measure is categorical
          map_plot <- map_plot + ggplot2::scale_fill_manual( # Using manual fill scale
            values = color_mapping, 
            na.value = "grey80" 
          )
        } else { # Else, if measure is numeric
          map_plot <- map_plot + ggplot2::scale_fill_gradientn( # Use gradient fill scale
            colors = palette_colors, 
            breaks = breaks,
            limits = c(min_value, max_value),
            oob = scales::squish 
          )
        }
        return(map_plot) # Returning map plot
      }
    )
    if (measure_col_name %in% c("country_of_the_most_similar_point", "country_of_the_most_similar_foreign_point")) {  # If measure is categorical
      subfigures <- append( # Appending country legend to subfigures
        x = subfigures, 
        values = list(country_legend)
      )
    } else { # Else, for numeric measures
      subfigures <- append( # Appending numeric colorbar to subfigures
        x = subfigures, 
        values = list(colorbar)
      )
    }
    figure_title <- informative_labels[measure_col_name] %||% measure_col_name # Setting figure title with informative labels
    composite_figure <- gridExtra::arrangeGrob( # Arranging subfigures into composite figure
      grobs = subfigures,
      layout_matrix = layout_matrix,
      widths = c(1.2, 1.2, 1.2, 1.2),
      heights = c(1.2, 1.2, 1.2, 1.2, 1.2),
      top = grid::textGrob( # Adding title
        label = figure_title, 
        gp = grid::gpar(fontsize = 10, fontface = "plain")
      )
    )
    output_file <- file.path( # Output file path
      output_folder_2,
      paste0(measure_col_name, ".png")
    )
    grDevices::png( # Opening PNG device
      filename = output_file,
      width = 2000,
      height = 4000,
      res = 300
    )
    grid::grid.newpage() # Creating new grid
    grid::grid.draw(x = composite_figure) # Drawing composite figure
    grDevices::dev.off() # Closing PNG device and saveing file
  },
  future.seed = TRUE # Ensuring parallel processing uses safe random seeds
)
)
new_measures <- c("mean_future_similarity", "count_of_analog_scenarios") # Defining list of measures
for (measure_col_name in new_measures) { # Looping over each measure
  min_value <- min(x = mean_sf[[measure_col_name]], na.rm = TRUE) # Computing minimum value
  max_value <- max(x = mean_sf[[measure_col_name]], na.rm = TRUE) # Computing maximum value
  breaks <- pretty(x = c(min_value, max_value), n = 10) # Creating breaks
  palette_colors <- as.character( # Generating color palette
    paletteer::paletteer_c(palette = "grDevices::Viridis", n = length(x = breaks) - 1)
  )
  colorbar <- ggplotify::as.grob( # Creating colorbar
    function() {
      fields::image.plot( # Plotting colorbar
        x = 1:2,
        y = 1:2,
        z = matrix(data = 1:4, nrow = 2),
        legend.only = TRUE, 
        horizontal = FALSE,
        col = palette_colors,
        breaks = breaks,
        smallplot = c(0.4, 0.6, 0.3, 0.7),
        axis.args = list(
          cex.axis = 0.6,
          at = breaks,
          labels = round(x = breaks, digits = 2)
        )
      )
    }
  )
  bbox_data <- sf::st_bbox(obj = mean_sf) # Getting bounding box
  map_plot <- ggplot2::ggplot(data = mean_sf) + # Initializing ggplot
    ggplot2::geom_sf( # Plotting sf data
      aes(fill = .data[[measure_col_name]]), # Mapping fill aesthetic
      shape = 22, 
      size = 2, 
      stroke = 0, 
      color = "transparent"
    ) +
    ggplot2::scale_fill_gradientn( # Applying gradient scale
      colors = palette_colors, 
      breaks = breaks, 
      limits = c(min_value, max_value),
      oob = scales::squish 
    ) +
    ggplot2::geom_sf( # Adding countries
      data = countries,
      fill = NA,
      color = "black", 
      size = 0.5
    ) +
    ggplot2::geom_sf( # Adding Hungary
      data = hungary, 
      fill = NA, 
      color = "darkred",
      linewidth = 0.7
    ) +
    ggplot2::theme_minimal() + # Using minimal theme
    ggplot2::theme( # Customizing theme elements
      axis.text = ggplot2::element_text(size = 6), 
      plot.title = ggplot2::element_text(size = 7, face = "plain", hjust = 0.5),
      legend.position = "none" 
    ) +
    ggplot2::coord_sf( # Cropping
      xlim = c(bbox_data$xmin, bbox_data$xmax), 
      ylim = c(bbox_data$ymin, bbox_data$ymax), 
      expand = FALSE
    ) +
    ggspatial::annotation_scale( # Adding scale bar
      location = "bl", 
      height = grid::unit(x = 1.2, units = "mm"), 
      text_cex = 0.6 
    )
  subfigures <- list(map_plot, colorbar) # Creating subfigures
  if (measure_col_name == "count_of_analog_scenarios") { # If measure is count of analog scenarios
    figure_title <- "Number of scenarios classifying a point as analogous" # Setting figure title
  } else { # Else, for mean future similarity measure
    figure_title <- "Mean future similarity among all scenarios" # Setting figure title
  }
  composite_figure <- gridExtra::arrangeGrob( # Arranging into composite figure
    grobs = subfigures, 
    layout_matrix = matrix( 
      data = c(1, 2), 
      ncol = 2, 
      byrow = TRUE 
    ),
    widths = c(3, 1), 
    top = grid::textGrob(
      label = figure_title,  
      gp = grid::gpar(fontsize = 14, fontface = "plain")
    )
  )
  out_file <- file.path( # Output file 
    "output/Task11/mean_sf_maps",  
    paste0(measure_col_name, ".png")
  )
  dir.create(path = dirname(path = out_file), showWarnings = FALSE, recursive = TRUE) # Creating output subfolder
  png(filename = out_file, width = 1600, height = 1900, res = 300) # Opening PNG device
  grid::grid.newpage() # Creating new grid
  grid::grid.draw(x = composite_figure) # Drawing composite figure
  dev.off() # Closin PNG device and saving file
}
plan(strategy = "sequential") # Reverting processing strategy to sequential