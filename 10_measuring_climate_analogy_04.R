# Creator: Roman Korolkov
# Date: 18 Mar 2025
# Version: 04
# Description: The script measures climate analogy.
# Duration: Approximately 2,5 hours
# Saved files size: ~ 4 MB

if (!requireNamespace(package = "sf")) install.packages(pkgs = "sf") # Installing and loading needed packages
if (!requireNamespace(package = "future")) install.packages(pkgs = "future")
if (!requireNamespace(package = "future.apply")) install.packages(pkgs = "future.apply")
library(package = sf)
library(package = future)
library(package = future.apply)

setwd("D:/Climate_Research") # Setting working directory
info_file <- "output/Task1/info.RData" # Path to info file
load(file = info_file) # Loading info file
all_points_file <- "output/Task9/all_points.RData" # Path to all points file
load(file = all_points_file) # Loading all points
mask_file <- "output/Task6/mask_of_hungarian_points.RData" # Path to Hungarian points mask
load(file = mask_file) # Loading Hungarian points mask
point_ids_file <- "output/Task8/point_ids.RData" # Path to point IDs file
load(file = point_ids_file) # Loading point IDs
hungarian_point_ids_file <- "output/Task9/hungarian_point_ids.RData" # Path to Hungarian point IDs
load(file = hungarian_point_ids_file) # Loading Hungarian point IDs
similarities_file <- "output/Task9/similarities.RData" # Path to similarities file
load(file = similarities_file) # Loading similarities
countries <- st_read(dsn = "input/countries/CNTR_RG_01M_2020_4326.shp") # Reading countries shapefile

hungarian_points <- all_points[mask_of_hungarian_points, ] # Creating new sf object with Hungarian points only
all_points_LAEA <- st_transform(x = all_points, crs = 3035) # Transforming all points to EPSG:3035
plan(strategy = "multisession") # Enabling multisession processing
options(future.globals.maxSize = Inf) # Removing memory size limit for futures

analogy_of_all_points_to_hungarian_points <- setNames( # Creating list of analogy of all points to Hungarian points
  object = lapply(
    X = info$climate_targets, # Iterating through climate targets
    FUN = function(climate_target) {
      selected_similarities <- similarities[[climate_target]] # Copying similarities
      selected_similarities <- st_drop_geometry(x = selected_similarities) # Dropping geometry
      results_list <- future_lapply( # Creating list of 1-row matrices
        X = point_ids, # Iterating through point IDs
        FUN = function(point_id) {
          studied_point_LAEA <- all_points_LAEA[point_id, , drop = FALSE] # Extracting specific point from all points LAEA
          similarity_column <- selected_similarities[[as.numeric(point_id)]] # Extracting similarity column for point
          finite_check <- is.finite(x = similarity_column) # Check for finite similarities
          if (!any(finite_check)) {
            return(matrix( # Returning matrix if no finite similarity
              data = rep(x = NA_real_, times = 5),
              nrow = 1,
              ncol = 5
            ))
          } else {
            index_most_similar <- which.max(x = similarity_column) # ID of most similar Hungarian point
            best_similarity <- similarity_column[index_most_similar] # Similarity of most similar Hungarian point
            geo_dist <- as.numeric( # Euclidean distance
              st_distance(
                x = studied_point_LAEA,
                y = all_points_LAEA[as.numeric(hungarian_point_ids[index_most_similar]), , drop = FALSE]
              )
            ) / 1000
            coords_studied <- st_coordinates(x = studied_point_LAEA) # Coordinates of studied point
            coords_hung = st_coordinates(x = all_points_LAEA[as.numeric(hungarian_point_ids[index_most_similar]), , drop = FALSE]) # Coordinates of Hungarian point
            lat_dist <- abs(coords_studied[1, 2] - coords_hung[1, 2]) / 1000 # Latitudinal distance in km
            mean_finite_sim <- mean(x = similarity_column[finite_check]) # Mean similarity of all finite Hungarian points
            return(matrix( # Returning 1-row matrix with 5 columns
              data = c(
                as.numeric(hungarian_point_ids[index_most_similar]),
                best_similarity,
                geo_dist,
                lat_dist,
                mean_finite_sim
              ),
              nrow = 1,
              ncol = 5
            ))
          }
        }
      )
      results_matrix <- do.call(what = rbind, args = results_list) # Binding 1-row matrices into multi-row matrix
      results_df <- as.data.frame(x = results_matrix) # Converting matrix to data frame
      colnames(x = results_df) <- c( # Setting column names
        "ID_of_most_similar_hungarian_point",
        "similarity_of_most_similar_hungarian_point",
        "geographical_distance_of_most_similar_hungarian_point",
        "latitudinal_distance_of_most_similar_hungarian_point",
        "mean_similarity_of_all_hungarian_points"
      )
      combined_df <- cbind.data.frame(results_df, geometry = st_geometry(all_points)) # Adding geometry column from all points
      analogy_sf  <- st_as_sf(x = combined_df) # Converting data frame to sf
      rownames(x = analogy_sf) <- point_ids # Setting row names
      return(analogy_sf) # Returning sf object
    }
  ),
  nm = info$climate_targets # Setting names
)

future_scenarios <- setdiff(x = info$climate_targets, y = "reference") # Identifying future scenarios only
all_future_values <- c() # Initializing vector for future similarities
for (scen in future_scenarios) {
  sf_obj <- analogy_of_all_points_to_hungarian_points[[scen]] # Extracting sf object
  sim_col <- sf_obj$similarity_of_most_similar_hungarian_point # Extracting similarity column
  all_future_values <- c(all_future_values, sim_col[is.finite(x = sim_col)]) # Combining finite values
}
threshold <- quantile(x = all_future_values, probs = 0.75, na.rm = TRUE) # Computing 75th percentile
cat("75th percentile threshold = ", threshold, "\n") # Printing threshold - 75th percentile threshold = 0.5913886
for (scen in future_scenarios) { # Creating above threshold column for each future scenario
  sf_obj <- analogy_of_all_points_to_hungarian_points[[scen]] # Extracting sf object
  sim_col <- sf_obj$similarity_of_most_similar_hungarian_point # Extracting similarity column
  sf_obj$above_threshold <- sim_col >= threshold # Creating logical column
  analogy_of_all_points_to_hungarian_points[[scen]] <- sf_obj # Updating list element
}

base_scen <- future_scenarios[1] # Base scenario for reference
base_sf <- analogy_of_all_points_to_hungarian_points[[base_scen]] # Extracting sf for base scenario
n_points <- nrow(x = base_sf) # Number of points
n_scen <- length(x = future_scenarios) # Number of future scenarios
similarity_matrix <- matrix(data = NA_real_, nrow = n_points, ncol = n_scen) # Initializing similarity matrix
for (j in seq_along(along.with = future_scenarios)) {
  scen_name <- future_scenarios[j] # Future scenario name
  sf_obj <- analogy_of_all_points_to_hungarian_points[[scen_name]] # Extracting sf object
  sim_col <- sf_obj$similarity_of_most_similar_hungarian_point # Extracting similarity column
  similarity_matrix[, j] <- sim_col # Filling matrix column
}
mean_future_similarity <- rowMeans(x = similarity_matrix, na.rm = TRUE) # Calculating row means
analog_matrix <- similarity_matrix >= threshold # Logical matrix for threshold
count_of_analog_scenarios <- rowSums(x = analog_matrix, na.rm = TRUE) # Counting how many scenarios are above threshold
mean_sf <- base_sf # Copying base sf
mean_sf$mean_future_similarity <- mean_future_similarity # Adding mean similarity column
mean_sf$count_of_analog_scenarios <- count_of_analog_scenarios # Adding count column

output_folder <- "output/Task10" # Defining output folder
dir.create(path = output_folder, showWarnings = FALSE, recursive = TRUE) # Creating folder
analogy_points_file <- file.path(output_folder, "analogy_of_all_points_to_hungarian_points.RData")
save(x = analogy_of_all_points_to_hungarian_points, file = analogy_points_file) # Saving analogy points
mean_sf_file <- file.path(output_folder, "mean_sf.RData")
save(x = mean_sf, file = mean_sf_file) # Saving mean sf

countries <- st_transform(x = countries, crs = st_crs(x = all_points_LAEA)) # Transforming countries to LAEA
analogy_of_hungarian_points_to_all_points <- setNames( # Creating list of analogy of Hungarian points to all points
  object = lapply(
    X = info$climate_targets, # Iterating through climate targets
    FUN = function(climate_target) {
      selected_similarities <- similarities[[climate_target]] # Copying similarities
      selected_similarities <- st_drop_geometry(x = selected_similarities) # Dropping geometry
      results_list <- future_lapply( # Creating list of 1-row matrices
        X = hungarian_point_ids, # Iterating through Hungarian point IDs
        FUN = function(point_id) {
          studied_point_LAEA <- all_points_LAEA[point_id, , drop = FALSE] # Extracting the specific Hungarian point
          similarity_row <- unlist(x = selected_similarities[point_id, ]) # Similarity values for point
          hungarian_cols <- match(x = paste0("X", hungarian_point_ids), table = names(x = similarity_row)) # Matching Hungarian IDs to columns
          similarity_row_foreign <- similarity_row # Copying the similarity row
          similarity_row_foreign[hungarian_cols] <- NA # Setting Hungarian points to NA
          finite_check <- is.finite(x = similarity_row) # Check for finite similarities
          finite_check_foreign <- is.finite(x = similarity_row_foreign) # Check finite for foreign
          if (!any(finite_check) && !any(finite_check_foreign)) {
            return(matrix( # Returning matrix if no finite similarity
              data = rep(x = NA_real_, times = 12),
              nrow = 1,
              ncol = 12
            ))
          }
          index_most_similar <- NA_integer_  # Index for most similar point
          best_similarity <- NA_real_
          geo_dist <- NA_real_
          lat_dist <- NA_real_
          mean_finite_sim <- NA_real_
          country_name <- NA_character_
          if (any(finite_check)) {
            index_most_similar <- which.max(x = similarity_row) # Finding most similar point among all
            best_similarity <- similarity_row[index_most_similar] # Best similarity value
            geo_dist <- as.numeric(
              st_distance(
                x = studied_point_LAEA,
                y = all_points_LAEA[index_most_similar, , drop = FALSE]
              )
            ) / 1000 # Distance in km
            coords_studied <- st_coordinates(x = studied_point_LAEA) # Coordinates of studied point
            coords_point   <- st_coordinates(x = all_points_LAEA[index_most_similar, , drop = FALSE]) # Coordinates of most similar point
            lat_dist <- abs(coords_studied[1, 2] - coords_point[1, 2]) / 1000 # Latitudinal distance in km
            mean_finite_sim <- mean(x = similarity_row[finite_check]) # Mean similarity of all finite points
            most_similar_point <- all_points_LAEA[index_most_similar, , drop = FALSE] # Geometry of most similar point
            country_name <- as.character(NA)
            country_match <- st_join(x = most_similar_point, y = countries, left = FALSE) # Finding country of most similar point
            if (nrow(x = country_match) > 0) {
              country_name <- as.character(country_match$NAME_ENGL[1]) # Extracting country name
            }
          }
          index_most_similar_foreign <- NA_integer_ # Index for most similar foreign point
          best_similarity_foreign <- NA_real_
          geo_dist_foreign <- NA_real_
          lat_dist_foreign <- NA_real_
          mean_finite_sim_foreign <- NA_real_
          country_name_foreign <- NA_character_
          if (any(finite_check_foreign)) {
            index_most_similar_foreign <- which.max(x = similarity_row_foreign) # Finding most similar foreign point
            best_similarity_foreign <- similarity_row_foreign[index_most_similar_foreign] # Best foreign similarity
            geo_dist_foreign <- as.numeric(
              st_distance(
                x = studied_point_LAEA,
                y = all_points_LAEA[index_most_similar_foreign, , drop = FALSE]
              )
            ) / 1000 # Distance in km
            coords_studied <- st_coordinates(x = studied_point_LAEA) # Coordinates of studied point
            coords_point_f <- st_coordinates(x = all_points_LAEA[index_most_similar_foreign, , drop = FALSE]) # Coordinates of foreign point
            lat_dist_foreign <- abs(coords_studied[1, 2] - coords_point_f[1, 2]) / 1000 # Latitudinal distance in km
            mean_finite_sim_foreign <- mean(x = similarity_row_foreign[finite_check_foreign]) # Mean foreign similarity
            most_similar_foreign_sf <- all_points_LAEA[index_most_similar_foreign, , drop = FALSE] # Geometry of most similar foreign point
            country_match_foreign <- st_join( # Joining country data for foreign point
              x = most_similar_foreign_sf,
              y = countries,
              left = FALSE
            )
            if (nrow(x = country_match_foreign) > 0) {
              country_name_foreign <- as.character(country_match_foreign$NAME_ENGL[1]) # Extracting country name
            }
          }
          return(data.frame( # Returning data frame with 12 columns
            ID_of_most_similar_point = as.numeric(index_most_similar),
            similarity_of_most_similar_point = best_similarity,
            geographical_distance_of_most_similar_point = geo_dist,
            latitudinal_distance_of_most_similar_point = lat_dist,
            mean_similarity_of_all_points = mean_finite_sim,
            country_of_the_most_similar_point = country_name,
            ID_of_most_similar_foreign_point = as.numeric(index_most_similar_foreign),
            similarity_of_most_similar_foreign_point = best_similarity_foreign,
            geographical_distance_of_most_similar_foreign_point = geo_dist_foreign,
            latitudinal_distance_of_most_similar_foreign_point = lat_dist_foreign,
            mean_similarity_of_all_foreign_points = mean_finite_sim_foreign,
            country_of_the_most_similar_foreign_point = country_name_foreign,
            stringsAsFactors = FALSE
          ))
        },
        future.seed = TRUE # Ensuring proper parallel-safe random numbers
      )
      results_matrix <- do.call(what = rbind, args = results_list) # Binding 1-row matrices into multi-row matrix
      results_df <- as.data.frame(x = results_matrix) # Converting to data frame
      colnames(x = results_df) <- c( # Setting column names
        "ID_of_most_similar_point",
        "similarity_of_most_similar_point",
        "geographical_distance_of_most_similar_point",
        "latitudinal_distance_of_most_similar_point",
        "mean_similarity_of_all_points",
        "country_of_the_most_similar_point",
        "ID_of_most_similar_foreign_point",
        "similarity_of_most_similar_foreign_point",
        "geographical_distance_of_most_similar_foreign_point",
        "latitudinal_distance_of_most_similar_foreign_point",
        "mean_similarity_of_all_foreign_points",
        "country_of_the_most_similar_foreign_point"
      )
      combined_df <- cbind.data.frame(results_df, geometry = st_geometry(hungarian_points)) # Adding geometry from Hungarian points
      analogy_sf  <- st_as_sf(x = combined_df) # Converting to sf
      rownames(x = analogy_sf) <- hungarian_point_ids # Setting row names
      return(analogy_sf) # Return sf object
    }
  ),
  nm = info$climate_targets # Setting names
)

analogy_hungarian_points_file <- file.path(output_folder, "analogy_of_hungarian_points_to_all_points.RData")
save(x = analogy_of_hungarian_points_to_all_points, file = analogy_hungarian_points_file) # Saving analogy object
plan(strategy = "sequential") # Reverting to sequential processing
