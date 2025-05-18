# Creator: Roman Korolkov
# Date: 03 Mar 2025
# Version: 04
# Description: The script calculates sigma similarity and masks point pairs.
# Duration: Approximately 4 hours
# Saved files size: ~ 420 MB

if (!requireNamespace("sf")) install.packages("sf") # Installing all needed packages
if (!requireNamespace("adehabitatLT")) install.packages("adehabitatLT")
if (!requireNamespace("future")) install.packages("future")
if (!requireNamespace("future.apply")) install.packages("future.apply")
library(sf)  
library(adehabitatLT) 
library(future)
library(future.apply)

setwd("D:/Climate_Research") # Setting working directory
info_file <- "output/Task1/info.RData" # loading info file
load(file = info_file)
mask_file <- "output/Task6/mask_of_hungarian_points.RData" # Hungarian points mask file
load(file = mask_file)
point_ids_file <- "output/Task8/point_ids.RData" # point IDs
load(file = point_ids_file)
interannual_file <- "output/Task8/interannual_variability.RData" # interannual variability
load(file = interannual_file)
specific_climate_file <- "output/Task7/specific_climate_periods.RData" # specific climate periods
load(file = specific_climate_file)
bioclimatic_file <- "output/Task7/bioclimatic_variables.RData" # bioclimatic variables
load(file = bioclimatic_file)
yearly_climate_points_file <- "output/Task8/yearly_climate_points.RData" # yearly climate points
load(file = yearly_climate_points_file)

all_points <- st_as_sf(yearly_climate_points[[1]][, 0])  # Creating a new sf object and removing all data columns
hungarian_points <- all_points[mask_of_hungarian_points, ]  # Creating a new sf object and keeping only Hungarian points
hungarian_point_ids <- as.character(which(mask_of_hungarian_points))  # Creating character vector with Hungarian points IDs
plan("multisession")  # Enabling multisession processing
options(future.globals.maxSize = Inf)  # Removing memory size limit

mask_of_comparable_point_pairs <- setNames( # Creating mask of comparable point pairs
  object = lapply(
    X = info$climate_targets,  # Iterating through nine climate targets
    FUN = function(climate_target) {
      scps_studied <- as.matrix(st_drop_geometry(specific_climate_periods[[climate_target]]))  # Creating studied SCPs
      scps_reference <- as.matrix(st_drop_geometry(specific_climate_periods[["reference"]])[mask_of_hungarian_points, ])  # Creating reference SCPs
      comparable_list <- setNames( # Creating comparable list
        object = future_lapply(
          X = point_ids,  # Iterating through point IDs
          FUN = function(point_id) {
            studied_index <- as.numeric(point_id) # Converting point ID to numeric index for studied SCPs
            comp_vector <- sapply( # Creating logical vector over reference points
              X = 1:nrow(scps_reference),  # Iterating through each Hungarian reference point
              FUN = function(ref_index) {
                differences <- abs(as.numeric(scps_studied[studied_index, ]) - as.numeric(scps_reference[ref_index, ])) # Calculating absolute differences
                circular_diff <- pmin(differences, 12 - differences)  # Calculating circular differences
                all(circular_diff <= 2)  # TRUE if all differences <= 2
              }
            )
            return(comp_vector)  # Returning logical vector
          }
        ),
        nm = point_ids  # Setting names
      )
      comp_df <- as.data.frame(comparable_list)  # Converting list to dataframe
      comp_sf <- st_as_sf(cbind(comp_df, geometry = st_geometry(hungarian_points)))  # Adding geometry and converting to sf object
      rownames(comp_sf) <- hungarian_point_ids  # Assigning row names
      message("<< Finished mask for climate_target = ", climate_target)
      return(comp_sf)  # Returning sf object for climate target
    }
  ),
  nm = info$climate_targets  # Setting names
)

output_folder <- "output/Task9"  # Defining output folder
dir.create(path = output_folder, showWarnings = FALSE, recursive = TRUE)  # Creating folder
mask_output_file <- file.path(output_folder, "mask_of_comparable_point_pairs.RData")
save(mask_of_comparable_point_pairs, file = mask_output_file) # Saving mask
all_points_file <- file.path(output_folder, "all_points.RData")
save(all_points, file = all_points_file)  # Saving all points
hungarian_ids_file <- file.path(output_folder, "hungarian_point_ids.RData")
save(hungarian_point_ids, file = hungarian_ids_file) # Saving Hungarian point IDs

similarities <- setNames( # Creating similarities list
  object = lapply(
    X = info$climate_targets,  # Iterating through nine climate targets
    FUN = function(climate_target) {
      bcvs_studied <- st_drop_geometry(bioclimatic_variables[[climate_target]]) # Creating studied BCVs
      bcvs_reference <- st_drop_geometry(bioclimatic_variables[["reference"]])[mask_of_hungarian_points, ]  # Creating reference BCVs
      similarity_list <- setNames( # Creating similarity list
        object = future_lapply(
          X = point_ids, # Iterating through point IDs
          FUN = function(point_id) {
            interannual_variability_of_studied_point <- interannual_variability[[point_id]]  # Extracting interannual variability
            standardized_bcvs_studied <- sweep(x = bcvs_studied[as.numeric(point_id), , drop = FALSE], MARGIN = 2, 
                                               STATS = interannual_variability_of_studied_point$sd_of_BCVs, FUN = "/")  # Creating standardized studied BCVs
            standardized_bcvs_reference <- sweep(x = bcvs_reference, MARGIN = 2, 
                                                 STATS = interannual_variability_of_studied_point$sd_of_BCVs, FUN = "/")  #Creating  standardized reference BCVs
            pc_scores_studied <- as.data.frame(predict(object = interannual_variability_of_studied_point$PCA, newdata = standardized_bcvs_studied))  # Calculating PCA scores for studied point
            pc_scores_reference <- as.data.frame(predict(object = interannual_variability_of_studied_point$PCA, newdata = standardized_bcvs_reference))  # Calculating PCA scores for reference points
            num_important <- interannual_variability_of_studied_point$no_of_important_PCs  # number of important PCs
            standardized_pc_scores_studied <- sweep(x = pc_scores_studied[, 1:num_important, drop = FALSE], 
                                                    MARGIN = 2, STATS = interannual_variability_of_studied_point$sd_of_important_PCs, FUN = "/")  # Standardized studied PC scores
            standardized_pc_scores_reference <- sweep(x = pc_scores_reference[, 1:num_important, drop = FALSE], 
                                                      MARGIN = 2, STATS = interannual_variability_of_studied_point$sd_of_important_PCs, FUN = "/")  # Standardized reference PC scores
            euclidean_distances <- sapply( # Creating euclidean distances
              X = 1:nrow(standardized_pc_scores_reference),  # Iterating through reference points
              FUN = function(i) {
                sqrt(sum((unlist(standardized_pc_scores_studied) - unlist(standardized_pc_scores_reference[i, , drop = TRUE]))^2)) # Calculating distances
              }
            )
            percentiles <- pchi(euclidean_distances, df = num_important)  # Calculating percentiles
            sigma_dissimilarities <- suppressWarnings(qchi(percentiles, df = 1))  # Calculating sigma dissimilarities
            sigma_dissimilarities[is.nan(sigma_dissimilarities)] <- Inf  # Replacing NaN with Inf
            sigma_similarities <- 1 / (1 + sigma_dissimilarities)  # Calculating similarity values
            return(sigma_similarities)  # Returning similarity vector
          }
        ),
        nm = point_ids  # Setting names
      )
      similarity_df <- as.data.frame(similarity_list)  # Converting to dataframe
      mask_matrix <- as.matrix(st_drop_geometry(mask_of_comparable_point_pairs[[climate_target]]))  # Getting mask matrix
      similarity_df_masked <- similarity_df  # Copying similarity dataframe
      similarity_df_masked[!mask_matrix] <- NA  # Setting values to NA where mask is FALSE
      similarity_sf <- st_as_sf(cbind(similarity_df_masked, geometry = st_geometry(hungarian_points)))  # Adding geometry
      rownames(similarity_sf) <- hungarian_point_ids  # Assigning row names
      message("<< Finished similarities for climate_target = ", climate_target)
      return(similarity_sf)  # Returning sigma similarity sf object
    }
  ),
  nm = info$climate_targets  # Setting names
)
similarities_file <- file.path(output_folder, "similarities.RData")
save(similarities, file = similarities_file)  # Saving similarities
plan("sequential")  # Reverting to sequential processing
writeLines("Script execution completed.")  # Completion message