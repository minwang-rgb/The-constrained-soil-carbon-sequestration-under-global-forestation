# ==============================================================================
# BIOME PLOT ANALYSIS: TYPE 1(Tropical/Subtropical Forests), 3(Temperate Forests), & 6(Boreal Forests)
# ==============================================================================

# --- 1. Load Libraries ---
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)

# --- 2. Directory Setup ---
# Input and Base Directory
input_dir <- "F:\\model\\results\\sixth\\futuremapping\\projected"# Alternatively, use all the future prediction maps
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
files     <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))

# Output Directories for Plots, Rasters, and Stats
out_dir         <- "F:\\model\\results\\sixth\\futuremapping\\projected\\biomeplot_1_3_6"
transformed_dir <- "F:\\model\\results\\sixth\\futuremapping\\projected\\transformed"
stats_dir       <- "F:\\model\\results\\sixth\\futuremapping\\projected\\statistics_1_3_6"
plot_data_dir   <- "F:\\model\\results\\sixth\\futuremapping\\projected\\plot_data_1_3_6"

# Create directories if they do not exist
if (!dir.exists(out_dir))         dir.create(out_dir, recursive = TRUE)
if (!dir.exists(transformed_dir)) dir.create(transformed_dir, recursive = TRUE)
if (!dir.exists(stats_dir))       dir.create(stats_dir, recursive = TRUE)
if (!dir.exists(plot_data_dir))   dir.create(plot_data_dir, recursive = TRUE)

# --- 3. Configuration & Metadata ---
# Load the Biome Classification Map
biome_rast <- rast("F:/BaiduNetdiskDownload/input/biome_projected.h5")

# Define Levels, Labels, and Visual Palettes
BIOME_LEVELS <- c("Type_1", "Type_3", "Type_6")
BIOME_LABELS <- c("Trop/sub. for", "Tem. for", "Bor. for")
BIOME_COLORS <- c("Trop/sub. for" = "#017A79",  # Lagoon
                  "Tem. for"     = "#D18B6B",# Light Blue
                  "Bor. for"     = "#4A6FA5")   # Yellow

# Initialize lists to store results
all_anova_results     <- list()
all_tukey_results     <- list()
all_descriptive_stats <- list()
all_plot_data         <- list()

# --- 4. Main Processing Loop ---
for (nm in names(files)) {
  
  # 4a. Raster Transformation
  r <- rast(files[[nm]])
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  
  transformed_filename <- file.path(transformed_dir, sprintf("%s_transformed.tif", nm))
  writeRaster(r_transformed, transformed_filename, overwrite = TRUE)
  message("Saved transformed raster to: ", transformed_filename)
  
  # 4b. Data Sampling and Preparation
  set.seed(42)
  cells      <- sample(ncell(r_transformed), min(100000, ncell(r_transformed)))
  vals_soil  <- terra::values(r_transformed)[cells]
  vals_biome <- terra::values(biome_rast)[cells]
  
  df <- data.frame(soil = vals_soil, biome = vals_biome)
  
  # Data Cleaning: Remove NAs, non-finite values, and specific biome types (5, 7, 8)
  df <- df[complete.cases(df), ]
  df <- df[is.finite(df$soil), ] 
  df <- df[!is.na(df$biome) & is.finite(df$biome), ]
  df <- df[!(df$biome %in% c(5, 8, 7)), ]
  
  # Recode Biome types to specific groups
  df <- df %>%
    mutate(
      biome_group = case_when(
        biome == 1 ~ "Type_1",
        biome == 3 ~ "Type_3",
        biome == 6 ~ "Type_6",
        TRUE ~ as.character(biome)
      )
    ) %>%
    filter(biome_group %in% BIOME_LEVELS) 
  
  df$biome_group <- factor(df$biome_group, levels = BIOME_LEVELS, labels = BIOME_LABELS)
  
  # 4c. Outlier Removal (based on 5th and 95th quantiles)
  if (nrow(df) > 0) {
    df_clean <- df %>%
      group_by(biome_group) %>%
      mutate(
        Q1 = quantile(soil, 0.05, na.rm = TRUE),
        Q3 = quantile(soil, 0.95, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR,
        is_extreme = soil < lower_bound | soil > upper_bound
      ) %>%
      ungroup()
    
    extreme_count <- sum(df_clean$is_extreme, na.rm = TRUE)
    df <- df_clean %>%
      filter(!is_extreme) %>%
      select(soil, biome_group)
    
    message(sprintf("  removed %d outliers (%.2f%%)", 
                    extreme_count, extreme_count/nrow(df_clean)*100))
  }
  
  df <- df[!is.na(df$biome_group), ]
  df$biome_group <- droplevels(df$biome_group)
  
  valid_groups <- unique(df$biome_group)
  if (length(valid_groups) < 2) {
    message("Warning: Not enough valid biome groups for file: ", nm)
    next
  }
  
  # Store plot data
  plot_data <- df
  plot_data$file <- nm
  all_plot_data[[nm]] <- plot_data
  
  # 4d. Statistical Testing: ANOVA
  df_aov <- df %>% filter(biome_group %in% valid_groups)
  df_aov$biome_group <- droplevels(df_aov$biome_group)
  
  anova_result  <- aov(soil ~ biome_group, data = df_aov)
  anova_summary <- summary(anova_result)
  p_value       <- round(anova_summary[[1]]$'Pr(>F)'[1], 2)
  
  # Save ANOVA summary
  anova_df <- data.frame(
    File = nm,
    Df = anova_summary[[1]]$Df[1],
    Sum_Sq = round(anova_summary[[1]]$'Sum Sq'[1], 4),
    Mean_Sq = round(anova_summary[[1]]$'Mean Sq'[1], 4),
    F_value = round(anova_summary[[1]]$'F value'[1], 4),
    P_value = p_value,
    Significance = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
  all_anova_results[[nm]] <- anova_df
  
  # 4e. Post-hoc Analysis: Tukey HSD
  tukey_df  <- NULL
  letter_df <- NULL
  
  if (p_value < 0.05) {
    tukey_result  <- TukeyHSD(anova_result)
    tukey_letters <- multcompLetters4(anova_result, tukey_result)
    letter_df     <- data.frame(biome_group = names(tukey_letters$biome_group$Letters),
                                Letters     = tukey_letters$biome_group$Letters)
    
    stat_vals <- df %>%
      group_by(biome_group) %>%
      summarise(
        max_val = max(soil, na.rm = TRUE),
        q3_val  = quantile(soil, 0.95, na.rm = TRUE)
      ) %>%
      mutate(biome_group = as.character(biome_group))
    
    letter_df <- merge(letter_df, stat_vals, by = "biome_group")
    
    tukey_df <- as.data.frame(tukey_result$biome_group)
    tukey_df$Comparison <- rownames(tukey_df)
    tukey_df$File       <- nm
    tukey_df <- tukey_df[, c("File", "Comparison", "diff", "lwr", "upr", "p adj")]
    all_tukey_results[[nm]] <- tukey_df
  }
  
  # 4f. Descriptive Statistics
  descriptive_stats <- df %>%
    group_by(biome_group) %>%
    summarise(
      n      = n(),
      mean   = round(mean(soil, na.rm = TRUE), 4),
      median = round(median(soil, na.rm = TRUE), 4),
      sd     = round(sd(soil, na.rm = TRUE), 4),
      min    = round(min(soil, na.rm = TRUE), 4),
      max    = round(max(soil, na.rm = TRUE), 4),
      q25    = round(quantile(soil, 0.25, na.rm = TRUE), 4),
      q75    = round(quantile(soil, 0.75, na.rm = TRUE), 4)
    ) %>%
    mutate(File = nm) %>%
    select(File, biome_group, everything())
  
  all_descriptive_stats[[nm]] <- descriptive_stats
  
  # 4g. Data Visualization (ggplot2)
  p <- ggplot(df, aes(x = biome_group, y = soil)) +
    geom_boxplot(
      aes(fill = biome_group),   
      color = "black",           
      outlier.shape = NA,        
      size = 1.2,                
      na.rm = TRUE,        
      alpha = 0.8            
    ) +
    stat_summary(
      fun = mean, geom = "point", shape = 18, size = 15, color = "red", na.rm = TRUE          
    ) +
    stat_summary(
      fun = median, geom = "point", shape = 15, size = 15, color = "blue", na.rm = TRUE               
    ) +
    scale_fill_manual(values = BIOME_COLORS) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0)), labels = BIOME_LABELS) + 
    theme_classic() +
    labs(x = "", y = "") +
    annotate("text", x = Inf, y = Inf, label = paste("p =", sprintf("%.2f", p_value)),
             hjust = 1.1, vjust = 1.5, size = 25, fontface = "bold", color = "black") +
    theme(
      axis.line         = element_line(size = 1.6, color = "grey40"), 
      axis.text         = element_text(size = 80),             
      axis.text.x       = element_text(hjust = 0.5, size = 80, angle = 0),  
      axis.text.y       = element_text(size = 80, margin = margin(r = 10)), 
      plot.title        = element_blank(),                                              
      legend.position   = "none",                                                       
      axis.ticks        = element_line(size = 1.5, color = "black"),      
      axis.ticks.length = unit(0.3, "cm"),                                         
      axis.ticks.x      = element_line(size = 1.5, color = "black"),    
      axis.ticks.y      = element_line(size = 1.5, color = "black")     
    )
  
  # Add Tukey significance letters if applicable
  if (!is.null(letter_df) && p_value < 0.05) {
    p <- p + 
      geom_text(data = letter_df, 
                aes(x = biome_group, y = q3_val * 1.15, label = Letters),
                size = 30, fontface = "bold", color = "black", vjust = -0.5,
                na.rm = TRUE) +  
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) 
  } else {
    p <- p + scale_y_continuous(expand = c(0, 0))
  }
  
  # Save the plot
  outfn <- file.path(out_dir, sprintf("%s_by_Biome_Type1_3_6_colored.tif", nm))
  ggsave(
    filename = outfn, plot = p, device = "tiff", dpi = 300,
    width = 18, height = 14, units = "in", compression = "lzw"
  )
  
  # Logging messages
  message("File: ", nm)
  message("ANOVA p-value: ", p_value)
  message("Valid data points: ", nrow(df))
  if (exists("tukey_result")) print(tukey_result)
  message("Saved plot to: ", outfn)
  message("----------------------------------------")
  
  # Cleanup loop variables
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
}

# --- 5. Export Combined Statistics ---

# Export ANOVA
if (length(all_anova_results) > 0) {
  anova_combined <- do.call(rbind, all_anova_results)
  write.csv(anova_combined, file.path(stats_dir, "ANOVA_results_biome_Type1_3_6_colored.csv"), row.names = FALSE)
}

# Export Tukey HSD
if (length(all_tukey_results) > 0) {
  tukey_combined <- do.call(rbind, all_tukey_results)
  write.csv(tukey_combined, file.path(stats_dir, "Tukey_HSD_results_biome_Type1_3_6_colored.csv"), row.names = FALSE)
}

# Export Descriptive Stats
if (length(all_descriptive_stats) > 0) {
  descriptive_combined <- do.call(rbind, all_descriptive_stats)
  write.csv(descriptive_combined, file.path(stats_dir, "Descriptive_statistics_biome_Type1_3_6_colored.csv"), row.names = FALSE)
}

# Export Plotting Data
if (length(all_plot_data) > 0) {
  plot_data_combined <- do.call(rbind, all_plot_data)
  write.csv(plot_data_combined, file.path(plot_data_dir, "Plot_data_biome_Type1_3_6_colored.csv"), row.names = FALSE)
}

# --- 6. Final Summary ---
overall_summary <- data.frame(
  Total_Files_Processed        = length(files),
  Files_with_Significant_ANOVA = sum(sapply(all_anova_results, function(x) x$P_value < 0.05)),
  Processing_Date              = Sys.Date()
)

write.csv(overall_summary, file.path(stats_dir, "Processing_summary_biome_Type1_3_6_colored.csv"), row.names = FALSE)

message("\n=== ALL BIOME PROCESSING COMPLETED ===")
message("Statistical results saved in: ", stats_dir)

# Clear Environment
rm(list=ls())




# ==============================================================================
# LATITUDE-BASED SPATIAL ANALYSIS & VISUALIZATION
# ==============================================================================
# Load necessary spatial and plotting libraries
library(raster)
library(geodata)
library(ggplot2)
library(terra)
library(rlang)
library(ggspatial)
library(maps)
library(sf)
library(tidyterra)
library(matrixStats)
library(rnaturalearth)

# Define input and output directory paths
input_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
output_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\latitude"

# Create output directory if it doesn't already exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function: Remove outliers using the Interquartile Range (IQR) method
# Uses the 5th and 95th percentiles as the basis for the range
remove_outliers_iqr <- function(x, multiplier = 1.5) {
  values <- x[!is.na(x)]
  if (length(values) == 0) {
    return(rep(NA, length(x)))
  }
  q1 <- quantile(values, 0.05, na.rm = TRUE)
  q3 <- quantile(values, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr
  result <- x
  result[x < lower_bound | x > upper_bound] <- NA
  return(result)
}

# List all GeoTIFF files in the input directory
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# Process each file in the loop
for (file_path in tif_files) {
  file_name <- tools::file_path_sans_ext(basename(file_path))
  raster_data <- rast(file_path)
  
  # Check if the raster contains data
  if (ncell(raster_data) == 0) {
    message(paste("Skipping", file_name, ": No data cells"))
    next
  }
  
  # Apply exponential transformation to convert log-scale to percentage change
  message(paste("Applying transformation: (exp(r)-1)*100 to", file_name))
  raster_transformed <- (exp(raster_data) - 1) * 100
  
  # Calculate statistics for the transformed data
  trans_values <- values(raster_transformed)
  trans_min <- min(trans_values, na.rm = TRUE)
  trans_max <- max(trans_values, na.rm = TRUE)
  trans_mean <- mean(trans_values, na.rm = TRUE)
  
  message(paste("Transformed data range:", round(trans_min, 4), "to", round(trans_max, 4)))
  message(paste("Transformed mean:", round(trans_mean, 4)))
  
  # Convert raster to matrix for row-wise outlier cleaning
  raster_matrix <- as.matrix(raster_transformed, wide = TRUE)
  cleaned_matrix <- matrix(NA, nrow = nrow(raster_matrix), ncol = ncol(raster_matrix))
  
  # Iterate through rows to clean outliers based on latitude
  for (i in 1:nrow(raster_matrix)) {
    row_data <- raster_matrix[i, ]
    cleaned_row <- remove_outliers_iqr(row_data, multiplier = 1.5)
    cleaned_matrix[i, ] <- cleaned_row
  }
  
  # Prepare data frame for plotting: Latitude vs. Mean values & SD
  dat <- data.frame(
    lat = yFromRow(raster_data),  
    row_means = rowMeans(cleaned_matrix, na.rm = TRUE),
    row_sds = apply(cleaned_matrix, 1, sd, na.rm = TRUE) 
  )
  
  # Calculate data loss statistics after cleaning
  original_means <- rowMeans(raster_matrix, na.rm = TRUE)
  original_sds <- apply(raster_matrix, 1, sd, na.rm = TRUE)
  total_values <- sum(!is.na(raster_matrix))
  removed_values <- total_values - sum(!is.na(cleaned_matrix))
  removal_percentage <- (removed_values / total_values) * 100
  
  message(paste("File:", file_name))
  message(paste("Removed", removed_values, "outliers (", 
                round(removal_percentage, 2), "% of total values)"))
  
  # Remove rows with NAs and validate if data remains
  dat <- na.omit(dat)
  if (nrow(dat) == 0) {
    message(paste("Skipping", file_name, ": No valid data after outlier removal and NA removal"))
    next
  }
  
  # Format labels for the Y-axis based on file names
  y_label <- gsub("_", " ", file_name)
  y_label <- paste0(toupper(substr(y_label, 1, 1)), substring(y_label, 2))
  y_label <- paste0(y_label, " (%)")
  
  # Create the plot
  p <- ggplot(dat, aes(y = row_means, x = lat)) +     
    # Add error ribbon (Standard Deviation)
    geom_ribbon(aes(ymin = row_means - row_sds, 
                    ymax = row_means + row_sds), 
                fill = "lightgrey", alpha = 0.5) +  
    # Add the main mean line
    geom_line(size = 2, color = "orange") + 
    labs(x = "Latitude (°)", y = y_label) + 
    theme_classic() +                
    coord_flip() + # Flip coordinates so Latitude is on the vertical axis
    scale_y_continuous(limits = c(min(dat$row_means, na.rm = TRUE), 
                                  max(dat$row_means, na.rm = TRUE))) + 
    # Styling for large-format display
    theme(
      axis.text = element_text(size = 90, color = "black"),  
      axis.title = element_text(size = 90, color = "black"), 
      axis.title.x = element_blank(), 
      plot.title = element_blank(), 
      panel.grid = element_blank(), 
      axis.line = element_line(color = "black", size = 1.5), 
      panel.background = element_rect(fill = "white", colour = "black", size = 1.5), 
      plot.margin = margin(30, 30, 30, 30) 
    )
  
  # Save the final plot as a high-resolution TIFF
  output_file <- file.path(output_dir, paste0(file_name, "_latitude_plot_transformed_iqr_cleaned.tiff"))
  ggsave(output_file, plot = p, width = 15, height = 14, device = "tiff", dpi = 300)
  
  # Output final processing messages
  message(paste("Saved:", output_file))
  message(paste("Transformed cleaned data range: Lat", round(min(dat$lat, na.rm = TRUE), 2), "to", 
                round(max(dat$lat, na.rm = TRUE), 2)))
  message(paste("Transformed cleaned value range:", round(min(dat$row_means, na.rm = TRUE), 4), "to", 
                round(max(dat$row_means, na.rm = TRUE), 4), "%"))
  message(paste("Transformed cleaned mean value:", round(mean(dat$row_means, na.rm = TRUE), 4), "%"))
  message("----------------------------------------")
}

message("All plots have been processed with transformation and IQR outlier removal!")

# ==============================================================================
# LAND USE SPATIAL ANALYSIS AND STATISTICAL PLOTTING
# ==============================================================================
# This script analyzes raster data based on land use types, performs 
# ANOVA/Tukey HSD tests, and generates high-resolution boxplots.
# ==============================================================================

# Load required libraries for spatial data, plotting, and statistics
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)

# --- Path Configuration ---
landuse_path <- "F:\\BaiduNetdiskDownload\\input\\Landuse_type_projected.h5"
if (!file.exists(landuse_path)) {
  stop("FATAL ERROR: Landuse file not found at: ", landuse_path)
}

message("Reading Landuse raster...")
landuse <- rast(landuse_path)

input_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
if (!dir.exists(input_dir)) {
  stop("FATAL ERROR: Input TIF directory does not exist: ", input_dir)
}

# Identify all raster files for analysis
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) {
  stop("FATAL ERROR: No TIF files found in the input directory.")
}

files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))
message(sprintf("Found %d TIF files to process.", length(files)))

# Setup output directories for plots and CSV statistics
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\landuseplot"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

stats_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\\\statistics"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

# Initialize lists to store combined results
all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()

# --- Main Processing Loop ---
for (nm in names(files)) {
  message("\n--- Starting processing for file: ", nm, " ---")
  
  tryCatch({
    r <- rast(files[[nm]])
  }, error = function(e) {
    message("Error reading raster: ", files[[nm]])
    message("Skipping this file.")
    next
  })
  
  # Data Transformation: Convert values to percentage using (exp(r)-1)*100
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  
  # Random Sampling: Extract 100k cells for statistical efficiency
  set.seed(42)
  n_cells <- ncell(r_transformed)
  cells <- sample(n_cells, min(100000, n_cells))
  vals_soil <- terra::values(r_transformed)[cells]
  vals_landuse <- terra::values(landuse)[cells]
  
  df <- data.frame(
    soil = vals_soil,
    landuse = vals_landuse
  )
  
  # Data Cleaning: Remove NAs, non-finite values, and specific landuse classes (5 & 8)
  df <- na.omit(df)
  df <- df[is.finite(df$soil) & is.finite(df$landuse), ] 
  df <- df[!(df$landuse %in% c(5, 8)), ]
  
  df$landuse <- factor(df$landuse)
  df$landuse <- droplevels(df$landuse) 
  
  # Ensure no NA levels remain in factors
  if (any(is.na(levels(df$landuse)))) {
    message("Warning: Landuse factor levels contained NA. Re-cleaning.")
    df$landuse <- factor(df$landuse, exclude = NA)
    df <- df[!is.na(df$landuse), ]
    df$landuse <- droplevels(df$landuse)
  }
  
  # Outlier Removal: IQR-based cleaning per landuse group
  initial_rows <- nrow(df)
  if (initial_rows > 0) {
    df_clean <- df %>%
      group_by(landuse) %>%
      mutate(
        Q1 = quantile(soil, 0.05, na.rm = TRUE),
        Q3 = quantile(soil, 0.95, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR,
        is_extreme = soil < lower_bound | soil > upper_bound
      ) %>%
      ungroup()
    
    df <- df_clean %>%
      filter(!is_extreme) %>%
      select(soil, landuse)
    
    extreme_count <- initial_rows - nrow(df)
    
    message(sprintf("removed %d outliers (%.2f%% of initial valid points)", 
                    extreme_count, extreme_count / initial_rows * 100))
  }
  
  # Check for data sufficiency after cleaning
  if (nrow(df) < 5 || length(unique(df$landuse)) < 2) {
    message("Warning: Insufficient valid data or landuse groups (<2) after cleaning for file: ", nm)
    next
  }
  
  message("Land-use type: ", paste(sort(unique(df$landuse)), collapse = ", "))
  message("available data: ", nrow(df))
  
  # --- Statistical Testing: ANOVA ---
  anova_result <- aov(soil ~ landuse, data = df)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 5) 
  anova_df <- data.frame(
    File = nm,
    Df = anova_summary[[1]]$Df[1],
    Sum_Sq = round(anova_summary[[1]]$'Sum Sq'[1], 4),
    Mean_Sq = round(anova_summary[[1]]$'Mean Sq'[1], 4),
    F_value = round(anova_summary[[1]]$'F value'[1], 4),
    P_value = p_value,
    Significance = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
  all_anova_results[[nm]] <- anova_df
  
  # --- Post-hoc Testing: Tukey HSD ---
  tukey_df <- NULL
  letter_df <- NULL
  
  if (p_value < 0.05) {
    if (all(table(df$landuse) > 1)) {
      tukey_result <- TukeyHSD(anova_result)
      tukey_letters <- multcompLetters4(anova_result, tukey_result)
      # Extract significance letters for plotting
      letter_df <- data.frame(landuse = names(tukey_letters$landuse$Letters),
                              Letters = tukey_letters$landuse$Letters)
      stat_vals <- df %>%
        group_by(landuse) %>%
        summarise(
          q3_val = quantile(soil, 0.75, na.rm = TRUE)
        ) %>%
        mutate(landuse = as.character(landuse))
      letter_df <- merge(letter_df, stat_vals, by = "landuse")
      
      # Prepare Tukey summary table
      tukey_df <- as.data.frame(tukey_result$landuse)
      tukey_df$Comparison <- rownames(tukey_df)
      tukey_df$File <- nm
      tukey_df <- tukey_df[, c("File", "Comparison", "diff", "lwr", "upr", "p adj")]
      all_tukey_results[[nm]] <- tukey_df
    } else {
      message("Skipping Tukey HSD: Not enough data points in all landuse groups.")
    }
  }
  
  # Calculate Descriptive Statistics
  descriptive_stats <- df %>%
    group_by(landuse) %>%
    summarise(
      n = n(),
      mean = round(mean(soil, na.rm = TRUE), 4),
      median = round(median(soil, na.rm = TRUE), 4),
      sd = round(sd(soil, na.rm = TRUE), 4),
      min = round(min(soil, na.rm = TRUE), 4),
      max = round(max(soil, na.rm = TRUE), 4),
      q25 = round(quantile(soil, 0.05, na.rm = TRUE), 4),
      q75 = round(quantile(soil, 0.95, na.rm = TRUE), 4)
    ) %>%
    mutate(File = nm) %>%
    select(File, landuse, everything())
  
  all_descriptive_stats[[nm]] <- descriptive_stats
  
  # --- Visualization ---
  ylab <- paste0(gsub("_", " ", nm), " (%)")
  
  p <- ggplot(df, aes(x = landuse, y = soil)) +
    # Color boxplots by the median value
    geom_boxplot(
      aes(fill = after_stat(middle)), 
      outlier.size = 1.4,
      alpha = 0.8,
      size = 1.2,
      na.rm = TRUE
    ) +
    # Mark the mean with a diamond shape
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 10, 
      color = "black",
      na.rm = TRUE
    ) +
    scale_fill_gradient(
      low  = "#E0F7FA",
      high = "#006064",
      name = "Median (%)"
    ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0)), na.translate = FALSE) + 
    theme_classic() +
    labs(x = "", y = ylab) +
    # Display P-value on plot
    annotate("text", 
             x = Inf, y = Inf, 
             label = paste("p =", sprintf("%.5f", p_value)),
             hjust = 1.1, vjust = 1.5, 
             size = 25, 
             fontface = "bold", 
             color = "black") +
    theme(
      axis.line       = element_line(size = 1.6, color = "grey40"), 
      axis.text       = element_text(size = 80), 
      axis.text.x     = element_text(hjust = 0.5, size = 80, angle = 0), 
      axis.text.y     = element_text(size = 80, margin = margin(r = 10)), 
      axis.title.y    = element_text(size = 80, face = "bold", margin = margin(r = 20)), 
      legend.position = "right",
      legend.title    = element_text(size = 60, face = "bold", margin = margin(b = 10)), 
      legend.text     = element_text(size = 50), 
      legend.key.height = unit(3, "cm"), 
      legend.key.width = unit(1, "cm"), 
      axis.ticks = element_line(size = 1.5, color = "black"), 
      axis.ticks.length = unit(0.3, "cm"), 
      axis.ticks.x = element_line(size = 1.5, color = "black"), 
      axis.ticks.y = element_line(size = 1.5, color = "black") 
    )
  
  # Add Tukey significance letters if applicable
  if (!is.null(letter_df) && p_value < 0.05) {
    p <- p + 
      geom_text(data = letter_df, 
                aes(x = landuse, y = q3_val * 1.15, label = Letters),
                size = 30, fontface = "bold", color = "black", vjust = -0.5,
                na.rm = TRUE) + 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))
  } else {
    p <- p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
  }
  
  # Save plot as high-quality TIFF
  outfn <- file.path(out_dir, sprintf("%s_by_Landuse.tif", nm))
  ggsave(
    filename = outfn,
    plot = p,
    device = "tiff",
    dpi = 300,
    width = 24,
    height = 16,
    units = "in",
    compression = "lzw"
  )
  
  # Log details to console
  message("ANOVA p-value: ", p_value)
  message("Transformed value range: ", round(min(df$soil, na.rm = TRUE), 4), 
          " to ", round(max(df$soil, na.rm = TRUE), 4), "%")
  message("Mean transformed value: ", round(mean(df$soil, na.rm = TRUE), 4), "%")
  message("Saved plot to: ", outfn)
  message("----------------------------------------")
  
  # Clean up temporary objects for memory management
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
  rm(r, r_transformed, df, df_clean, anova_result, anova_summary)
}

# --- Export Combined Statistical Results ---
if (length(all_anova_results) > 0) {
  anova_combined <- do.call(rbind, all_anova_results)
  write.csv(anova_combined, 
            file.path(stats_dir, "ANOVA_results_combined.csv"), 
            row.names = FALSE)
  message("ANOVA results saved to: ", file.path(stats_dir, "ANOVA_results_combined.csv"))
}

if (length(all_tukey_results) > 0) {
  tukey_combined <- do.call(rbind, all_tukey_results)
  write.csv(tukey_combined, 
            file.path(stats_dir, "Tukey_HSD_results_combined.csv"), 
            row.names = FALSE)
  message("Tukey HSD results saved to: ", file.path(stats_dir, "Tukey_HSD_results_combined.csv"))
}

if (length(all_descriptive_stats) > 0) {
  descriptive_combined <- do.call(rbind, all_descriptive_stats)
  write.csv(descriptive_combined, 
            file.path(stats_dir, "Descriptive_statistics_combined.csv"), 
            row.names = FALSE)
  message("Descriptive statistics saved to: ", file.path(stats_dir, "Descriptive_statistics_combined.csv"))
}

# Final Processing Summary
overall_summary <- data.frame(
  Total_Files_Found = length(files),
  Total_Files_Processed = length(all_anova_results),
  Files_with_Significant_ANOVA = sum(sapply(all_anova_results, function(x) x$P_value < 0.05)),
  Processing_Date = Sys.Date()
)

write.csv(overall_summary, 
          file.path(stats_dir, "Processing_summary.csv"), 
          row.names = FALSE)
message("Processing summary saved to: ", file.path(stats_dir, "Processing_summary.csv"))

message("\n=== ALL PROCESSING COMPLETED SUCCESSFULLY ===")
message("Plots saved in: ", out_dir)
message("Statistical results saved in: ", stats_dir)

# Clear workspace
rm(list=ls())