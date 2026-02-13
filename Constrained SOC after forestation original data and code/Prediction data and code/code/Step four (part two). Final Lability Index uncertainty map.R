#Final Lability Index uncertainty map derived from CV
# Load necessary libraries
library(raster)
library(terra) # Alternative if using newer terra package

# Option 1: Using raster package
# Read the SDLI raster file
sdl_raster <- raster("F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\current\\current\\spreading\\LI_sub_CV.tif")



# Calculate CVLI using your formula: CV_LI = sqrt(exp(SD_LI^2) - 1) * 100
cvli_raster <- sqrt(exp(sdl_raster^2) - 1) * 100

# Write the result to a new file
writeRaster(cvli_raster, "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\current\\current\\spreading\\CVLI_sub.tif", format = "GTiff", overwrite = TRUE)

rm(list = ls())


#################LI CV cleaning and clipping###############

library(terra)


in_dir <- "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading"
out_dir <- "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading\\clean"
if (!dir.exists(out_dir)) dir.create(out_dir)

tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("processing:", basename(f), "\n")
  
  r <- rast(f)
  r[r < 0] <- -0
  r[r > 200]  <- 200
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}
