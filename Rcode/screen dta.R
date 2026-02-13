library(tidyverse)
library(raster)
library(sp)
library(dplyr)
library(readxl)


# 设置工作目录
setwd("E:/extreme_climate/three_models_data")

# 加载必要的包
library(readxl)
library(raster)
library(dplyr)

# 读取坐标点数据
points_samp <- read_excel("COR.xlsx")

# 设置要提取的tif文件目录
tif_dir <- "E:/extreme_climate/output"

# 获取目录下所有的tif文件
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

# 创建一个空的数据框来存储所有提取结果
all_results <- data.frame(ID = 1:nrow(points_samp))

# 循环处理每个tif文件
for (tif_file in tif_files) {
  # 获取文件名（不含路径和扩展名）
  file_name <- tools::file_path_sans_ext(basename(tif_file))
  
  cat("正在处理:", file_name, "\n")
  
  # 读取栅格数据
  raster_data <- raster(tif_file)
  
  # 提取值
  extracted_values <- raster::extract(raster_data, points_samp)
  
  # 处理缺失值和0值（使用与您原始代码相同的逻辑）
  if (any(is.na(extracted_values) | extracted_values == 0)) {
    a <- which(is.na(extracted_values) | extracted_values == 0)
    a1 <- raster::extract(raster_data, points_samp[a,], buffer = 5000)
    a2 <- array()
    
    for (i in 1:length(a1)) {
      a2[i] <- a1[[i]][a1[[i]] != 0] %>% na.omit() %>% mean()
    }
    extracted_values[a] <- a2
  }
  
  # 如果还有缺失值或0值，使用更大的缓冲区
  if (any(is.na(extracted_values) | extracted_values == 0)) {
    b <- which(is.na(extracted_values) | extracted_values == 0)
    a3 <- raster::extract(raster_data, points_samp[b,], buffer = 10000)
    a4 <- array()
    
    for (i in 1:length(a3)) {
      a4[i] <- a3[[i]][a3[[i]] != 0] %>% na.omit() %>% mean()
    }
    extracted_values[b] <- a4
  }
  
  # 将结果添加到数据框中
  all_results[[file_name]] <- extracted_values
}

# 保存所有结果到一个CSV文件
write.csv(all_results, "all_tif_extracted_values.csv", row.names = FALSE)

# 同时保存每个文件的单独CSV（可选）
for (col_name in names(all_results)[-1]) {  # 跳过ID列
  single_result <- data.frame(values = all_results[[col_name]])
  write.csv(single_result, paste0(col_name, "_extracted.csv"), row.names = FALSE)
}

cat("所有tif文件处理完成！\n")
























##########SOC screen data############


# Load required libraries
requiredPackages <- c('car', 'ggplot2', 'tidyr',
                      'parallel', 'lubridate', 'corrplot', 'rlang',
                      'future.apply', 'lightgbm', 'dplyr', 'caret',
                      'SHAPforxgboost', 'xgboost', 'data.table',
                      'pdp', 'doParallel', 'foreach')

for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Set working directory

#active模型部分######
# Load dataset

setwd("F:/model")
df0 <- read.csv("dataset_SOC0318without_purpose_no wetland.csv") %>% as.data.frame()


V1 <- "SOC" # 'passive', 'SOC',"active"
df0 <- df0 %>%
  filter(Var1 == V1) %>%
  dplyr::select(-Var1)
colnames(df0)[1] <- "yi"

# Rename columns
df <- plyr::rename(df0,
                   c("Longitude"  = "Lon",
                     "Latitude" = "Lat",
                     "Soil.layer" = "Soillayer",
                     "Agenumber" = "Age",
                     "Recovery.mode" = "Recovmode",
                     "Vege.model" = "Vegetype",
                     "Landuse"= "LUtype"
                     
                   ))

target_var <- "yi"

# Convert categorical variables to numeric
char2num <- function(data) {
  factor(data) %>% as.numeric()
}



df$Soillayer <- as.numeric(as.character(df$Soillayer))

df$LUtype <- as.numeric(as.character(df$LUtype))
#df$Recovmode <- char2num(df$Recovmode)
df$Recovmode <- as.numeric(as.character(df$Recovmode))
#df$Purpose <- char2num(df$Purpose)
#df$Vegetype<- char2num(df$Vegetype)
df$Vegetype <- as.numeric(as.character(df$Vegetype))
#df$Altitude<- char2num(df$Altitude)
df$Altitude <- as.numeric(as.character(df$Altitude))
str(df)

# Convert ALL columns to numeric (including those already numeric for consistency)
df <- df %>%
  mutate(across(everything(), ~ {
    # First convert to character to handle any factor variables
    x <- as.character(.)
    
    # Then convert to numeric, suppressing warnings about NAs
    suppressWarnings(as.numeric(x))
  }))

# For the bio* variables that were integers, ensure they become proper numeric
bio_cols <- grep("^bio", names(df), value = TRUE)
df[bio_cols] <- lapply(df[bio_cols], as.numeric)

# Verify the conversion
str(df)  # Should show all columns as numeric now


# Remove columns with >1% missing values
df <- df[, sapply(df, function(x) mean(is.na(x)) <= 0.1)]
df <- df %>% filter(if_all(everything(), is.finite))

# Remove high VIF variables (Multicollinearity Check)
remove_high_vif <- function(data, response_var, threshold = 5) {
  removed_vars <- c()
  repeat {
    formula <- as.formula(paste(response_var, "~ ."))
    lm_model <- lm(formula, data = data)
    vif_values <- vif(lm_model)
    max_vif <- max(vif_values)
    
    if (max_vif < threshold) break
    
    remove_var <- names(which.max(vif_values))
    removed_vars <- c(removed_vars, remove_var)
    data <- data[, !names(data) %in% remove_var]
  }
  return(list(data = data, removed_vars = removed_vars))
}

# Identify and remove collinear variables
lin_combos <- findLinearCombos(df[, -which(names(df) == "yi")])
if (length(lin_combos$remove) > 0) {
  df <- df[, -lin_combos$remove]
}

# Apply VIF-based feature selection
result <- remove_high_vif(df, response_var = "yi")
data.predictor.VIF <- setdiff(names(result$data), "yi")
df.clean <- result$data
#save(df.clean, file = 'df.clean.yi.Rda')
write.csv(df.clean, file = "df.clean.yi.csv", row.names = FALSE)  # Save as CSV
#save(data.predictor.VIF, file = 'data.predictor.VIF.yi.Rda')
write.csv(data.predictor.VIF, file = "data.predictor.VIF.yi.csv", row.names = FALSE)  # Save as CSV



















setwd("F:/model")

#LI model
# Load required libraries
requiredPackages <- c('car', 'ggplot2', 'tidyr',
                      'parallel', 'lubridate', 'corrplot', 'rlang',
                      'future.apply', 'lightgbm', 'dplyr', 'caret',
                      'SHAPforxgboost', 'xgboost', 'data.table',
                      'pdp', 'doParallel', 'foreach')

for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Set working directory

#active模型部分######
# Load dataset
setwd("F:/model")
df0 <- read.csv("20205SIKOoutli0115.csv") %>% as.data.frame()

V1 <- "yi"



# Rename columns
df <- plyr::rename(df0,
                   c("Longitude"  = "Lon",
                     "Latitude" = "Lat",
                     "Soil.layer" = "Soillayer",
                     "Agenumber" = "Age",
                     "Recovery.mode" = "Recovmode",
                     "Vege.model" = "Vegetype",
                     "Landuse"= "LUtype"
                     
                   ))

target_var <- "yi"

# Convert categorical variables to numeric
char2num <- function(data) {
  factor(data) %>% as.numeric()
}



df$Soillayer <- char2num(df$Soillayer)
df$LUtype <- char2num(df$LUtype)
df$Recovmode <- char2num(df$Recovmode)
#df$Purpose <- char2num(df$Purpose)
df$Vegetype<- char2num(df$Vegetype)
df$Altitude<- char2num(df$Altitude)

str(df)

# Convert ALL columns to numeric (including those already numeric for consistency)
df <- df %>%
  mutate(across(everything(), ~ {
    # First convert to character to handle any factor variables
    x <- as.character(.)
    
    # Then convert to numeric, suppressing warnings about NAs
    suppressWarnings(as.numeric(x))
  }))

# For the bio* variables that were integers, ensure they become proper numeric
bio_cols <- grep("^bio", names(df), value = TRUE)
df[bio_cols] <- lapply(df[bio_cols], as.numeric)

# Verify the conversion
str(df)  # Should show all columns as numeric now




# Remove columns with >1% missing values
df <- df[, sapply(df, function(x) mean(is.na(x)) <= 0.1)]
df <- df %>% filter(if_all(everything(), is.finite))

# Remove high VIF variables (Multicollinearity Check)
remove_high_vif <- function(data, response_var, threshold = 5) {
  removed_vars <- c()
  repeat {
    formula <- as.formula(paste(response_var, "~ ."))
    lm_model <- lm(formula, data = data)
    vif_values <- vif(lm_model)
    max_vif <- max(vif_values)
    
    if (max_vif < threshold) break
    
    remove_var <- names(which.max(vif_values))
    removed_vars <- c(removed_vars, remove_var)
    data <- data[, !names(data) %in% remove_var]
  }
  return(list(data = data, removed_vars = removed_vars))
}

# Identify and remove collinear variables
lin_combos <- findLinearCombos(df[, -which(names(df) == "yi")])
if (length(lin_combos$remove) > 0) {
  df <- df[, -lin_combos$remove]
}

# Apply VIF-based feature selection
result <- remove_high_vif(df, response_var = "yi")
data.predictor.VIF <- setdiff(names(result$data), "yi")
df.clean <- result$data
#save(df.clean, file = 'df.clean.yi.Rda')
write.csv(df.clean, file = "df.clean.yi.csv", row.names = FALSE)  # Save as CSV
#save(data.predictor.VIF, file = 'data.predictor.VIF.yi.Rda')
write.csv(data.predictor.VIF, file = "data.predictor.VIF.yi.csv", row.names = FALSE)  # Save as CSV





















####################projection################
# 批量设置 TIFF 文件投影的 R 代码 - 最终修正版
# 修复了字符串连接错误和计数逻辑

# 2. 加载包
library(terra)
library(sf)

# 3. 设置路径
#input_folder <- "F:/model/results/futuremapping"
#output_folder <- "F:/model/results/futuremapping/projected"


input_folder <- "F:/model/results/clean"
output_folder <- "F:/model/results/sixth/current_mapping/projected"
reference_file <- "F:/model/results/AI.tif"

# 4. 创建输出文件夹
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("已创建输出文件夹:", output_folder, "\n")
}

# 5. 获取参考文件的投影信息
cat("读取参考文件:", reference_file, "\n")
ref_raster <- try(rast(reference_file), silent = TRUE)

if (inherits(ref_raster, "try-error")) {
  if (!require("raster")) install.packages("raster")
  library(raster)
  ref_raster <- raster(reference_file)
  ref_crs <- crs(ref_raster)
  use_terra <- FALSE
} else {
  ref_crs <- crs(ref_raster)
  use_terra <- TRUE
}

# 简化CRS字符串用于gdalwarp
# 使用EPSG:4326（WGS84）的简写形式
ref_crs_short <- "EPSG:4326"
cat("参考坐标系:", ref_crs_short, "\n\n")

# 6. 获取需要处理的TIFF文件
tif_files <- list.files(input_folder, 
                        pattern = "\\.tif$", 
                        full.names = TRUE,
                        ignore.case = TRUE)

# 排除参考文件和已处理的文件
tif_files <- tif_files[!grepl(basename(reference_file), basename(tif_files), fixed = TRUE)]
cat("找到", length(tif_files), "个TIFF文件需要处理\n")

# 7. 全新的批量处理函数 - 简化版
batch_project_simple <- function(input_files, output_dir, target_crs_epsg) {
  success_count <- 0
  fail_count <- 0
  failed_files <- c()
  success_files <- c()
  
  for (i in seq_along(input_files)) {
    input_file <- input_files[i]
    file_name <- basename(input_file)
    output_file <- file.path(output_dir, file_name)
    
    cat(sprintf("处理文件 %d/%d: %s\n", i, length(input_files), file_name))
    
    # 方法1: 直接使用系统调用gdalwarp（最可靠）
    if (Sys.which("gdalwarp") != "") {
      cat("  使用gdalwarp处理...\n")
      cmd <- sprintf('gdalwarp -t_srs "%s" "%s" "%s" -overwrite',
                     target_crs_epsg, input_file, output_file)
      
      system_result <- try(system(cmd, intern = TRUE, ignore.stderr = FALSE), silent = TRUE)
      
      if (!inherits(system_result, "try-error") && file.exists(output_file)) {
        success_count <- success_count + 1
        success_files <- c(success_files, file_name)
        cat("  ✓ gdalwarp处理成功\n")
      } else {
        # 方法2: 尝试gdal_translate（仅分配投影，不重采样）
        cat("  gdalwarp失败，尝试gdal_translate...\n")
        cmd2 <- sprintf('gdal_translate -a_srs "%s" "%s" "%s" -co COMPRESS=LZW',
                        target_crs_epsg, input_file, output_file)
        
        system_result2 <- try(system(cmd2, intern = TRUE, ignore.stderr = FALSE), silent = TRUE)
        
        if (!inherits(system_result2, "try-error") && file.exists(output_file)) {
          success_count <- success_count + 1
          success_files <- c(success_files, file_name)
          cat("  ✓ gdal_translate处理成功\n")
        } else {
          # 方法3: 尝试使用R包处理
          cat("  GDAL工具失败，尝试使用R包...\n")
          tryCatch({
            # 读取文件
            if (use_terra) {
              input_raster <- rast(input_file)
              # 设置投影
              crs(input_raster) <- target_crs_epsg
              # 保存文件
              writeRaster(input_raster, output_file, overwrite = TRUE)
            } else {
              input_raster <- raster(input_file)
              # 设置投影
              crs(input_raster) <- target_crs_epsg
              # 保存文件
              writeRaster(input_raster, output_file, overwrite = TRUE)
            }
            
            if (file.exists(output_file)) {
              success_count <- success_count + 1
              success_files <- c(success_files, file_name)
              cat("  ✓ R包处理成功\n")
            } else {
              fail_count <- fail_count + 1
              failed_files <- c(failed_files, file_name)
              cat("  ✗ 所有方法都失败\n")
            }
          }, error = function(e) {
            fail_count <- fail_count + 1
            failed_files <- c(failed_files, file_name)
            cat("  ✗ R包处理失败:", e$message, "\n")
          })
        }
      }
    } else {
      # 直接使用R包处理
      cat("  gdalwarp不可用，直接使用R包...\n")
      tryCatch({
        # 读取文件
        if (use_terra) {
          input_raster <- rast(input_file)
          # 设置投影
          crs(input_raster) <- target_crs_epsg
          # 保存文件
          writeRaster(input_raster, output_file, overwrite = TRUE)
        } else {
          input_raster <- raster(input_file)
          # 设置投影
          crs(input_raster) <- target_crs_epsg
          # 保存文件
          writeRaster(input_raster, output_file, overwrite = TRUE)
        }
        
        if (file.exists(output_file)) {
          success_count <- success_count + 1
          success_files <- c(success_files, file_name)
          cat("  ✓ R包处理成功\n")
        } else {
          fail_count <- fail_count + 1
          failed_files <- c(failed_files, file_name)
          cat("  ✗ R包处理失败\n")
        }
      }, error = function(e) {
        fail_count <- fail_count + 1
        failed_files <- c(failed_files, file_name)
        cat("  ✗ R包处理失败:", e$message, "\n")
      })
    }
    cat("---\n")
  }
  
  return(list(
    success = success_count,
    failed = fail_count,
    success_files = success_files,
    failed_files = failed_files
  ))
}

# 8. 执行批量处理
cat("开始批量投影处理...\n")
results <- batch_project_simple(tif_files, output_folder, ref_crs_short)

cat("\n")
cat(rep("=", 50), sep = "")
cat("\n")
cat("处理完成！\n")
cat("成功处理:", results$success, "个文件\n")
cat("处理失败:", results$failed, "个文件\n")

if (length(results$success_files) > 0) {
  cat("\n成功处理的文件:\n")
  for (file in results$success_files) {
    cat("  ✓", file, "\n")
  }
}

if (length(results$failed_files) > 0) {
  cat("\n失败的文件:\n")
  for (file in results$failed_files) {
    cat("  ✗", file, "\n")
  }
}

# 9. 详细验证处理结果
cat("\n")
cat(rep("=", 50), sep = "")
cat("\n")
cat("详细验证处理结果:\n")

if (dir.exists(output_folder)) {
  output_files <- list.files(output_folder, 
                             pattern = "\\.tif$", 
                             full.names = TRUE)
  
  if (length(output_files) > 0) {
    cat("输出文件夹中的文件:\n")
    
    for (file in output_files) {
      file_name <- basename(file)
      cat(sprintf("\n文件: %s\n", file_name))
      
      # 检查文件是否存在且可读
      if (file.exists(file)) {
        file_size <- file.info(file)$size / 1024^2  # MB
        cat(sprintf("  大小: %.2f MB\n", file_size))
        
        # 尝试读取投影信息
        tryCatch({
          if (use_terra) {
            r <- try(rast(file), silent = TRUE)
            if (inherits(r, "try-error")) {
              r <- raster(file)
            }
          } else {
            r <- raster(file)
          }
          
          # 获取CRS
          file_crs <- crs(r)
          
          if (!is.na(file_crs) && !is.null(file_crs) && file_crs != "") {
            cat("  投影: 已设置\n")
            
            # 检查是否是WGS84
            if (grepl("WGS 84|WGS84|4326", file_crs, ignore.case = TRUE)) {
              cat("  类型: WGS 84 (与AI.tif匹配) ✅\n")
            } else {
              cat("  类型: 其他坐标系\n")
              # 只显示前100个字符
              crs_str <- as.character(file_crs)
              if (nchar(crs_str) > 100) {
                crs_str <- paste0(substr(crs_str, 1, 100), "...")
              }
              cat("  详细信息:", crs_str, "\n")
            }
          } else {
            cat("  投影: 未设置或无法识别 ⚠️\n")
          }
          
          # 显示基本信息
          if (inherits(r, "RasterLayer") || inherits(r, "SpatRaster")) {
            cat(sprintf("  尺寸: %d行 × %d列\n", nrow(r), ncol(r)))
            cat(sprintf("  范围: x(%.4f, %.4f), y(%.4f, %.4f)\n",
                        xmin(r), xmax(r), ymin(r), ymax(r)))
          }
          
        }, error = function(e) {
          cat("  状态: 无法读取文件内容 ❌\n")
          cat("  错误:", e$message, "\n")
        })
      } else {
        cat("  状态: 文件不存在\n")
      }
    }
  } else {
    cat("输出文件夹中没有TIFF文件\n")
  }
}

# 10. 检查文件是否能在ArcGIS中正常使用
cat("\n")
cat(rep("=", 50), sep = "")
cat("\n")
cat("ArcGIS兼容性检查:\n")
cat("1. 在ArcGIS中打开AI.tif，确认其投影正确\n")
cat("2. 逐个添加处理后的文件，检查:\n")
cat("   - 是否出现'missing spatial reference'警告\n")
cat("   - 是否能与AI.tif正确叠加\n")
cat("   - 是否有偏移或变形\n")
cat("3. 如果有问题，尝试在ArcGIS中使用'Define Projection'工具\n")

# 11. 生成最终报告
report_file <- file.path(output_folder, "projection_final_report.txt")
sink(report_file)
cat("TIFF文件批量投影处理最终报告\n")
cat("生成时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("参考文件:", reference_file, "\n")
cat("目标投影:", ref_crs_short, "\n")
cat("输入文件夹:", input_folder, "\n")
cat("输出文件夹:", output_folder, "\n\n")

cat("处理统计:\n")
cat("总文件数:", length(tif_files), "\n")
cat("成功处理:", results$success, "\n")
cat("处理失败:", results$failed, "\n\n")

if (length(results$success_files) > 0) {
  cat("成功文件列表:\n")
  for (file in results$success_files) {
    cat("- ", file, "\n", sep = "")
  }
  cat("\n")
}

if (length(results$failed_files) > 0) {
  cat("失败文件列表:\n")
  for (file in results$failed_files) {
    cat("- ", file, "\n", sep = "")
  }
  cat("\n")
}

# 检查输出文件
output_files <- list.files(output_folder, pattern = "\\.tif$")
cat("输出文件夹中的文件 (", length(output_files), "个):\n", sep = "")
for (file in output_files) {
  file_path <- file.path(output_folder, file)
  if (file.exists(file_path)) {
    size_mb <- file.info(file_path)$size / 1024^2
    cat(sprintf("- %s (%.2f MB)\n", file, size_mb))
  } else {
    cat(sprintf("- %s (不存在)\n", file))
  }
}
sink()

cat("\n")
cat(rep("=", 50), sep = "")
cat("\n")
cat("最终报告已保存到:", report_file, "\n")
cat("输出文件位于:", output_folder, "\n")

# 12. 如果仍有问题，提供手动解决方案
if (results$failed > 0) {
  cat("\n对于处理失败的文件，请尝试以下手动方法:\n")
  cat("1. 使用ArcGIS的'Define Projection'工具:\n")
  cat("   a. 在ArcToolbox中搜索'Define Projection'\n")
  cat("   b. 选择输入文件\n")
  cat("   c. 坐标系选择'GCS_WGS_1984' (EPSG:4326)\n")
  cat("   d. 运行工具\n\n")
  
  cat("2. 使用QGIS:\n")
  cat("   a. 打开QGIS\n")
  cat("   b. 加载文件\n")
  cat("   c. 右键图层 → 导出 → 另存为\n")
  cat("   d. 在CRS中选择'WGS 84 (EPSG:4326)'\n")
  cat("   e. 保存新文件\n")
}

# 13. 打开输出文件夹（Windows系统）
if (.Platform$OS.type == "windows") {
  shell.exec(output_folder)
  cat("\n已打开输出文件夹\n")
}

# 14. 最终状态汇总
cat("\n")
cat(rep("=", 50), sep = "")
cat("\n")
cat("最终状态汇总:\n")
cat("输入文件总数:", length(tif_files), "\n")
cat("输出文件总数:", length(list.files(output_folder, pattern = "\\.tif$")), "\n")

if (length(tif_files) == results$success) {
  cat("✅ 所有文件处理成功！\n")
} else if (results$success == 0) {
  cat("❌ 所有文件处理失败！请检查GDAL安装或文件权限。\n")
} else {
  cat("⚠️  部分文件处理失败，请查看上面的失败文件列表。\n")
}










library(terra)

# 输入输出路径
in_dir <- "F:\\model\\results\\projected"
out_dir <- "F:\\model\\results\\clean"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < -1.5] <- -1.5
  r[r > 1.5]  <- 1.5
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}











rm(list=ls())



#biome 146################
# 安装并加载必要的包
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)

# 0. 定义文件路径 - 读取 F:/model/results/raw 所有 tif 文件
input_dir <- "F:\\model\\results\\clean" 
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# 创建文件列表，使用文件名作为键
files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))

# 1. 输出目录 (更新文件名以反映新的分组)
out_dir <- "F:/model/results/biomeplot_1_4_6"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1b. 创建转换后tif文件的输出目录
transformed_dir <- "F:/model/results/transformed"
if (!dir.exists(transformed_dir)) dir.create(transformed_dir, recursive = TRUE)

# 1c. 创建统计结果CSV文件输出目录 (更新文件名以反映新的分组)
stats_dir <- "F:/model/results/statistics_1_4_6"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

# 1d. 创建绘图数据保存目录 (更新文件名以反映新的分组)
plot_data_dir <- "F:/model/results/plot_data_1_4_6"
if (!dir.exists(plot_data_dir)) dir.create(plot_data_dir, recursive = TRUE)

# 2. 只读一次 Biome 栅格
biome_rast <- rast("F:/BaiduNetdiskDownload/input/biome_projected.h5")

# 创建空列表来存储所有文件的统计结果
all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()
all_plot_data <- list()

# 定义新的分组水平 - 修改为1,4,6
BIOME_LEVELS <- c("Type_1", "Type_4", "Type_6")
# 🚩🚩🚩 修改：为每个类型赋予新的名称
BIOME_LABELS <- c("Trop/sub. for", "Tem. for", "Bor. for")

# 定义颜色 - lagoon, yellow, orange
BIOME_COLORS <- c("Trop/sub. for" = "#017A79",  # lagoon
                  "Tem. for" = "lightblue",     # light blue  
                  "Bor. for" = "yellow")        # yellow

# 3. 循环处理每个 soil-layer 文件
for (nm in names(files)) {
  
  # 3a. 读取当前 raster
  r <- rast(files[[nm]])
  
  # 3b. 应用转换公式：exp(tif-1)*100 转换为百分数
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  
  # 3b1. 保存转换后的tif文件
  transformed_filename <- file.path(transformed_dir, sprintf("%s_transformed.tif", nm))
  writeRaster(r_transformed, transformed_filename, overwrite = TRUE)
  message("Saved transformed raster to: ", transformed_filename)
  
  # 3c. 随机采样至多 100k 个像元
  set.seed(42)
  cells <- sample(ncell(r_transformed), min(100000, ncell(r_transformed)))
  
  # 3d. 提取值
  vals_soil  <- terra::values(r_transformed)[cells]
  vals_biome <- terra::values(biome_rast)[cells]
  
  # 3e. 构建 data.frame 并彻底清理数据
  df <- data.frame(
    soil  = vals_soil,
    biome = vals_biome
  )
  
  # 3e1. 彻底移除所有NA和无效值
  df <- df[complete.cases(df), ]  # 移除任何包含NA的行
  df <- df[is.finite(df$soil), ]  # 移除无穷值
  
  # 3e2. 移除biome中的NA、NaN和无效值
  df <- df[!is.na(df$biome) & is.finite(df$biome), ]
  
  # 3e3. 移除特定的biome类型 (5, 8, 7) - 修改为保留1,4,6
  df <- df[!(df$biome %in% c(5, 8, 7)), ]
  
  # 3e4. 🚩🚩🚩 修改：将biome类型 4 和 6 分开，只保留类型 1, 4, 6 
  df <- df %>%
    mutate(
      biome_group = case_when(
        biome == 1 ~ "Type_1",
        biome == 4 ~ "Type_4", # 分开 4
        biome == 6 ~ "Type_6", # 分开 6
        TRUE ~ as.character(biome)
      )
    ) %>%
    # 只保留类型 1, 4, 6
    filter(biome_group %in% BIOME_LEVELS) 
  
  # 3e5. 🚩🚩🚩 修改：将biome_group转换为因子，使用新的 levels 和 labels
  df$biome_group <- factor(df$biome_group, 
                           levels = BIOME_LEVELS,
                           labels = BIOME_LABELS)
  
  # 3e6. 移除极大值和极小值（使用IQR方法）
  if (nrow(df) > 0) {
    # 计算每个biome组的异常值边界
    df_clean <- df %>%
      group_by(biome_group) %>%
      mutate(
        Q1 = quantile(soil, 0.25, na.rm = TRUE),
        Q3 = quantile(soil, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR,
        is_extreme = soil < lower_bound | soil > upper_bound
      ) %>%
      ungroup()
    
    # 统计移除的异常值数量
    extreme_count <- sum(df_clean$is_extreme, na.rm = TRUE)
    
    # 移除异常值
    df <- df_clean %>%
      filter(!is_extreme) %>%
      select(soil, biome_group)
    
    message(sprintf("  移除了 %d 个异常值 (%.2f%%)", 
                    extreme_count, extreme_count/nrow(df_clean)*100))
  }
  
  # 最终检查：确保没有NA或无效的biome值
  df <- df[!is.na(df$biome_group), ]
  df$biome_group <- droplevels(df$biome_group)  # 移除空的因子水平
  
  # 检查数据是否有效且分组数足够（至少2个分组）
  valid_groups <- unique(df$biome_group)
  if (length(valid_groups) < 2) {
    message("Warning: Not enough valid biome groups (only ", length(valid_groups), ") after cleaning for file: ", nm)
    next
  }
  
  # 保存绘图数据
  plot_data <- df
  plot_data$file <- nm
  all_plot_data[[nm]] <- plot_data
  
  # 输出清理后的biome类型
  message("清理后的biome类型: ", paste(sort(unique(df$biome_group)), collapse = ", "))
  message("Trop/sub. for 数据点: ", sum(df$biome_group == "Trop/sub. for"))
  message("Tem. for 数据点: ", sum(df$biome_group == "Tem. for"))
  message("Bor. for 数据点: ", sum(df$biome_group == "Bor. for"))
  
  # 3f. 进行方差分析 (ANOVA)
  # 确保只有存在的因子水平参与 aov
  df_aov <- df %>% filter(biome_group %in% valid_groups)
  df_aov$biome_group <- droplevels(df_aov$biome_group)
  
  anova_result <- aov(soil ~ biome_group, data = df_aov)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 2)  # 保留两位小数
  
  # 保存ANOVA结果
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
  
  # 3g. 如果ANOVA显著，进行事后检验 (Tukey HSD)
  tukey_df <- NULL
  letter_df <- NULL
  
  if (p_value < 0.05) {
    tukey_result <- TukeyHSD(anova_result)
    
    # 获取显著性字母
    tukey_letters <- multcompLetters4(anova_result, tukey_result)
    letter_df <- data.frame(biome_group = names(tukey_letters$biome_group$Letters),
                            Letters = tukey_letters$biome_group$Letters)
    
    # 计算每个biome类型的最大值和上四分位数，用于放置显著性字母
    stat_vals <- df %>%
      group_by(biome_group) %>%
      summarise(
        max_val = max(soil, na.rm = TRUE),
        q3_val = quantile(soil, 0.75, na.rm = TRUE)
      ) %>%
      mutate(biome_group = as.character(biome_group))
    
    letter_df <- merge(letter_df, stat_vals, by = "biome_group")
    
    # 保存Tukey HSD结果
    tukey_df <- as.data.frame(tukey_result$biome_group)
    tukey_df$Comparison <- rownames(tukey_df)
    tukey_df$File <- nm
    tukey_df <- tukey_df[, c("File", "Comparison", "diff", "lwr", "upr", "p adj")]
    all_tukey_results[[nm]] <- tukey_df
  }
  
  # 3h. 计算描述性统计并保存
  descriptive_stats <- df %>%
    group_by(biome_group) %>%
    summarise(
      n = n(),
      mean = round(mean(soil, na.rm = TRUE), 4),
      median = round(median(soil, na.rm = TRUE), 4),
      sd = round(sd(soil, na.rm = TRUE), 4),
      min = round(min(soil, na.rm = TRUE), 4),
      max = round(max(soil, na.rm = TRUE), 4),
      q25 = round(quantile(soil, 0.25, na.rm = TRUE), 4),
      q75 = round(quantile(soil, 0.75, na.rm = TRUE), 4)
    ) %>%
    mutate(File = nm) %>%
    select(File, biome_group, everything())
  
  all_descriptive_stats[[nm]] <- descriptive_stats
  
  # 4. 绘图：修改为只显示箱线图，使用指定填充颜色
  p <- ggplot(df, aes(x = biome_group, y = soil)) +
    # 🚩🚩🚩 修改：只保留箱线图，移除散点图
    geom_boxplot(
      aes(fill = biome_group),      # 按分组填充颜色
      color = "black",              # 黑色边框
      outlier.shape = NA,           # 不显示箱线图的异常点
      size = 1.2,                   # 增大箱线图线条粗细
      na.rm = TRUE,                 # 确保忽略NA值
      alpha = 0.8                   # 稍微透明
    ) +
    # 添加均值点和中位数点
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 15,                    # 🚩🚩🚩 增大均值点大小
      color = "red",                # 红色均值点
      na.rm = TRUE                  # 确保忽略NA值
    ) +
    # 添加中位数点（箱线图本身的中位数线）
    stat_summary(
      fun = median, 
      geom = "point", 
      shape = 15, 
      size = 15,                     # 🚩🚩🚩 增大中位数点大小
      color = "blue",               # 蓝色中位数点
      na.rm = TRUE                  # 确保忽略NA值
    ) +
    # 🚩🚩🚩 修改：使用指定填充颜色
    scale_fill_manual(values = BIOME_COLORS) +
    # 🚩🚩🚩 修改：自定义x轴标签以反映新的名称
    scale_x_discrete(expand = expansion(add = c(0.5, 0)), 
                     labels = BIOME_LABELS) + 
    theme_classic() +
    labs(
      x = "",                       # 去掉x轴标题
      y = ""                        # 去掉y轴标题
    ) +
    # 添加P值标注 - 🚩🚩🚩 进一步增大字体
    annotate("text", 
             x = Inf, y = Inf, 
             label = paste("p =", sprintf("%.2f", p_value)),
             hjust = 1.1, vjust = 1.5, 
             size = 25,             # 🚩🚩🚩 进一步增大P值文本大小 (15->25)
             fontface = "bold",     # 加粗
             color = "black") +
    theme(
      axis.line       = element_line(size = 1.6, color = "grey40"),  # 增大坐标轴线粗细
      axis.text       = element_text(size = 80),                    # 字体放大
      axis.text.x     = element_text(hjust = 0.5, size = 80, angle = 0),  # x轴文本居中
      axis.text.y     = element_text(size = 80, margin = margin(r = 10)),  # 字体放大
      # 去掉y轴标题
      plot.title      = element_blank(),                           # 去掉图表标题
      legend.position = "none",                                    # 去掉图例
      
      # 增加tick标记的设置
      axis.ticks = element_line(size = 1.5, color = "black"),        # 增大刻度线粗细
      axis.ticks.length = unit(0.3, "cm"),                           # 增加刻度线长度
      axis.ticks.x = element_line(size = 1.5, color = "black"),      # x轴刻度线
      axis.ticks.y = element_line(size = 1.5, color = "black")       # y轴刻度线
    )
  
  # 5. 如果ANOVA显著，添加显著性字母
  if (!is.null(letter_df) && p_value < 0.05) {
    p <- p + 
      geom_text(data = letter_df, 
                aes(x = biome_group, y = q3_val * 1.15, label = Letters),
                size = 30, fontface = "bold", color = "black", vjust = -0.5,
                na.rm = TRUE) +  # 确保忽略NA值
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))  # 增加顶部空间
  } else {
    p <- p + scale_y_continuous(expand = c(0, 0))
  }
  
  # 6. 保存为 TIFF
  # 🚩🚩🚩 修改：更新输出文件名以反映新的分组
  outfn <- file.path(out_dir, sprintf("%s_by_Biome_Type1_4_6_colored.tif", nm))
  ggsave(
    filename    = outfn,
    plot        = p,
    device      = "tiff",
    dpi         = 300,
    width       = 18,  # 稍微调整宽度以适应三个分组
    height      = 14,  # 保持高度
    units       = "in",
    compression = "lzw"
  )
  
  # 7. 输出统计结果
  message("File: ", nm)
  message("ANOVA p-value: ", p_value)
  message("Transformed value range: ", round(min(df$soil, na.rm = TRUE), 4), 
          " to ", round(max(df$soil, na.rm = TRUE), 4), "%")
  message("Mean transformed value: ", round(mean(df$soil, na.rm = TRUE), 4), "%")
  message("Biome groups included: ", paste(sort(unique(df$biome_group)), collapse = ", "))
  message("Valid data points: ", nrow(df))
  if (exists("tukey_result")) {
    message("Tukey HSD results:")
    print(tukey_result)
  }
  message("Saved plot to: ", outfn)
  message("----------------------------------------")
  
  # 清除临时变量
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
}

# 8. 保存所有统计结果为CSV文件
# 🚩🚩🚩 修改：更新保存CSV文件的文件名
# 合并所有ANOVA结果
if (length(all_anova_results) > 0) {
  anova_combined <- do.call(rbind, all_anova_results)
  write.csv(anova_combined, 
            file.path(stats_dir, "ANOVA_results_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("ANOVA results saved to: ", file.path(stats_dir, "ANOVA_results_biome_Type1_4_6_colored.csv"))
}

# 合并所有Tukey HSD结果
if (length(all_tukey_results) > 0) {
  tukey_combined <- do.call(rbind, all_tukey_results)
  write.csv(tukey_combined, 
            file.path(stats_dir, "Tukey_HSD_results_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Tukey HSD results saved to: ", file.path(stats_dir, "Tukey_HSD_results_biome_Type1_4_6_colored.csv"))
}

# 合并所有描述性统计结果
if (length(all_descriptive_stats) > 0) {
  descriptive_combined <- do.call(rbind, all_descriptive_stats)
  write.csv(descriptive_combined, 
            file.path(stats_dir, "Descriptive_statistics_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Descriptive statistics saved to: ", file.path(stats_dir, "Descriptive_statistics_biome_Type1_4_6_colored.csv"))
}

# 保存绘图数据
if (length(all_plot_data) > 0) {
  plot_data_combined <- do.call(rbind, all_plot_data)
  write.csv(plot_data_combined, 
            file.path(plot_data_dir, "Plot_data_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Plot data saved to: ", file.path(plot_data_dir, "Plot_data_biome_Type1_4_6_colored.csv"))
}

# 9. 保存总体汇总统计
overall_summary <- data.frame(
  Total_Files_Processed = length(files),
  Files_with_Significant_ANOVA = sum(sapply(all_anova_results, function(x) x$P_value < 0.05)),
  Processing_Date = Sys.Date()
)

write.csv(overall_summary, 
          file.path(stats_dir, "Processing_summary_biome_Type1_4_6_colored.csv"), 
          row.names = FALSE)
message("Processing summary saved to: ", file.path(stats_dir, "Processing_summary_biome_Type1_4_6_colored.csv"))

message("\n=== ALL BIOME PROCESSING COMPLETED ===")
message("Plots saved in: ", out_dir)
message("Transformed TIFFs saved in: ", transformed_dir)
message("Statistical results saved in: ", stats_dir)
message("Plot data saved in: ", plot_data_dir)

rm(list=ls())


rm(list=ls())

#landuse############
# 安装并加载必要的包
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)



# 检查 Landuse 文件路径
landuse_path <- "F:\\BaiduNetdiskDownload\\input\\Landuse_type_projected.h5"
if (!file.exists(landuse_path)) {
  stop("FATAL ERROR: Landuse file not found at: ", landuse_path)
}

# Landuse 栅格只读一次
message("Reading Landuse raster...")
landuse <- rast(landuse_path)

# 定义输入目录和 TIF 文件列表
input_dir <- "F:\\model\\results\\clean"
if (!dir.exists(input_dir)) {
  stop("FATAL ERROR: Input TIF directory does not exist: ", input_dir)
}

tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) {
  stop("FATAL ERROR: No TIF files found in the input directory.")
}

# 创建文件列表，使用文件名作为键
files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))
message(sprintf("Found %d TIF files to process.", length(files)))

# 1. 输出目录
out_dir <- "F:\\model\\results\\landuseplot"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1c. 创建统计结果CSV文件输出目录
stats_dir <- "F:\\model\\results\\statistics"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

# 创建空列表来存储所有文件的统计结果
all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()


for (nm in names(files)) {
  message("\n--- Starting processing for file: ", nm, " ---")
  
  # 3a. 读取当前 raster
  tryCatch({
    r <- rast(files[[nm]])
  }, error = function(e) {
    message("Error reading raster: ", files[[nm]])
    message("Skipping this file.")
    next
  })
  
  # 3b. 应用转换公式：exp(tif-1)*100 转换为百分数
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  
  # 3c. 随机采样至多 100k 个像元
  set.seed(42)
  n_cells <- ncell(r_transformed)
  cells <- sample(n_cells, min(100000, n_cells))
  
  # 3d. 提取值 
  vals_soil <- terra::values(r_transformed)[cells]
  vals_landuse <- terra::values(landuse)[cells]
  
  # 3e. 构建 data.frame 并彻底清理数据
  df <- data.frame(
    soil = vals_soil,
    landuse = vals_landuse
  )
  
  # 3e1. 彻底移除所有NA和无效值 (包括 soil 和 landuse)
  df <- na.omit(df)
  df <- df[is.finite(df$soil) & is.finite(df$landuse), ] 
  
  # 3e3. 移除特定的landuse类型 (5, 8)
  df <- df[!(df$landuse %in% c(5, 8)), ]
  
  # 3e4. 将landuse转换为因子，并彻底清理因子水平
  df$landuse <- factor(df$landuse)
  df$landuse <- droplevels(df$landuse) # 移除空的因子水平
  
  # 检查 landuse 因子水平是否包含 NA 或 NaN
  if (any(is.na(levels(df$landuse)))) {
    message("Warning: Landuse factor levels contained NA. Re-cleaning.")
    df$landuse <- factor(df$landuse, exclude = NA)
    df <- df[!is.na(df$landuse), ]
    df$landuse <- droplevels(df$landuse)
  }
  
  # 3e5. 移除极大值和极小值（使用IQR方法）
  initial_rows <- nrow(df)
  if (initial_rows > 0) {
    # 计算每个landuse组的异常值边界
    df_clean <- df %>%
      group_by(landuse) %>%
      mutate(
        Q1 = quantile(soil, 0.25, na.rm = TRUE),
        Q3 = quantile(soil, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR,
        is_extreme = soil < lower_bound | soil > upper_bound
      ) %>%
      ungroup()
    
    # 移除异常值
    df <- df_clean %>%
      filter(!is_extreme) %>%
      select(soil, landuse)
    
    extreme_count <- initial_rows - nrow(df)
    
    message(sprintf("  移除了 %d 个异常值 (%.2f%% of initial valid points)", 
                    extreme_count, extreme_count / initial_rows * 100))
  }
  
  # 最终检查数据是否有效
  if (nrow(df) < 5 || length(unique(df$landuse)) < 2) {
    message("Warning: Insufficient valid data or landuse groups (<2) after cleaning for file: ", nm)
    next
  }
  
  message("清理后的landuse类型: ", paste(sort(unique(df$landuse)), collapse = ", "))
  message("有效数据点: ", nrow(df))
  
  # 3f. 进行方差分析 (ANOVA)
  anova_result <- aov(soil ~ landuse, data = df)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 5) # 使用5位小数以提高精度
  
  # 保存ANOVA结果
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
  
  # 3g. 如果ANOVA显著，进行事后检验 (Tukey HSD)
  tukey_df <- NULL
  letter_df <- NULL
  
  if (p_value < 0.05) {
    # 检查是否每个分组都有数据，避免 Tukey 报错
    if (all(table(df$landuse) > 1)) {
      tukey_result <- TukeyHSD(anova_result)
      
      # 获取显著性字母
      tukey_letters <- multcompLetters4(anova_result, tukey_result)
      letter_df <- data.frame(landuse = names(tukey_letters$landuse$Letters),
                              Letters = tukey_letters$landuse$Letters)
      
      # 计算每个landuse类型的上四分位数，用于放置显著性字母
      stat_vals <- df %>%
        group_by(landuse) %>%
        summarise(
          q3_val = quantile(soil, 0.75, na.rm = TRUE)
        ) %>%
        mutate(landuse = as.character(landuse))
      
      letter_df <- merge(letter_df, stat_vals, by = "landuse")
      
      # 保存Tukey HSD结果
      tukey_df <- as.data.frame(tukey_result$landuse)
      tukey_df$Comparison <- rownames(tukey_df)
      tukey_df$File <- nm
      tukey_df <- tukey_df[, c("File", "Comparison", "diff", "lwr", "upr", "p adj")]
      all_tukey_results[[nm]] <- tukey_df
    } else {
      message("Skipping Tukey HSD: Not enough data points in all landuse groups.")
    }
  }
  
  # 3h. 计算描述性统计并保存
  descriptive_stats <- df %>%
    group_by(landuse) %>%
    summarise(
      n = n(),
      mean = round(mean(soil, na.rm = TRUE), 4),
      median = round(median(soil, na.rm = TRUE), 4),
      sd = round(sd(soil, na.rm = TRUE), 4),
      min = round(min(soil, na.rm = TRUE), 4),
      max = round(max(soil, na.rm = TRUE), 4),
      q25 = round(quantile(soil, 0.25, na.rm = TRUE), 4),
      q75 = round(quantile(soil, 0.75, na.rm = TRUE), 4)
    ) %>%
    mutate(File = nm) %>%
    select(File, landuse, everything())
  
  all_descriptive_stats[[nm]] <- descriptive_stats
  
  # 3i. Y 轴标签
  ylab <- paste0(gsub("_", " ", nm), " (%)")
  
  # 4. 绘图
  p <- ggplot(df, aes(x = landuse, y = soil)) +
    geom_boxplot(
      aes(fill = after_stat(middle)), 
      outlier.size = 1.4,
      alpha = 0.8,
      size = 1.2,
      na.rm = TRUE
    ) +
    # 添加均值点
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 10, 
      color = "black",
      na.rm = TRUE
    ) +
    # 使用 lagoon 梯度颜色
    scale_fill_gradient(
      low  = "#E0F7FA",
      high = "#006064",
      name = "Median (%)"
    ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0)), na.translate = FALSE) + 
    theme_classic() +
    labs(
      x     = "",
      y     = ylab # 使用自定义的 Y 轴标签
    ) +
    # 添加P值标注
    annotate("text", 
             x = Inf, y = Inf, 
             label = paste("p =", sprintf("%.5f", p_value)), # 增加P值显示精度
             hjust = 1.1, vjust = 1.5, 
             size = 25, 
             fontface = "bold", 
             color = "black") +
    theme(
      axis.line       = element_line(size = 1.6, color = "grey40"), 
      axis.text       = element_text(size = 80), 
      axis.text.x     = element_text(hjust = 0.5, size = 80, angle = 0), 
      axis.text.y     = element_text(size = 80, margin = margin(r = 10)), 
      axis.title.y    = element_text(size = 80, face = "bold", margin = margin(r = 20)), # 增大Y轴标题
      legend.position = "right",
      legend.title    = element_text(size = 60, face = "bold", margin = margin(b = 10)), 
      legend.text     = element_text(size = 50), 
      legend.key.height = unit(3, "cm"), 
      legend.key.width = unit(1, "cm"), 
      
      # 增加tick标记的设置
      axis.ticks = element_line(size = 1.5, color = "black"), 
      axis.ticks.length = unit(0.3, "cm"), 
      axis.ticks.x = element_line(size = 1.5, color = "black"), 
      axis.ticks.y = element_line(size = 1.5, color = "black") 
    )
  
  # 5. 如果ANOVA显著，添加显著性字母
  if (!is.null(letter_df) && p_value < 0.05) {
    # 增加顶部空间，确保显著性字母能显示
    p <- p + 
      geom_text(data = letter_df, 
                aes(x = landuse, y = q3_val * 1.15, label = Letters),
                size = 30, fontface = "bold", color = "black", vjust = -0.5,
                na.rm = TRUE) + 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))
  } else {
    # 不显著时，也留一些顶部空间
    p <- p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
  }
  
  # 6. 保存为 TIFF - 调整图像尺寸
  outfn <- file.path(out_dir, sprintf("%s_by_Landuse.tif", nm))
  ggsave(
    filename = outfn,
    plot = p,
    device = "tiff",
    dpi = 300,
    width = 24, # 调整宽度
    height = 16, # 调整高度
    units = "in",
    compression = "lzw"
  )
  
  # 7. 输出统计结果
  message("ANOVA p-value: ", p_value)
  message("Transformed value range: ", round(min(df$soil, na.rm = TRUE), 4), 
          " to ", round(max(df$soil, na.rm = TRUE), 4), "%")
  message("Mean transformed value: ", round(mean(df$soil, na.rm = TRUE), 4), "%")
  message("Saved plot to: ", outfn)
  message("----------------------------------------")
  
  # 清除临时变量
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
  rm(r, r_transformed, df, df_clean, anova_result, anova_summary)
}


# 合并所有ANOVA结果
if (length(all_anova_results) > 0) {
  anova_combined <- do.call(rbind, all_anova_results)
  write.csv(anova_combined, 
            file.path(stats_dir, "ANOVA_results_combined.csv"), 
            row.names = FALSE)
  message("ANOVA results saved to: ", file.path(stats_dir, "ANOVA_results_combined.csv"))
}

# 合并所有Tukey HSD结果
if (length(all_tukey_results) > 0) {
  tukey_combined <- do.call(rbind, all_tukey_results)
  write.csv(tukey_combined, 
            file.path(stats_dir, "Tukey_HSD_results_combined.csv"), 
            row.names = FALSE)
  message("Tukey HSD results saved to: ", file.path(stats_dir, "Tukey_HSD_results_combined.csv"))
}

# 合并所有描述性统计结果
if (length(all_descriptive_stats) > 0) {
  descriptive_combined <- do.call(rbind, all_descriptive_stats)
  write.csv(descriptive_combined, 
            file.path(stats_dir, "Descriptive_statistics_combined.csv"), 
            row.names = FALSE)
  message("Descriptive statistics saved to: ", file.path(stats_dir, "Descriptive_statistics_combined.csv"))
}

# 9. 保存总体汇总统计
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


































rm(list=ls())

#########latitude#################
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

# 设置输入和输出目录
input_dir <- "F:\\model\\results\\clean"
output_dir <- "F:\\model\\results\\latitude"

# 创建输出目录
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 定义基于IQR去除异常值的函数
remove_outliers_iqr <- function(x, multiplier = 1.5) {
  # 只处理非NA值
  values <- x[!is.na(x)]
  
  if (length(values) == 0) {
    return(rep(NA, length(x)))
  }
  
  # 计算四分位数和IQR
  q1 <- quantile(values, 0.25, na.rm = TRUE)
  q3 <- quantile(values, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  # 确定异常值边界
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr
  
  # 创建结果向量，初始化为NA
  result <- x
  
  # 将异常值设为NA
  result[x < lower_bound | x > upper_bound] <- NA
  
  return(result)
}

# 获取所有tif文件
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# 循环处理每个tif文件
for (file_path in tif_files) {
  # 提取文件名（不含扩展名）
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # 读取栅格数据
  raster_data <- rast(file_path)
  # 检查数据有效性
  if (ncell(raster_data) == 0) {
    message(paste("Skipping", file_name, ": No data cells"))
    next
  }
  
  # === 应用转换公式：exp(r-1)*100 转换为百分数 ===
  message(paste("Applying transformation: (exp(r)-1)*100 to", file_name))
  
  
  raster_transformed <- (exp(raster_data) - 1) * 100
  
  # 检查转换后的数据范围
  trans_values <- values(raster_transformed)
  trans_min <- min(trans_values, na.rm = TRUE)
  trans_max <- max(trans_values, na.rm = TRUE)
  trans_mean <- mean(trans_values, na.rm = TRUE)
  
  message(paste("Transformed data range:", round(trans_min, 4), "to", round(trans_max, 4)))
  message(paste("Transformed mean:", round(trans_mean, 4)))
  
  # 转换为矩阵以便逐行处理（使用转换后的数据）
  raster_matrix <- as.matrix(raster_transformed, wide = TRUE)
  
  # 初始化存储处理后的行数据的矩阵
  cleaned_matrix <- matrix(NA, nrow = nrow(raster_matrix), ncol = ncol(raster_matrix))
  
  # 逐行应用IQR异常值去除
  for (i in 1:nrow(raster_matrix)) {
    row_data <- raster_matrix[i, ]
    # 应用IQR异常值去除
    cleaned_row <- remove_outliers_iqr(row_data, multiplier = 1.5)
    cleaned_matrix[i, ] <- cleaned_row
  }
  
  # 创建数据框
  dat <- data.frame(
    lat = yFromRow(raster_data),  # 纬度从90到-90（使用原始栅格的纬度信息）
    row_means = rowMeans(cleaned_matrix, na.rm = TRUE),  # 使用清洗后的转换数据计算行平均值
    row_sds = apply(cleaned_matrix, 1, sd, na.rm = TRUE)  # 使用清洗后的转换数据计算行标准差
  )
  
  # 记录原始转换数据的统计信息（用于调试）
  original_means <- rowMeans(raster_matrix, na.rm = TRUE)
  original_sds <- apply(raster_matrix, 1, sd, na.rm = TRUE)
  
  # 计算去除的异常值比例
  total_values <- sum(!is.na(raster_matrix))
  removed_values <- total_values - sum(!is.na(cleaned_matrix))
  removal_percentage <- (removed_values / total_values) * 100
  
  message(paste("File:", file_name))
  message(paste("Removed", removed_values, "outliers (", 
                round(removal_percentage, 2), "% of total values)"))
  
  # 移除NA值
  dat <- na.omit(dat)
  
  # 检查是否有有效数据
  if (nrow(dat) == 0) {
    message(paste("Skipping", file_name, ": No valid data after outlier removal and NA removal"))
    next
  }
  
  # 生成有意义的y轴标签
  y_label <- gsub("_", " ", file_name)
  y_label <- paste0(toupper(substr(y_label, 1, 1)), substring(y_label, 2))
  
  # 为y轴标签添加单位（因为是百分比）
  y_label <- paste0(y_label, " (%)")
  
  # 创建绘图 - 移除标题，取消加粗，字体再大一倍
  p <- ggplot(dat, aes(y = row_means, x = lat)) +     
    geom_line(size = 2, color = "orange") +    # 线条粗细
    geom_ribbon(aes(ymin = row_means - row_sds, 
                    ymax = row_means + row_sds), 
                fill = "lightgrey", alpha = 0.5) +  # 添加置信区间带
    labs(x = "Latitude (°)", y = y_label) +  # 坐标轴标签
    theme_classic() +               
    coord_flip() +  # 翻转x和y轴
    scale_y_continuous(limits = c(min(dat$row_means, na.rm = TRUE), 
                                  max(dat$row_means, na.rm = TRUE))) +  # 基于数据设置动态限制
    theme(
      axis.text = element_text(size = 85, color = "black"),   # 坐标轴文本大小（再大一倍），取消加粗
      axis.title = element_text(size = 85, color = "black"),  # 坐标轴标题大小（再大一倍），取消加粗
      plot.title = element_blank(),  # 完全移除标题
      panel.grid = element_blank(),  # 移除网格线
      axis.line = element_line(color = "black", size = 1.5),  # 坐标轴线
      panel.background = element_rect(fill = "white", colour = "black", size = 1.5),  # 面板边框
      plot.margin = margin(30, 30, 30, 30)  # 增加边距以适应更大字体
    )
  
  # 可选：添加原始转换数据的对比（用于比较）
  # 如果需要，可以取消注释下面的代码来同时显示原始数据
  # p <- p + 
  #   geom_line(aes(y = original_means[!is.na(original_means)], x = lat), 
  #             color = "blue", alpha = 0.3, size = 1)
  
  # 打印绘图
  print(p)
  
  # 保存图像 - 进一步增大保存尺寸以适应更大字体
  output_file <- file.path(output_dir, paste0(file_name, "_latitude_plot_transformed_iqr_cleaned.tiff"))
  ggsave(output_file, plot = p, width = 14, height = 16, device = "tiff", dpi = 300)
  
  message(paste("Saved:", output_file))
  
  # 添加统计信息输出（基于转换和清洗后的数据）
  message(paste("Transformed cleaned data range: Lat", round(min(dat$lat, na.rm = TRUE), 2), "to", 
                round(max(dat$lat, na.rm = TRUE), 2)))
  message(paste("Transformed cleaned value range:", round(min(dat$row_means, na.rm = TRUE), 4), "to", 
                round(max(dat$row_means, na.rm = TRUE), 4), "%"))
  message(paste("Transformed cleaned mean value:", round(mean(dat$row_means, na.rm = TRUE), 4), "%"))
  
  # 可选：保存转换和清洗后的数据为新的tif文件
  # 如果需要保存转换和清洗后的栅格数据，可以取消注释下面的代码
  # cleaned_raster <- rast(cleaned_matrix)
  # ext(cleaned_raster) <- ext(raster_data)
  # crs(cleaned_raster) <- crs(raster_data)
  # output_tif_file <- file.path(output_dir, paste0(file_name, "_transformed_cleaned.tif"))
  # writeRaster(cleaned_raster, output_tif_file, overwrite = TRUE)
  # message(paste("Saved transformed cleaned raster:", output_tif_file))
  
  message("----------------------------------------")
}

message("All plots have been processed with transformation and IQR outlier removal!")

