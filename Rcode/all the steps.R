####################projection################
# 批量设置 TIFF 文件投影的 R 代码 - 最终修正版
# 修复了字符串连接错误和计数逻辑

# 2. 加载包
library(terra)
library(sf)

# 3. 设置路径
#input_folder <- "F:/model/results/sixth/current_mapping/projected/clean/clean"
#output_folder <- "F:/model/results/sixth/current_mapping/projected"


input_folder <- "F:/model/results"
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




##clean raster###############

library(terra)

# 输入输出路径
in_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < -2] <- -2
  r[r > 2]  <- 2
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}












#############计算 LI ######################################
library(terra)

# 文件路径
active_path <- "F:\\model\\results\\sixth\\current_mapping\\current_projected\\active_sub.tif"
passive_path <- "F:\\model\\results\\sixth\\current_mapping\\current_projected\\passive_sub.tif"
output_path <- "F:\\model\\results\\sixth\\current_mapping\\current_projected\\LI_sub.tif"

# 文件路径
#active_path <- "F:/model/results/sixth/futuremapping/projected/active_sub_future.tif"
#passive_path <- "F:/model/results/sixth/futuremapping/projected/passive_sub_future.tif"
#output_path <- "F:/model/results/sixth/futuremapping/projected/LI_sub_future.tif"
# 读取栅格
active_rast <- rast(active_path)
passive_rast <- rast(passive_path)

# 检查栅格是否对齐
if (!compareGeom(active_rast, passive_rast, crs = TRUE, ext = TRUE, rowcol = TRUE)) {
  stop("栅格空间参考或尺寸不匹配")
}

# 执行减法
LI_rast <- active_rast - passive_rast

# 保存结果
writeRaster(LI_rast, output_path, overwrite = TRUE)

print(paste("计算完成，结果已保存至:", output_path))

rm(list = ls())




#############计算 LI ######################################
library(terra)

# 文件路径
#active_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\active_sub.tif"
#passive_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\passive_sub.tif""F:/model/results/sixth/futuremapping"


# 文件路径
active_path <- "F:/model/results/sixth/futuremapping/projected/active_top_future.tif"
passive_path <- "F:/model/results/sixth/futuremapping/projected/passive_top_future.tif"
output_path <- "F:/model/results/sixth/futuremapping/projected/LI_top_future.tif"
# 读取栅格
active_rast <- rast(active_path)
passive_rast <- rast(passive_path)

# 检查栅格是否对齐
if (!compareGeom(active_rast, passive_rast, crs = TRUE, ext = TRUE, rowcol = TRUE)) {
  stop("栅格空间参考或尺寸不匹配")
}

# 执行减法
LI_rast <- active_rast - passive_rast

# 保存结果
writeRaster(LI_rast, output_path, overwrite = TRUE)

print(paste("计算完成，结果已保存至:", output_path))

rm(list = ls())




#############计算 LI ######################################
library(terra)

# 文件路径
active_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\active_top.tif"
passive_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\passive_top.tif"
output_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\LI_top.tif"

# 读取栅格
active_rast <- rast(active_path)
passive_rast <- rast(passive_path)

# 检查栅格是否对齐
if (!compareGeom(active_rast, passive_rast, crs = TRUE, ext = TRUE, rowcol = TRUE)) {
  stop("栅格空间参考或尺寸不匹配")
}

# 执行减法
LI_rast <- active_rast - passive_rast

# 保存结果
writeRaster(LI_rast, output_path, overwrite = TRUE)

print(paste("计算完成，结果已保存至:", output_path))

rm(list = ls())



































#############计算 LI ######################################
library(terra)

# 文件路径
active_path <- "F:\\model\\results\\futuremapping\\projected\\active_top_future.tif"
passive_path <- "F:\\model\\results\\futuremapping\\projected\\passive_top_future.tif"
output_path <- "F:\\model\\results\\futuremapping\\projected\\LI_top_future.tif"

# 读取栅格
active_rast <- rast(active_path)
passive_rast <- rast(passive_path)

# 检查栅格是否对齐
if (!compareGeom(active_rast, passive_rast, crs = TRUE, ext = TRUE, rowcol = TRUE)) {
  stop("栅格空间参考或尺寸不匹配")
}

# 执行减法
LI_rast <- active_rast - passive_rast

# 保存结果
writeRaster(LI_rast, output_path, overwrite = TRUE)

print(paste("计算完成，结果已保存至:", output_path))

rm(list = ls())




#############计算 LI ######################################
library(terra)

# 文件路径
active_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean\\clipped_active_top.tif"
passive_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean\\clipped_passive_top.tif"
output_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean\\LI_top.tif"

# 读取栅格
active_rast <- rast(active_path)
passive_rast <- rast(passive_path)

# 检查栅格是否对齐
if (!compareGeom(active_rast, passive_rast, crs = TRUE, ext = TRUE, rowcol = TRUE)) {
  stop("栅格空间参考或尺寸不匹配")
}

# 执行减法
LI_rast <- active_rast - passive_rast

# 保存结果
writeRaster(LI_rast, output_path, overwrite = TRUE)

print(paste("计算完成，结果已保存至:", output_path))

rm(list = ls())














#############计算 LI ######################################
library(terra)

# 文件路径
active_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean\\clipped_active_sub.tif"
passive_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean\\clipped_passive_sub.tif"
output_path <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean\\LI_sub.tif"

# 读取栅格
active_rast <- rast(active_path)
passive_rast <- rast(passive_path)

# 检查栅格是否对齐
if (!compareGeom(active_rast, passive_rast, crs = TRUE, ext = TRUE, rowcol = TRUE)) {
  stop("栅格空间参考或尺寸不匹配")
}

# 执行减法
LI_rast <- active_rast - passive_rast

# 保存结果
writeRaster(LI_rast, output_path, overwrite = TRUE)

print(paste("计算完成，结果已保存至:", output_path))

rm(list = ls())























rm(list=ls())




#landuse############
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)

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

tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) {
  stop("FATAL ERROR: No TIF files found in the input directory.")
}

files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))
message(sprintf("Found %d TIF files to process.", length(files)))

out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\landuseplot"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

stats_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\\\statistics"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()


for (nm in names(files)) {
  message("\n--- Starting processing for file: ", nm, " ---")
  
  tryCatch({
    r <- rast(files[[nm]])
  }, error = function(e) {
    message("Error reading raster: ", files[[nm]])
    message("Skipping this file.")
    next
  })
  
  # coverted to percentage using formula：exp(tif-1)*100 
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  
  set.seed(42)
  n_cells <- ncell(r_transformed)
  cells <- sample(n_cells, min(100000, n_cells))
  
  vals_soil <- terra::values(r_transformed)[cells]
  vals_landuse <- terra::values(landuse)[cells]
  
  df <- data.frame(
    soil = vals_soil,
    landuse = vals_landuse
  )
  
  df <- na.omit(df)
  df <- df[is.finite(df$soil) & is.finite(df$landuse), ] 
  
  # remove land-use type (5, 8),as they are not our concern in the report
  df <- df[!(df$landuse %in% c(5, 8)), ]
  
  df$landuse <- factor(df$landuse)
  df$landuse <- droplevels(df$landuse) 
  
  if (any(is.na(levels(df$landuse)))) {
    message("Warning: Landuse factor levels contained NA. Re-cleaning.")
    df$landuse <- factor(df$landuse, exclude = NA)
    df <- df[!is.na(df$landuse), ]
    df$landuse <- droplevels(df$landuse)
  }
  
  # remove outlier using IQR
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
    
    message(sprintf("  移除了 %d 个异常值 (%.2f%% of initial valid points)", 
                    extreme_count, extreme_count / initial_rows * 100))
  }
  
  if (nrow(df) < 5 || length(unique(df$landuse)) < 2) {
    message("Warning: Insufficient valid data or landuse groups (<2) after cleaning for file: ", nm)
    next
  }
  
  # ANOVA test
  anova_result <- aov(soil ~ landuse, data = df)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 5) # 使用5位小数以提高精度
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
  
  # Tukey HSD test
  tukey_df <- NULL
  letter_df <- NULL
  
  if (p_value < 0.05) {
    if (all(table(df$landuse) > 1)) {
      tukey_result <- TukeyHSD(anova_result)
      tukey_letters <- multcompLetters4(anova_result, tukey_result)
      letter_df <- data.frame(landuse = names(tukey_letters$landuse$Letters),
                              Letters = tukey_letters$landuse$Letters)
      stat_vals <- df %>%
        group_by(landuse) %>%
        summarise(
          q3_val = quantile(soil, 0.95, na.rm = TRUE)
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
  
  # save the results
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
  
  ylab <- paste0(gsub("_", " ", nm), " (%)")
  
  # ploting
  p <- ggplot(df, aes(x = landuse, y = soil)) +
    geom_boxplot(
      aes(fill = after_stat(middle)), 
      outlier.size = 1.4,
      alpha = 0.8,
      size = 1.2,
      na.rm = TRUE
    ) +
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
    labs(
      x     = "",
      y     = ylab 
    ) +
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
      axis.ticks = element_line(size = 1.5, color = "black"), 
      axis.ticks.length = unit(0.3, "cm"), 
      axis.ticks.x = element_line(size = 1.5, color = "black"), 
      axis.ticks.y = element_line(size = 1.5, color = "black") 
    )
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
  
  message("ANOVA p-value: ", p_value)
  message("Transformed value range: ", round(min(df$soil, na.rm = TRUE), 4), 
          " to ", round(max(df$soil, na.rm = TRUE), 4), "%")
  message("Mean transformed value: ", round(mean(df$soil, na.rm = TRUE), 4), "%")
  message("Saved plot to: ", outfn)
  message("----------------------------------------")
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
  rm(r, r_transformed, df, df_clean, anova_result, anova_summary)
}


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







#biome 146################
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)
input_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\biomeplot_1_4_6"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
transformed_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\transformed"
if (!dir.exists(transformed_dir)) dir.create(transformed_dir, recursive = TRUE)
stats_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\statistics_1_4_6"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)
plot_data_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\plot_data_1_4_6"
if (!dir.exists(plot_data_dir)) dir.create(plot_data_dir, recursive = TRUE)
biome_rast <- rast("F:/BaiduNetdiskDownload/input/biome_projected.h5")
all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()
all_plot_data <- list()
BIOME_LEVELS <- c("Type_1", "Type_4", "Type_6")
BIOME_LABELS <- c("Trop/sub. for", "Tem. for", "Bor. for")
BIOME_COLORS <- c("Trop/sub. for" = "#017A79", 
                  "Tem. for" = "lightblue",   
                  "Bor. for" = "yellow")    
for (nm in names(files)) {
  
  r <- rast(files[[nm]])
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  transformed_filename <- file.path(transformed_dir, sprintf("%s_transformed.tif", nm))
  writeRaster(r_transformed, transformed_filename, overwrite = TRUE)
  message("Saved transformed raster to: ", transformed_filename)
  set.seed(42)
  cells <- sample(ncell(r_transformed), min(100000, ncell(r_transformed)))
  vals_soil  <- terra::values(r_transformed)[cells]
  vals_biome <- terra::values(biome_rast)[cells]
  df <- data.frame(
    soil  = vals_soil,
    biome = vals_biome
  )
  df <- df[complete.cases(df), ] 
  df <- df[is.finite(df$soil), ]
  df <- df[!is.na(df$biome) & is.finite(df$biome), ]
  df <- df[!(df$biome %in% c(5, 8, 7)), ]
  df <- df %>%
    mutate(
      biome_group = case_when(
        biome == 1 ~ "Type_1",
        biome == 4 ~ "Type_4", # 分开 4
        biome == 6 ~ "Type_6", # 分开 6
        TRUE ~ as.character(biome)
      )
    ) %>%
    filter(biome_group %in% BIOME_LEVELS) 
  df$biome_group <- factor(df$biome_group, 
                           levels = BIOME_LEVELS,
                           labels = BIOME_LABELS)
  if (nrow(df) > 0) {
    # 计算每个biome组的异常值边界
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
    
    message(sprintf("  removed %d outlier (%.2f%%)", 
                    extreme_count, extreme_count/nrow(df_clean)*100))
  }
  
  df <- df[!is.na(df$biome_group), ]
  df$biome_group <- droplevels(df$biome_group)
  valid_groups <- unique(df$biome_group)
  if (length(valid_groups) < 2) {
    message("Warning: Not enough valid biome groups (only ", length(valid_groups), ") after cleaning for file: ", nm)
    next
  }
  plot_data <- df
  plot_data$file <- nm
  all_plot_data[[nm]] <- plot_data
  message("biome: ", paste(sort(unique(df$biome_group)), collapse = ", "))
  message("Trop/sub. for: ", sum(df$biome_group == "Trop/sub. for"))
  message("Tem. for: ", sum(df$biome_group == "Tem. for"))
  message("Bor. for : ", sum(df$biome_group == "Bor. for"))
  df_aov <- df %>% filter(biome_group %in% valid_groups)
  df_aov$biome_group <- droplevels(df_aov$biome_group)
  
  anova_result <- aov(soil ~ biome_group, data = df_aov)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 2)  # 保留两位小数
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
  tukey_df <- NULL
  letter_df <- NULL
  if (p_value < 0.05) {
    tukey_result <- TukeyHSD(anova_result)
    tukey_letters <- multcompLetters4(anova_result, tukey_result)
    letter_df <- data.frame(biome_group = names(tukey_letters$biome_group$Letters),
                            Letters = tukey_letters$biome_group$Letters)
    stat_vals <- df %>%
      group_by(biome_group) %>%
      summarise(
        max_val = max(soil, na.rm = TRUE),
        q3_val = quantile(soil, 0.95, na.rm = TRUE)
      ) %>%
      mutate(biome_group = as.character(biome_group))
    letter_df <- merge(letter_df, stat_vals, by = "biome_group")
    tukey_df <- as.data.frame(tukey_result$biome_group)
    tukey_df$Comparison <- rownames(tukey_df)
    tukey_df$File <- nm
    tukey_df <- tukey_df[, c("File", "Comparison", "diff", "lwr", "upr", "p adj")]
    all_tukey_results[[nm]] <- tukey_df
  }
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
  # ploting
  p <- ggplot(df, aes(x = biome_group, y = soil)) +
    geom_boxplot(
      aes(fill = biome_group),    
      color = "black",             
      outlier.shape = NA,       
      size = 1.2,                  
      na.rm = TRUE,        
      alpha = 0.8    
    ) +
    # 添加均值点和中位数点
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 15,           
      color = "red",             
      na.rm = TRUE    
    ) +
    stat_summary(
      fun = median, 
      geom = "point", 
      shape = 15, 
      size = 15,             
      color = "blue",        
      na.rm = TRUE               
    ) +
    scale_fill_manual(values = BIOME_COLORS) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0)), 
                     labels = BIOME_LABELS) + 
    theme_classic() +
    labs(
      x = "",                 
      y = ""                    
    ) +
    annotate("text", 
             x = Inf, y = Inf, 
             label = paste("p =", sprintf("%.2f", p_value)),
             hjust = 1.1, vjust = 1.5, 
             size = 25,            
             fontface = "bold",    
             color = "black") +
    theme(
      axis.line       = element_line(size = 1.6, color = "grey40"), 
      axis.text       = element_text(size = 80),                   
      axis.text.x     = element_text(hjust = 0.5, size = 80, angle = 0), 
      axis.text.y     = element_text(size = 80, margin = margin(r = 10)), 
      plot.title      = element_blank(),                      
      legend.position = "none",                               
      axis.ticks = element_line(size = 1.5, color = "black"),     
      axis.ticks.length = unit(0.3, "cm"),                       
      axis.ticks.x = element_line(size = 1.5, color = "black"), 
      axis.ticks.y = element_line(size = 1.5, color = "black")  
    )
  
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
  
  # save the files
  outfn <- file.path(out_dir, sprintf("%s_by_Biome_Type1_4_6_colored.tif", nm))
  ggsave(
    filename    = outfn,
    plot        = p,
    device      = "tiff",
    dpi         = 300,
    width       = 18,
    height      = 14, 
    units       = "in",
    compression = "lzw"
  )
  
  #print output
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
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
}

if (length(all_anova_results) > 0) {
  anova_combined <- do.call(rbind, all_anova_results)
  write.csv(anova_combined, 
            file.path(stats_dir, "ANOVA_results_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("ANOVA results saved to: ", file.path(stats_dir, "ANOVA_results_biome_Type1_4_6_colored.csv"))
}

if (length(all_tukey_results) > 0) {
  tukey_combined <- do.call(rbind, all_tukey_results)
  write.csv(tukey_combined, 
            file.path(stats_dir, "Tukey_HSD_results_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Tukey HSD results saved to: ", file.path(stats_dir, "Tukey_HSD_results_biome_Type1_4_6_colored.csv"))
}

if (length(all_descriptive_stats) > 0) {
  descriptive_combined <- do.call(rbind, all_descriptive_stats)
  write.csv(descriptive_combined, 
            file.path(stats_dir, "Descriptive_statistics_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Descriptive statistics saved to: ", file.path(stats_dir, "Descriptive_statistics_biome_Type1_4_6_colored.csv"))
}

if (length(all_plot_data) > 0) {
  plot_data_combined <- do.call(rbind, all_plot_data)
  write.csv(plot_data_combined, 
            file.path(plot_data_dir, "Plot_data_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Plot data saved to: ", file.path(plot_data_dir, "Plot_data_biome_Type1_4_6_colored.csv"))
}

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


input_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
output_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\latitude"


if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


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


tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)


for (file_path in tif_files) {

  file_name <- tools::file_path_sans_ext(basename(file_path))
  

  raster_data <- rast(file_path)

  if (ncell(raster_data) == 0) {
    message(paste("Skipping", file_name, ": No data cells"))
    next
  }
  

  message(paste("Applying transformation: (exp(r)-1)*100 to", file_name))
  
  
  raster_transformed <- (exp(raster_data) - 1) * 100
  

  trans_values <- values(raster_transformed)
  trans_min <- min(trans_values, na.rm = TRUE)
  trans_max <- max(trans_values, na.rm = TRUE)
  trans_mean <- mean(trans_values, na.rm = TRUE)
  
  message(paste("Transformed data range:", round(trans_min, 4), "to", round(trans_max, 4)))
  message(paste("Transformed mean:", round(trans_mean, 4)))
 
  raster_matrix <- as.matrix(raster_transformed, wide = TRUE)
  

  cleaned_matrix <- matrix(NA, nrow = nrow(raster_matrix), ncol = ncol(raster_matrix))
  

  for (i in 1:nrow(raster_matrix)) {
    row_data <- raster_matrix[i, ]
    # 应用IQR异常值去除
    cleaned_row <- remove_outliers_iqr(row_data, multiplier = 1.5)
    cleaned_matrix[i, ] <- cleaned_row
  }
  

  dat <- data.frame(
    lat = yFromRow(raster_data),
    row_means = rowMeans(cleaned_matrix, na.rm = TRUE),  
    row_sds = apply(cleaned_matrix, 1, sd, na.rm = TRUE)  
  )
  

  original_means <- rowMeans(raster_matrix, na.rm = TRUE)
  original_sds <- apply(raster_matrix, 1, sd, na.rm = TRUE)
  total_values <- sum(!is.na(raster_matrix))
  removed_values <- total_values - sum(!is.na(cleaned_matrix))
  removal_percentage <- (removed_values / total_values) * 100
  message(paste("File:", file_name))
  message(paste("Removed", removed_values, "outliers (", 
                round(removal_percentage, 2), "% of total values)"))
  dat <- na.omit(dat)
  if (nrow(dat) == 0) {
    message(paste("Skipping", file_name, ": No valid data after outlier removal and NA removal"))
    next
  }
  y_label <- gsub("_", " ", file_name)
  y_label <- paste0(toupper(substr(y_label, 1, 1)), substring(y_label, 2))
  y_label <- paste0(y_label, " (%)")
  p <- ggplot(dat, aes(y = row_means, x = lat)) +     
    geom_ribbon(aes(ymin = row_means - row_sds, 
                    ymax = row_means + row_sds), 
                fill = "lightgrey", alpha = 0.5) +
    geom_line(size = 2, color = "orange") + 
    labs(x = "Latitude (°)", y = y_label) +
    theme_classic() +               
    coord_flip() +
    scale_y_continuous(limits = c(min(dat$row_means, na.rm = TRUE), 
                                  max(dat$row_means, na.rm = TRUE))) + 
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
  output_file <- file.path(output_dir, paste0(file_name, "_latitude_plot_transformed_iqr_cleaned.tiff"))
  ggsave(output_file, plot = p, width = 14, height = 14, device = "tiff", dpi = 300)
  
  message(paste("Saved:", output_file))
  message(paste("Transformed cleaned data range: Lat", round(min(dat$lat, na.rm = TRUE), 2), "to", 
                round(max(dat$lat, na.rm = TRUE), 2)))
  message(paste("Transformed cleaned value range:", round(min(dat$row_means, na.rm = TRUE), 4), "to", 
                round(max(dat$row_means, na.rm = TRUE), 4), "%"))
  message(paste("Transformed cleaned mean value:", round(mean(dat$row_means, na.rm = TRUE), 4), "%"))

  message("----------------------------------------")
}

message("All plots have been processed with transformation and IQR outlier removal!")






























































































































#future##############
#landuse############
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)

landuse_path <- "F:\\BaiduNetdiskDownload\\input\\Landuse_type_projected.h5"
if (!file.exists(landuse_path)) {
  stop("FATAL ERROR: Landuse file not found at: ", landuse_path)
}

message("Reading Landuse raster...")
landuse <- rast(landuse_path)
input_dir <- "F:/model/results/sixth/futuremapping/projected"
if (!dir.exists(input_dir)) {
  stop("FATAL ERROR: Input TIF directory does not exist: ", input_dir)
}

tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) {
  stop("FATAL ERROR: No TIF files found in the input directory.")
}

files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))
message(sprintf("Found %d TIF files to process.", length(files)))

out_dir <- "F:/model/results/sixth/futuremapping/projected/landuseplot"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

stats_dir <- "F:/model/results/sixth/futuremapping/projected/statistics"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()


for (nm in names(files)) {
  message("\n--- Starting processing for file: ", nm, " ---")
  
  tryCatch({
    r <- rast(files[[nm]])
  }, error = function(e) {
    message("Error reading raster: ", files[[nm]])
    message("Skipping this file.")
    next
  })
  
  # coverted to percentage using formula：exp(tif-1)*100 
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  
  set.seed(42)
  n_cells <- ncell(r_transformed)
  cells <- sample(n_cells, min(100000, n_cells))
  
  vals_soil <- terra::values(r_transformed)[cells]
  vals_landuse <- terra::values(landuse)[cells]
  
  df <- data.frame(
    soil = vals_soil,
    landuse = vals_landuse
  )
  
  df <- na.omit(df)
  df <- df[is.finite(df$soil) & is.finite(df$landuse), ] 
  
  # remove land-use type (5, 8),as they are not our concern in the report
  df <- df[!(df$landuse %in% c(5, 8)), ]
  
  df$landuse <- factor(df$landuse)
  df$landuse <- droplevels(df$landuse) 
  
  if (any(is.na(levels(df$landuse)))) {
    message("Warning: Landuse factor levels contained NA. Re-cleaning.")
    df$landuse <- factor(df$landuse, exclude = NA)
    df <- df[!is.na(df$landuse), ]
    df$landuse <- droplevels(df$landuse)
  }
  
  # remove outlier using IQR
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
    
    message(sprintf("  移除了 %d 个异常值 (%.2f%% of initial valid points)", 
                    extreme_count, extreme_count / initial_rows * 100))
  }
  
  if (nrow(df) < 5 || length(unique(df$landuse)) < 2) {
    message("Warning: Insufficient valid data or landuse groups (<2) after cleaning for file: ", nm)
    next
  }
  
  # ANOVA test
  anova_result <- aov(soil ~ landuse, data = df)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 5) # 使用5位小数以提高精度
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
  
  # Tukey HSD test
  tukey_df <- NULL
  letter_df <- NULL
  
  if (p_value < 0.05) {
    if (all(table(df$landuse) > 1)) {
      tukey_result <- TukeyHSD(anova_result)
      tukey_letters <- multcompLetters4(anova_result, tukey_result)
      letter_df <- data.frame(landuse = names(tukey_letters$landuse$Letters),
                              Letters = tukey_letters$landuse$Letters)
      stat_vals <- df %>%
        group_by(landuse) %>%
        summarise(
          q3_val = quantile(soil, 0.95, na.rm = TRUE)
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
  
  # save the results
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
  
  ylab <- paste0(gsub("_", " ", nm), " (%)")
  
  # ploting
  p <- ggplot(df, aes(x = landuse, y = soil)) +
    geom_boxplot(
      aes(fill = after_stat(middle)), 
      outlier.size = 1.4,
      alpha = 0.8,
      size = 1.2,
      na.rm = TRUE
    ) +
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
    labs(
      x     = "",
      y     = ylab 
    ) +
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
      axis.ticks = element_line(size = 1.5, color = "black"), 
      axis.ticks.length = unit(0.3, "cm"), 
      axis.ticks.x = element_line(size = 1.5, color = "black"), 
      axis.ticks.y = element_line(size = 1.5, color = "black") 
    )
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
  
  message("ANOVA p-value: ", p_value)
  message("Transformed value range: ", round(min(df$soil, na.rm = TRUE), 4), 
          " to ", round(max(df$soil, na.rm = TRUE), 4), "%")
  message("Mean transformed value: ", round(mean(df$soil, na.rm = TRUE), 4), "%")
  message("Saved plot to: ", outfn)
  message("----------------------------------------")
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
  rm(r, r_transformed, df, df_clean, anova_result, anova_summary)
}


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







#biome 146################
# install.packages(c("terra", "ggplot2", "dplyr", "rstatix", "multcompView"))
library(terra)
library(ggplot2)
library(dplyr)
library(rstatix)
library(multcompView)
input_dir <- "F:/model/results/sixth/futuremapping/projected"
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))
out_dir <- "F:/model/results/sixth/futuremapping/projected/biomeplot_1_4_6"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
transformed_dir <- "F:/model/results/sixth/futuremapping/projected/transformed"
if (!dir.exists(transformed_dir)) dir.create(transformed_dir, recursive = TRUE)
stats_dir <- "F:/model/results/sixth/futuremapping/projected/statistics_1_4_6"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)
plot_data_dir <- "F:/model/results/sixth/futuremapping/projected/plot_data_1_4_6"
if (!dir.exists(plot_data_dir)) dir.create(plot_data_dir, recursive = TRUE)
biome_rast <- rast("F:/BaiduNetdiskDownload/input/biome_projected.h5")
all_anova_results <- list()
all_tukey_results <- list()
all_descriptive_stats <- list()
all_plot_data <- list()
BIOME_LEVELS <- c("Type_1", "Type_4", "Type_6")
BIOME_LABELS <- c("Trop/sub. for", "Tem. for", "Bor. for")
BIOME_COLORS <- c("Trop/sub. for" = "#017A79", 
                  "Tem. for" = "lightblue",   
                  "Bor. for" = "yellow")    
for (nm in names(files)) {
  
  r <- rast(files[[nm]])
  r_transformed <- (exp(r) - 1) * 100
  r_transformed[is.na(r)] <- NA
  transformed_filename <- file.path(transformed_dir, sprintf("%s_transformed.tif", nm))
  writeRaster(r_transformed, transformed_filename, overwrite = TRUE)
  message("Saved transformed raster to: ", transformed_filename)
  set.seed(42)
  cells <- sample(ncell(r_transformed), min(100000, ncell(r_transformed)))
  vals_soil  <- terra::values(r_transformed)[cells]
  vals_biome <- terra::values(biome_rast)[cells]
  df <- data.frame(
    soil  = vals_soil,
    biome = vals_biome
  )
  df <- df[complete.cases(df), ] 
  df <- df[is.finite(df$soil), ]
  df <- df[!is.na(df$biome) & is.finite(df$biome), ]
  df <- df[!(df$biome %in% c(5, 8, 7)), ]
  df <- df %>%
    mutate(
      biome_group = case_when(
        biome == 1 ~ "Type_1",
        biome == 4 ~ "Type_4", # 分开 4
        biome == 6 ~ "Type_6", # 分开 6
        TRUE ~ as.character(biome)
      )
    ) %>%
    filter(biome_group %in% BIOME_LEVELS) 
  df$biome_group <- factor(df$biome_group, 
                           levels = BIOME_LEVELS,
                           labels = BIOME_LABELS)
  if (nrow(df) > 0) {
    # 计算每个biome组的异常值边界
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
    
    message(sprintf("  removed %d outlier (%.2f%%)", 
                    extreme_count, extreme_count/nrow(df_clean)*100))
  }
  
  df <- df[!is.na(df$biome_group), ]
  df$biome_group <- droplevels(df$biome_group)
  valid_groups <- unique(df$biome_group)
  if (length(valid_groups) < 2) {
    message("Warning: Not enough valid biome groups (only ", length(valid_groups), ") after cleaning for file: ", nm)
    next
  }
  plot_data <- df
  plot_data$file <- nm
  all_plot_data[[nm]] <- plot_data
  message("biome: ", paste(sort(unique(df$biome_group)), collapse = ", "))
  message("Trop/sub. for: ", sum(df$biome_group == "Trop/sub. for"))
  message("Tem. for: ", sum(df$biome_group == "Tem. for"))
  message("Bor. for : ", sum(df$biome_group == "Bor. for"))
  df_aov <- df %>% filter(biome_group %in% valid_groups)
  df_aov$biome_group <- droplevels(df_aov$biome_group)
  
  anova_result <- aov(soil ~ biome_group, data = df_aov)
  anova_summary <- summary(anova_result)
  p_value <- round(anova_summary[[1]]$'Pr(>F)'[1], 2)  # 保留两位小数
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
  tukey_df <- NULL
  letter_df <- NULL
  if (p_value < 0.05) {
    tukey_result <- TukeyHSD(anova_result)
    tukey_letters <- multcompLetters4(anova_result, tukey_result)
    letter_df <- data.frame(biome_group = names(tukey_letters$biome_group$Letters),
                            Letters = tukey_letters$biome_group$Letters)
    stat_vals <- df %>%
      group_by(biome_group) %>%
      summarise(
        max_val = max(soil, na.rm = TRUE),
        q3_val = quantile(soil, 0.95, na.rm = TRUE)
      ) %>%
      mutate(biome_group = as.character(biome_group))
    letter_df <- merge(letter_df, stat_vals, by = "biome_group")
    tukey_df <- as.data.frame(tukey_result$biome_group)
    tukey_df$Comparison <- rownames(tukey_df)
    tukey_df$File <- nm
    tukey_df <- tukey_df[, c("File", "Comparison", "diff", "lwr", "upr", "p adj")]
    all_tukey_results[[nm]] <- tukey_df
  }
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
  # ploting
  p <- ggplot(df, aes(x = biome_group, y = soil)) +
    geom_boxplot(
      aes(fill = biome_group),    
      color = "black",             
      outlier.shape = NA,       
      size = 1.2,                  
      na.rm = TRUE,        
      alpha = 0.8    
    ) +
    # 添加均值点和中位数点
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 15,           
      color = "red",             
      na.rm = TRUE    
    ) +
    stat_summary(
      fun = median, 
      geom = "point", 
      shape = 15, 
      size = 15,             
      color = "blue",        
      na.rm = TRUE               
    ) +
    scale_fill_manual(values = BIOME_COLORS) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0)), 
                     labels = BIOME_LABELS) + 
    theme_classic() +
    labs(
      x = "",                 
      y = ""                    
    ) +
    annotate("text", 
             x = Inf, y = Inf, 
             label = paste("p =", sprintf("%.2f", p_value)),
             hjust = 1.1, vjust = 1.5, 
             size = 25,            
             fontface = "bold",    
             color = "black") +
    theme(
      axis.line       = element_line(size = 1.6, color = "grey40"), 
      axis.text       = element_text(size = 80),                   
      axis.text.x     = element_text(hjust = 0.5, size = 80, angle = 0), 
      axis.text.y     = element_text(size = 80, margin = margin(r = 10)), 
      plot.title      = element_blank(),                      
      legend.position = "none",                               
      axis.ticks = element_line(size = 1.5, color = "black"),     
      axis.ticks.length = unit(0.3, "cm"),                       
      axis.ticks.x = element_line(size = 1.5, color = "black"), 
      axis.ticks.y = element_line(size = 1.5, color = "black")  
    )
  
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
  
  # save the files
  outfn <- file.path(out_dir, sprintf("%s_by_Biome_Type1_4_6_colored.tif", nm))
  ggsave(
    filename    = outfn,
    plot        = p,
    device      = "tiff",
    dpi         = 300,
    width       = 18,
    height      = 14, 
    units       = "in",
    compression = "lzw"
  )
  
  #print output
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
  if (exists("tukey_result")) rm(tukey_result)
  if (exists("letter_df")) rm(letter_df)
}

if (length(all_anova_results) > 0) {
  anova_combined <- do.call(rbind, all_anova_results)
  write.csv(anova_combined, 
            file.path(stats_dir, "ANOVA_results_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("ANOVA results saved to: ", file.path(stats_dir, "ANOVA_results_biome_Type1_4_6_colored.csv"))
}

if (length(all_tukey_results) > 0) {
  tukey_combined <- do.call(rbind, all_tukey_results)
  write.csv(tukey_combined, 
            file.path(stats_dir, "Tukey_HSD_results_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Tukey HSD results saved to: ", file.path(stats_dir, "Tukey_HSD_results_biome_Type1_4_6_colored.csv"))
}

if (length(all_descriptive_stats) > 0) {
  descriptive_combined <- do.call(rbind, all_descriptive_stats)
  write.csv(descriptive_combined, 
            file.path(stats_dir, "Descriptive_statistics_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Descriptive statistics saved to: ", file.path(stats_dir, "Descriptive_statistics_biome_Type1_4_6_colored.csv"))
}

if (length(all_plot_data) > 0) {
  plot_data_combined <- do.call(rbind, all_plot_data)
  write.csv(plot_data_combined, 
            file.path(plot_data_dir, "Plot_data_biome_Type1_4_6_colored.csv"), 
            row.names = FALSE)
  message("Plot data saved to: ", file.path(plot_data_dir, "Plot_data_biome_Type1_4_6_colored.csv"))
}

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


input_dir <- "F:/model/results/sixth/futuremapping/projected"
output_dir <- "F:/model/results/sixth/futuremapping/projected/latitude"


if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


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


tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)


for (file_path in tif_files) {
  
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  
  raster_data <- rast(file_path)
  
  if (ncell(raster_data) == 0) {
    message(paste("Skipping", file_name, ": No data cells"))
    next
  }
  
  
  message(paste("Applying transformation: (exp(r)-1)*100 to", file_name))
  
  
  raster_transformed <- (exp(raster_data) - 1) * 100
  
  
  trans_values <- values(raster_transformed)
  trans_min <- min(trans_values, na.rm = TRUE)
  trans_max <- max(trans_values, na.rm = TRUE)
  trans_mean <- mean(trans_values, na.rm = TRUE)
  
  message(paste("Transformed data range:", round(trans_min, 4), "to", round(trans_max, 4)))
  message(paste("Transformed mean:", round(trans_mean, 4)))
  
  raster_matrix <- as.matrix(raster_transformed, wide = TRUE)
  
  
  cleaned_matrix <- matrix(NA, nrow = nrow(raster_matrix), ncol = ncol(raster_matrix))
  
  
  for (i in 1:nrow(raster_matrix)) {
    row_data <- raster_matrix[i, ]
    # 应用IQR异常值去除
    cleaned_row <- remove_outliers_iqr(row_data, multiplier = 1.5)
    cleaned_matrix[i, ] <- cleaned_row
  }
  
  
  dat <- data.frame(
    lat = yFromRow(raster_data),
    row_means = rowMeans(cleaned_matrix, na.rm = TRUE),  
    row_sds = apply(cleaned_matrix, 1, sd, na.rm = TRUE)  
  )
  
  
  original_means <- rowMeans(raster_matrix, na.rm = TRUE)
  original_sds <- apply(raster_matrix, 1, sd, na.rm = TRUE)
  total_values <- sum(!is.na(raster_matrix))
  removed_values <- total_values - sum(!is.na(cleaned_matrix))
  removal_percentage <- (removed_values / total_values) * 100
  message(paste("File:", file_name))
  message(paste("Removed", removed_values, "outliers (", 
                round(removal_percentage, 2), "% of total values)"))
  dat <- na.omit(dat)
  if (nrow(dat) == 0) {
    message(paste("Skipping", file_name, ": No valid data after outlier removal and NA removal"))
    next
  }
  y_label <- gsub("_", " ", file_name)
  y_label <- paste0(toupper(substr(y_label, 1, 1)), substring(y_label, 2))
  y_label <- paste0(y_label, " (%)")
  p <- ggplot(dat, aes(y = row_means, x = lat)) +     
    geom_ribbon(aes(ymin = row_means - row_sds, 
                    ymax = row_means + row_sds), 
                fill = "lightgrey", alpha = 0.5) +
    geom_line(size = 2, color = "orange") + 
    labs(x = "Latitude (°)", y = y_label) +
    theme_classic() +               
    coord_flip() +
    scale_y_continuous(limits = c(min(dat$row_means, na.rm = TRUE), 
                                  max(dat$row_means, na.rm = TRUE))) + 
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
  output_file <- file.path(output_dir, paste0(file_name, "_latitude_plot_transformed_iqr_cleaned.tiff"))
  ggsave(output_file, plot = p, width = 14, height = 14, device = "tiff", dpi = 300)
  
  message(paste("Saved:", output_file))
  message(paste("Transformed cleaned data range: Lat", round(min(dat$lat, na.rm = TRUE), 2), "to", 
                round(max(dat$lat, na.rm = TRUE), 2)))
  message(paste("Transformed cleaned value range:", round(min(dat$row_means, na.rm = TRUE), 4), "to", 
                round(max(dat$row_means, na.rm = TRUE), 4), "%"))
  message(paste("Transformed cleaned mean value:", round(mean(dat$row_means, na.rm = TRUE), 4), "%"))
  
  message("----------------------------------------")
}

message("All plots have been processed with transformation and IQR outlier removal!")


















































































































########LICV#######

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

# 查看当前R的工作目录
getwd()

# 查看完整的输出文件路径
output_path <- file.path(getwd(), "CVLI_sub.tif")
print(output_path)




########LICV#######

# Load necessary libraries
library(raster)
library(terra) # Alternative if using newer terra package

# Option 1: Using raster package
# Read the SDLI raster file
sdl_raster <- raster("F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\current\\current\\spreading\\LI_top_CV.tif")



# Calculate CVLI using your formula: CV_LI = sqrt(exp(SD_LI^2) - 1) * 100
cvli_raster <- sqrt(exp(sdl_raster^2) - 1) * 100

# Write the result to a new file
writeRaster(cvli_raster, "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\current\\current\\spreading\\CVLI_top.tif", format = "GTiff", overwrite = TRUE)

# 查看当前R的工作目录
getwd()

# 查看完整的输出文件路径
output_path <- file.path(getwd(), "CVLI_sub.tif")
print(output_path)





#################LI CV clipping###############



library(terra)

# 输入输出路径
in_dir <- "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\current\\current\\spreading"
out_dir <- "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\current\\current\\spreading\\clean"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < 0] <- -0
  r[r > 200]  <- 200
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}



rm(list = ls())


########LICV#######

# Load necessary libraries
library(raster)
library(terra) # Alternative if using newer terra package

# Option 1: Using raster package
# Read the SDLI raster file
sdl_raster <- raster("F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading\\LI_sub_CV.tif")



# Calculate CVLI using your formula: CV_LI = sqrt(exp(SD_LI^2) - 1) * 100
cvli_raster <- sqrt(exp(sdl_raster^2) - 1) * 100

# Write the result to a new file
writeRaster(cvli_raster, "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading\\CVLI_sub.tif", format = "GTiff", overwrite = TRUE)

# 查看当前R的工作目录
getwd()

# 查看完整的输出文件路径
output_path <- file.path(getwd(), "CVLI_sub.tif")
print(output_path)


rm(list = ls())


########LICV#######

# Load necessary libraries
library(raster)
library(terra) # Alternative if using newer terra package

# Option 1: Using raster package
# Read the SDLI raster file
sdl_raster <- raster("F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading\\LI_top_CV.tif")



# Calculate CVLI using your formula: CV_LI = sqrt(exp(SD_LI^2) - 1) * 100
cvli_raster <- sqrt(exp(sdl_raster^2) - 1) * 100

# Write the result to a new file
writeRaster(cvli_raster, "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading\\CVLI_top.tif", format = "GTiff", overwrite = TRUE)

# 查看当前R的工作目录
getwd()

# 查看完整的输出文件路径
output_path <- file.path(getwd(), "CVLI_sub.tif")
print(output_path)




rm(list = ls())

#################LI CV clipping###############



library(terra)

# 输入输出路径
in_dir <- "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading"
out_dir <- "F:\\model\\spreaDING\\current\\spreadingmapping\\spreadingmapping\\future\\future\\spreading\\clean"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < 0] <- -0
  r[r > 200]  <- 200
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}



