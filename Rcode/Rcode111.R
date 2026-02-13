


library(terra)

# 输入输出路径
in_dir <- "F:\\model\\results\\futuremapping\\projected"
out_dir <- "F:\\model\\results\\futuremapping\\projected\\clean"
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







library(terra)

# 输入输出路径
in_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected"
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean"
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
































library(terra)

# 输入输出路径
#in_dir <- "F:\\model\\results\\third\\clean"
in_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\transformed"
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\transformed\\clean100"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < -100] <- -100
  r[r > 100]  <- 100
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}




library(terra)

# 输入输出路径
#in_dir <- "F:\\model\\results\\third\\clean"
in_dir <- "F:\\model\\results\\sixth\\futuremapping\\projected\\transformed"
out_dir <- "F:\\model\\results\\sixth\\futuremapping\\projected\\transformed\\clean100"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < -100] <- -100
  r[r > 100]  <- 100
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}









library(terra)

# 输入输出路径
in_dir <- "F:\\model\\results\\clean\\clean-3400880167e7\\clean\\transformed"
out_dir <- "F:\\model\\results\\clean\\clean-3400880167e7\\clean\\transformed\\secondfinalclippted100"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 获取所有tif
tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)

for (f in tif_files) {
  cat("处理:", basename(f), "\n")
  
  r <- rast(f)
  
  # 裁剪值到 -1.5 ~ 1.5
  r[r < -100] <- -100
  r[r > 100]  <- 100
  # 保存裁剪后的TIF
  out_tif <- file.path(out_dir, paste0("clipped_", basename(f)))
  writeRaster(r, out_tif, overwrite = TRUE)
  
  cat("已保存TIF:", out_tif, "\n\n")
}







rm(list=ls())
######去除异常值########################
library(raster)


batch_process_tifs_serial <- function(input_dir, output_dir,
                                      method = "percentile",
                                      lower_pct = 0.01,
                                      upper_pct = 0.99) {
  
  # 1. 检查并创建输出目录
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(paste("创建输出目录:", output_dir))
  }
  
  tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    stop("在输入目录中未找到任何 .tif 文件。")
  }
  
  # 使用 for 循环进行串行处理，避免并行冲突
  for (i in 1:length(tif_files)) {
    input_file <- tif_files[i]
    output_file <- file.path(output_dir, basename(input_file))
    
    message(sprintf("\n--- 正在处理文件 %d/%d: %s ---", i, length(tif_files), basename(input_file)))
    
    # 尝试读取栅格
    r <- tryCatch(
      raster(input_file),
      error = function(e) {
        warning(sprintf("读取文件失败 (%s): %s", basename(input_file), e$message))
        return(NULL)
      }
    )
    
    if (is.null(r)) next # 跳过失败的文件
    
    # 2. 获取所有像元值以计算异常值边界 (内存瓶颈点)
    vals <- getValues(r)
    
    # 3. 计算边界值
    if (method == "percentile") {
      # 使用 type=7 确保结果稳定
      lower_bound <- quantile(vals, probs = lower_pct, na.rm = TRUE, type = 7)
      upper_bound <- quantile(vals, probs = upper_pct, na.rm = TRUE, type = 7)
      message(sprintf("边界 (百分位法): %.4f 到 %.4f", lower_bound, upper_bound))
      
    } else if (method == "iqr") {
      q <- quantile(vals, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)
      iqr_val <- q[2] - q[1]
      lower_bound <- q[1] - 1.5 * iqr_val
      upper_bound <- q[2] + 1.5 * iqr_val
      message(sprintf("边界 (IQR法): %.4f 到 %.4f", lower_bound, upper_bound))
      
    } else {
      stop("无效的 method 参数。请使用 'percentile' 或 'iqr'。")
    }
    
    # 4. 定义用于 calc() 的处理函数
    # calc() 会自动分块处理数据，减少内存占用
    fun_outlier_removal <- function(x) {
      # x 是 calc 每次读取的一个数据块
      x[x < lower_bound] <- lower_bound
      x[x > upper_bound] <- upper_bound
      return(x)
    }
    
    # 5. 应用处理并创建新的栅格对象
    r_new <- calc(r, fun = fun_outlier_removal)
    
    # 6. 写入结果
    writeRaster(r_new, output_file, format = "GTiff", overwrite = TRUE)
    message("文件处理并保存成功。")
    
    # 7. 清理内存，帮助 R 释放资源
    rm(r, vals, r_new)
    gc(verbose = FALSE)
  }
  message("\n--- 所有文件处理完成 ---")
}


# ⬇️ 运行代码块 (请修改您的实际路径) ⬇️


# 设置输入和输出目录
input_directory <- "F:\\model\\results\\transformed\\LI" # 示例路径 A
output_directory <- "F:\\model\\results\\transformed\\LI\\remove_outlier" # 示例路径 B

# 或者使用您最新的路径 (如果文件路径中包含中文，建议使用 / 代替 \\)
# input_directory <- "F:/model/results/transformed/LI"
# output_directory <- "F:/model/results/transformed/LI/remove_outlier"

# 调用优化后的串行处理函数
batch_process_tifs_serial(
  input_dir = input_directory,
  output_dir = output_directory,
  method = "percentile",
  lower_pct = 0.01, # 保留最低 1% 到最高 99% 的数据
  upper_pct = 0.99
)

















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
input_dir <- "F:\\model\\results\\futuremapping\\projected"
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# 创建文件列表，使用文件名作为键
files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))

# 1. 输出目录 (更新文件名以反映新的分组)
out_dir <- "F:/model/results/biomeplot_1_4_6"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1b. 创建转换后tif文件的输出目录
transformed_dir <- "F:\\model\\results\\futuremapping\\projected\\transformed"
if (!dir.exists(transformed_dir)) dir.create(transformed_dir, recursive = TRUE)

# 1c. 创建统计结果CSV文件输出目录 (更新文件名以反映新的分组)
stats_dir <- "F:\\model\\results\\futuremapping\\projected\\statistics_1_4_6"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

# 1d. 创建绘图数据保存目录 (更新文件名以反映新的分组)
plot_data_dir <- "F:\\model\\results\\futuremapping\\projected\\plot_data_1_4_6"
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
        Q1 = quantile(soil, 0.05, na.rm = TRUE),
        Q3 = quantile(soil, 0.95, na.rm = TRUE),
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
        q3_val = quantile(soil, 0.95, na.rm = TRUE)
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

# 定义输入目录和 TIF 文\件列表
input_dir <- "F:/model/results/sixth/current_mapping/projected"
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
out_dir <- "F:/model/results/sixth/current_mapping/projected/landuseplot"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1c. 创建统计结果CSV文件输出目录
stats_dir <- "F:/model/results/sixth/current_mapping/projected/statistics"
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
        Q1 = quantile(soil, 0.05, na.rm = TRUE),
        Q3 = quantile(soil, 0.95, na.rm = TRUE),
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
      q25 = round(quantile(soil, 0.05, na.rm = TRUE), 4),
      q75 = round(quantile(soil, 0.95, na.rm = TRUE), 4)
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
input_dir <- "F:/model/results/sixth/current_mapping/projected/clean/clean"
output_dir <- "F:/model/results/sixth/current_mapping/projected/"

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
  q1 <- quantile(values, 0.05, na.rm = TRUE)
  q3 <- quantile(values, 0.95, na.rm = TRUE)
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
  # 创建绘图 - 移除所有坐标轴标题，增大字体
  # 创建绘图 - 移除所有坐标轴标题，增大字体
  # 在ggplot的theme部分，进行以下修改：
  
  # 创建绘图 - 移除标题，取消加粗，字体再大一倍
  # 创建绘图 - 移除标题，取消加粗，字体再大一倍
  p <- ggplot(dat, aes(y = row_means, x = lat)) +     
    # 先绘制置信区间（放在底层）
    geom_ribbon(aes(ymin = row_means - row_sds, 
                    ymax = row_means + row_sds), 
                fill = "lightgrey", alpha = 0.5) +  # 添加置信区间带
    # 再绘制线条（放在上层）
    geom_line(size = 2, color = "orange") +    # 线条粗细
    labs(x = "Latitude (°)", y = y_label) +  # 修改这里：将x轴标签设置为NULL
    theme_classic() +               
    coord_flip() +  # 翻转x和y轴
    scale_y_continuous(limits = c(min(dat$row_means, na.rm = TRUE), 
                                  max(dat$row_means, na.rm = TRUE))) +  # 基于数据设置动态限制
    theme(
      axis.text = element_text(size = 90, color = "black"),   # 坐标轴文本大小（再大一倍），取消加粗
      axis.title = element_text(size = 90, color = "black"),  # 坐标轴标题大小（再大一倍），取消加粗
      axis.title.x = element_blank(),  # 添加这行：完全移除x轴标题
      plot.title = element_blank(),  # 完全移除标题
      panel.grid = element_blank(),  # 移除网格线
      axis.line = element_line(color = "black", size = 1.5),  # 坐标轴线
      panel.background = element_rect(fill = "white", colour = "black", size = 1.5),  # 面板边框
      plot.margin = margin(30, 30, 30, 30)  # 增加边距以适应更大字体
    )
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
input_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean"
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# 创建文件列表，使用文件名作为键
files <- setNames(as.list(tif_files), tools::file_path_sans_ext(basename(tif_files)))

# 1. 输出目录 (更新文件名以反映新的分组)
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\biomeplot_1_4_6"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1b. 创建转换后tif文件的输出目录
transformed_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\transformed"
if (!dir.exists(transformed_dir)) dir.create(transformed_dir, recursive = TRUE)

# 1c. 创建统计结果CSV文件输出目录 (更新文件名以反映新的分组)
stats_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\statistics_1_4_6"
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

# 1d. 创建绘图数据保存目录 (更新文件名以反映新的分组)
plot_data_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\plot_data_1_4_6"
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
        Q1 = quantile(soil, 0.05, na.rm = TRUE),
        Q3 = quantile(soil, 0.95, na.rm = TRUE),
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
        q3_val = quantile(soil, 0.95, na.rm = TRUE)
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
input_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\clean"
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
out_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\landuseplot"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1c. 创建统计结果CSV文件输出目录
stats_dir <- "F:\\model\\results\\sixth\\current_mapping\\projected\\\\statistics"
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
        Q1 = quantile(soil, 0.05, na.rm = TRUE),
        Q3 = quantile(soil, 0.95, na.rm = TRUE),
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
 