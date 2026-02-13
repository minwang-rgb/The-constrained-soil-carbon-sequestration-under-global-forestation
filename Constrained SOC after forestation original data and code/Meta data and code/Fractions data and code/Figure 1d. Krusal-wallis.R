




#########cloud and rain plot########
library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1 & 2. 数据处理 ---
df <- read.csv("/Users/min/Desktop/NEE/SImeta/20205SIKOoutli0625.csv")

df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

# 更新因子层级顺序
level_order <- c(
  "Combined", 
  "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", 
  "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 显著性分析与 P 值计算 ---

# 3.1 第1层：主要组比较 (All Methods, Total Physical, Total Chemical, Combined)
main_groups <- c("All Methods", "Total Physical", "Total Chemical", "Combined")
df_main <- df_final %>% filter(Method %in% main_groups)

# 主要组ANOVA检验
if (n_distinct(df_main$Method) > 1) {
  anova_main <- aov(yi ~ Method, data = df_main)
  hsd_main <- TukeyHSD(anova_main)
  letters_main <- multcompLetters4(anova_main, hsd_main)
  cld_main <- data.frame(letters = letters_main$Method$Letters)
  cld_main$Method <- rownames(cld_main)
} else {
  cld_main <- data.frame(
    Method = main_groups,
    letters = "A",
    stringsAsFactors = FALSE
  )
}

# 转换第一层字母为大写
cld_main$letters <- toupper(cld_main$letters)

# 3.2 第2层：化学方法内部比较 (KMnO4 vs Acid solution)
chemical_groups <- c("KMnO4", "Acid solution")
df_chemical_internal <- df_final %>% filter(Method %in% chemical_groups)

# 化学方法内部ANOVA
if (n_distinct(df_chemical_internal$Method) > 1) {
  anova_chem <- aov(yi ~ Method, data = df_chemical_internal)
  hsd_chem <- TukeyHSD(anova_chem)
  letters_chem <- multcompLetters4(anova_chem, hsd_chem)
  cld_chem <- data.frame(letters = letters_chem$Method$Letters)
  cld_chem$Method <- rownames(cld_chem)
} else {
  cld_chem <- data.frame(
    Method = chemical_groups,
    letters = "a",
    stringsAsFactors = FALSE
  )
}

# 转换第二层字母为小写
cld_chem$letters <- tolower(cld_chem$letters)

# 3.3 第3层：物理方法内部比较 (POCMAOC, Aggregate, Density)
physical_groups <- c("POCMAOC", "Aggregate", "Density")
df_physical_internal <- df_final %>% filter(Method %in% physical_groups)

# 物理方法内部ANOVA
if (n_distinct(df_physical_internal$Method) > 1) {
  anova_phys <- aov(yi ~ Method, data = df_physical_internal)
  hsd_phys <- TukeyHSD(anova_phys)
  letters_phys <- multcompLetters4(anova_phys, hsd_phys)
  cld_phys <- data.frame(letters = letters_phys$Method$Letters)
  cld_phys$Method <- rownames(cld_phys)
} else {
  cld_phys <- data.frame(
    Method = physical_groups,
    letters = "a",
    stringsAsFactors = FALSE
  )
}

# 转换第三层字母为小写
cld_phys$letters <- tolower(cld_phys$letters)

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld_main %>% rename(letters_main = letters), by = "Method") %>%
  left_join(cld_chem %>% rename(letters_chem = letters), by = "Method") %>%
  left_join(cld_phys %>% rename(letters_phys = letters), by = "Method")

# --- 5. 绘图 ---
method_colors <- c(
  "All Methods"    = "#2E86AB", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)

# 创建标注位置数据
method_levels <- levels(df_final$Method)

# 准备标注数据
annotation_data <- data.frame()
for (i in 1:length(method_levels)) {
  current_group <- method_levels[i]
  group_index <- which(method_levels == current_group)
  
  annotation_data <- rbind(annotation_data, data.frame(
    Method = current_group,
    y_position = group_index,
    
    # 第1层：主要组标注（大写字母）
    label_main = ifelse(current_group %in% main_groups, 
                       cld_main$letters[cld_main$Method == current_group], NA),
    
    # 第2层：化学方法内部标注（小写字母）
    label_chem = ifelse(current_group %in% chemical_groups, 
                       cld_chem$letters[cld_chem$Method == current_group], NA),
    
    # 第3层：物理方法内部标注（小写字母）
    label_phys = ifelse(current_group %in% physical_groups, 
                       cld_phys$letters[cld_phys$Method == current_group], NA)
  ))
}

# 调整y轴范围
y_range_adjustment <- 1.25

p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  coord_flip(ylim = c(y_min * y_range_adjustment, y_max * 1.1)) +
  
  geom_text(
    data = annotation_data %>% filter(!is.na(label_main)),
    aes(x = y_position, y = y_min* 1.1 , label = label_main),
    inherit.aes = FALSE, 
    size = 6,  # 增大字体大小
    color = "black",
    hjust = 0.5,
    vjust = 0.5
  ) +
  
  # 第2层：化学方法内部标注 (橙色，小写字母)
  geom_text(
    data = annotation_data %>% filter(!is.na(label_chem)),
    aes(x = y_position, y = y_min* 1.1, label = label_chem),
    inherit.aes = FALSE, 
    size = 8, 
    hjust = 0.5,
    vjust = 0.5
  ) +
  
  # 第3层：物理方法内部标注 (绿色，小写字母)
  geom_text(
    data = annotation_data %>% filter(!is.na(label_phys)),
    aes(x = y_position, y = y_min* 1.1, label = label_phys),
    inherit.aes = FALSE, 
    size =8, 
    hjust = 0.5,
    vjust = 0.5
  ) +
  
  # 标注 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max * 0.8, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = 0, size = 7, fontface = "italic", color = "black"
  ) +
  
  theme_bw(base_size = 25) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|All|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(linewidth = 1, color = "black")
  ) 

print(p)


# 保存图片
ggsave("/Users/min/Desktop/NEE/fractions/Method_Grouped_CloudRain_ThreeLayers_HSD_Case.png", 
       p,   width = 10, height = 8.5, dpi = 300, bg = "transparent")
















library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1 & 2. 数据处理 ---
# (此部分保持不变)
df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

level_order <- c(
  "Combined", 
  "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", 
  "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 显著性分析 (Dunn's 检验) ---

# 1. 整体非参数检验 (Kruskal-Wallis)
kw_test <- df_final %>% kruskal_test(yi ~ Method)
p_val_label <- paste0("Kruskal-Wallis, P ", p_format(kw_test$p, digits = 3))

# 2. 事后检验 (Dunn's test)
# 使用 Bonferroni 校正
dunn_res <- df_final %>% dunn_test(yi ~ Method, p.adjust.method = "bonferroni")

# 3. 将 Dunn's 检验结果转换为字母顺序
# multcompLetters 需要一个命名的向量，名称格式为 "group1-group2"
p_vector <- dunn_res$p.adj
names(p_vector) <- paste0(dunn_res$group1, "-", dunn_res$group2)
letters_list <- multcompLetters(p_vector)

# 转换为数据框
cld <- data.frame(
  Method = names(letters_list$Letters),
  letters = letters_list$Letters,
  stringsAsFactors = FALSE
)

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld, by = "Method")

# --- 5. 绘图参数设置 ---
method_colors <- c(
  "All Methods"    = "#444444", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)
y_range <- y_max - y_min
letter_pos <- y_min - (y_range * 0.15) # 字母在左侧 15% 处

# --- 6. 绘图 ---
p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  
  # 标注 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max * 0.6, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = -0.8, size = 4.5, fontface = "italic", color = "black"
  ) +
  
  # 标注显著性字母 (左侧)
  geom_text(
    data = stats_summary,
    aes(x = Method, y = letter_pos, label = letters),
    inherit.aes = FALSE, size = 5, fontface = "bold", color = "black", hjust = 0.5
  ) +
  
  # 标注整体 P 值
  annotate(
    "text", 
    x = length(level_order) + 0.5, 
    y = y_min, 
    label = p_val_label, 
    size = 4.5, fontface = "bold.italic", color = "black", hjust = 0
  ) +
  
  coord_flip(clip = "off") + 
  expand_limits(y = letter_pos - 0.1) + 
  theme_bw(base_size = 18) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|All|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(linewidth = 1, color = "black"),
    plot.margin = margin(10, 20, 10, 10)，
    panel.background = element_rect(fill = "transparent", color = NA), # 面板背景透明
    plot.background = element_rect(fill = "transparent", color = NA),  # 整幅图背景透明
    legend.background = element_rect(fill = "transparent", color = NA), # 图例背景透明
  )

print(p)


# 保存图片，设置背景为透明
ggsave("/Users/min/Desktop/NEE/fractions/Method_Grouped_CloudRain.png", 
       plot = p, 
       width = 10, 
       height = 12, 
       dpi = 300, 
       bg = "transparent") # 关键参数



























library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1 & 2. 数据处理 (保持不变) ---
# df <- read.csv("/Users/min/Desktop/NEE/SImeta/20205SIKOoutli0625.csv")

df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

level_order <- c(
  "Combined", 
  "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", 
  "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 分组显著性分析 (两组 Dunn's 检验) ---

# 定义两个分析集合
group_fractions <- c("Combined", "KMnO4", "Acid solution", "POCMAOC", "Aggregate", "Density")
group_totals    <- c("Total Chemical", "Total Physical", "All Methods")

# 函数：执行 Dunn's 并返回字母
get_dunn_letters <- function(data, target_methods) {
  sub_data <- data %>% filter(Method %in% target_methods)
  # Dunn's test
  dunn <- sub_data %>% dunn_test(yi ~ Method, p.adjust.method = "bonferroni")
  # Generate letters
  p_vec <- dunn$p.adj
  names(p_vec) <- paste0(dunn$group1, "-", dunn$group2)
  res <- multcompLetters(p_vec)
  return(data.frame(Method = names(res$Letters), letters = res$Letters))
}

# 分别计算
cld_fractions <- get_dunn_letters(df_final, group_fractions)
cld_totals    <- get_dunn_letters(df_final, group_totals)

# 合并字母结果
cld_all <- bind_rows(cld_fractions, cld_totals)

# 获取两个整体 P 值用于标注
kw_f <- df_final %>% filter(Method %in% group_fractions) %>% kruskal_test(yi ~ Method)
kw_t <- df_final %>% filter(Method %in% group_totals) %>% kruskal_test(yi ~ Method)
p_val_combined <- paste0("P_fractions: ", p_format(kw_f$p, digits = 3), 
                         " | P_totals: ", p_format(kw_t$p, digits = 3))

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld_all, by = "Method")

# --- 5. 绘图参数设置 ---
method_colors <- c(
  "All Methods"    = "#444444", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)
y_range <- y_max - y_min
letter_pos <- y_min - (y_range * 0.18) # 增加了一点点左侧间距

# --- 6. 绘图 ---
p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  
  # 标注 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = 0, size = 4, fontface = "italic", color = "black"
  ) +
  
  # 标注显著性字母 (左侧)
  geom_text(
    data = stats_summary,
    aes(x = Method, y = letter_pos, label = letters),
    inherit.aes = FALSE, size = 5.5, fontface = "bold", color = "black", hjust = 0.5
  ) +
  
  # 标注两组整体 P 值
  annotate(
    "text", 
    x = length(level_order) + 0.6, 
    y = y_min, 
    label = p_val_label, # 如果需要显示合并的P值，可改为 p_val_combined
    size = 4, fontface = "bold.italic", color = "black", hjust = 0
  ) +
  
  coord_flip(clip = "off") + 
  expand_limits(y = letter_pos - 0.2) + 
  theme_bw(base_size = 18) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    # 加粗特定的分类标签
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|All|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(linewidth = 1, color = "black"),
    plot.margin = margin(10, 40, 10, 10), # 增加边距
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )


print(p)
# 保存图片
ggsave("/Users/min/Desktop/NEE/fractions/Method_Grouped_CloudRain_Transparent.png", 
       plot = p, width = 11, height = 12, dpi = 300, bg = "transparent")





































library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1. 数据读取与清洗 ---
# 请确保您的文件路径正确
df <- read.csv("/Users/min/Desktop/NEE/SImeta/20205SIKOoutli0625.csv")

df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  # 统一名称：将 Acid 更改为 Acid solution
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

# --- 2. 构建汇总组 ---
df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

# 设置因子层级（决定图中从下到上的顺序）
level_order <- c(
  "Combined", 
  "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", 
  "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 分组显著性分析 (两组 Dunn's 检验) ---

# 定义两个独立的统计集合
group_fractions <- c("Combined", "KMnO4", "Acid solution", "POCMAOC", "Aggregate", "Density")
group_totals    <- c("Total Chemical", "Total Physical", "All Methods")

# 编写函数：执行 Dunn's 并返回字母
get_dunn_letters <- function(data, target_methods) {
  sub_data <- data %>% filter(Method %in% target_methods)
  # Dunn's test (使用 Bonferroni 校正)
  dunn <- sub_data %>% dunn_test(yi ~ Method, p.adjust.method = "bonferroni")
  # 生成字母
  p_vec <- dunn$p.adj
  names(p_vec) <- paste0(dunn$group1, "-", dunn$group2)
  res <- multcompLetters(p_vec)
  return(data.frame(Method = names(res$Letters), letters = res$Letters))
}

# 分别计算显著性字母
cld_fractions <- get_dunn_letters(df_final, group_fractions)
cld_totals    <- get_dunn_letters(df_final, group_totals)
cld_all       <- bind_rows(cld_fractions, cld_totals)

# 分别计算整体 Kruskal-Wallis P 值
kw_f <- df_final %>% filter(Method %in% group_fractions) %>% kruskal_test(yi ~ Method)
kw_t <- df_final %>% filter(Method %in% group_totals) %>% kruskal_test(yi ~ Method)

# 构造双 P 值标注文本
p_val_display <- paste0(
  "Kruskal-Wallis Test:\n",
  "Fractions: P ", p_format(kw_f$p, digits = 3), "\n",
  "Totals: P ", p_format(kw_t$p, digits = 3)
)

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld_all, by = "Method")

# --- 5. 绘图参数设置 ---
method_colors <- c(
  "All Methods"    = "#444444", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)
y_range <- y_max - y_min
# 显著性字母在左侧的定位（根据 y 轴范围动态调整）
letter_pos <- y_min - (y_range * 0.20) 

# --- 6. 绘图 ---
p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  # 添加零线
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  # 云：分布密度图
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  # 雨：原始数据散点
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  # 箱线图
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  # 红色菱形表示均值
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  
  # 颜色设置
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  # 坐标轴标签美化
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  
  # 标注样本量 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = 0, size = 4, fontface = "italic", color = "black"
  ) +
  
  # 标注显著性字母 (左侧)
  geom_text(
    data = stats_summary,
    aes(x = Method, y = letter_pos, label = letters),
    inherit.aes = FALSE, size = 5.5, fontface = "bold", color = "black", hjust = 0.5
  ) +
  
  # 标注两个组别的整体 P 值
  annotate(
    "text", 
    x = length(level_order) + 0.8, 
    y = y_min, 
    label = p_val_display, 
    size = 4.5, fontface = "bold.italic", color = "black", hjust = 0, vjust = 1
  ) +
  
  # 翻转坐标轴
  coord_flip(clip = "off") + 
  expand_limits(y = letter_pos - 0.2) + 
  
  # 主题与透明设置
  theme_bw(base_size = 18) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    # 加粗特定的汇总组标签
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|TOTAL|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(linewidth = 1, color = "black"),
    plot.margin = margin(10, 50, 10, 10), # 增加右侧边距防止 n= 被切掉
    
    # 背景透明设置
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

# --- 7. 打印并保存图片 ---
print(p)

ggsave("/Users/min/Desktop/NEE/fractions/Method_Grouped_CloudRain_Transparent.png", 
       plot = p, 
       width = 11, 
       height = 12, 
       dpi = 300, 
       bg = "transparent")






















library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1. 数据读取与清洗 ---
df <- read.csv("/Users/min/Desktop/NEE/SImeta/20205SIKOoutli0625.csv")

df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

# --- 2. 构建汇总组 ---
df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

level_order <- c(
  "Combined", 
  "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", 
  "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 分组显显著性分析 (仅计算字母) ---
group_fractions <- c("Combined", "KMnO4", "Acid solution", "POCMAOC", "Aggregate", "Density")
group_totals    <- c("Total Chemical", "Total Physical", "All Methods")

get_dunn_letters <- function(data, target_methods) {
  sub_data <- data %>% filter(Method %in% target_methods)
  dunn <- sub_data %>% dunn_test(yi ~ Method, p.adjust.method = "bonferroni")
  p_vec <- dunn$p.adj
  names(p_vec) <- paste0(dunn$group1, "-", dunn$group2)
  res <- multcompLetters(p_vec)
  return(data.frame(Method = names(res$Letters), letters = res$Letters))
}

cld_fractions <- get_dunn_letters(df_final, group_fractions)
cld_totals    <- get_dunn_letters(df_final, group_totals)
cld_all       <- bind_rows(cld_fractions, cld_totals)

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld_all, by = "Method")

# --- 5. 绘图参数设置 ---
method_colors <- c(
  "All Methods"    = "#444444", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)
y_range <- y_max - y_min
letter_pos <- y_min - (y_range * 0.15) 

# --- 6. 绘图 ---
p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  
  # 仅标注样本量 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = 0, size = 4, fontface = "italic", color = "black"
  ) +
  
  # 仅标注显著性字母
  geom_text(
    data = stats_summary,
    aes(x = Method, y = letter_pos, label = letters),
    inherit.aes = FALSE, size = 5.5, fontface = "bold", color = "black", hjust = 0.5
  ) +
  
  coord_flip(clip = "off") + 
  expand_limits(y = letter_pos - 0.2) + 
  theme_bw(base_size = 18) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|TOTAL|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(linewidth = 1, color = "black"),
    plot.margin = margin(10, 50, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

print(p)

# 保存图片
ggsave("/Users/min/Desktop/NEE/fractions/Method_NoPval_CloudRain.png", 
       plot = p, width = 11, height = 12, dpi = 300, bg = "transparent")






















library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1. 数据读取与清洗 ---
df <- read.csv("/Users/min/Desktop/NEE/SImeta/20205SIKOoutli0625.csv")

df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

# --- 2. 构建汇总组 ---
df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

level_order <- c(
  "Combined", "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 显著性分析 ---
group_fractions <- c("Combined", "KMnO4", "Acid solution", "POCMAOC", "Aggregate", "Density")
group_totals    <- c("Total Chemical", "Total Physical", "All Methods")

# 内部函数：生成字母标注
get_dunn_letters <- function(data, target_methods) {
  sub_data <- data %>% filter(Method %in% target_methods)
  dunn <- sub_data %>% dunn_test(yi ~ Method, p.adjust.method = "bonferroni")
  p_vec <- dunn$p.adj
  names(p_vec) <- paste0(dunn$group1, "-", dunn$group2)
  res <- multcompLetters(p_vec)
  return(data.frame(Method = names(res$Letters), letters = res$Letters))
}

# 计算字母 (用于标注差异)
cld_fractions <- get_dunn_letters(df_final, group_fractions)
cld_totals    <- get_dunn_letters(df_final, group_totals)
cld_all       <- bind_rows(cld_fractions, cld_totals)

# --- 特别修改：仅对大类执行 ANOVA P 值检验 ---
df_totals_only <- df_final %>% filter(Method %in% group_totals)
res_aov_totals <- aov(yi ~ Method, data = df_totals_only)
p_val_aov_totals <- summary(res_aov_totals)[[1]][["Pr(>F)"]][1]

p_val_display <- paste0(
  "One-way ANOVA (Totals):\nP ", p_format(p_val_aov_totals, digits = 3)
)

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld_all, by = "Method")

# --- 5. 绘图参数 ---
method_colors <- c(
  "All Methods"    = "#444444", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)
y_range <- y_max - y_min
letter_pos <- y_min - (y_range * 0.18) 

# --- 6. 绘图 ---
p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  
  # 标注字母
  geom_text(
    data = stats_summary,
    aes(x = Method, y = letter_pos, label = letters),
    inherit.aes = FALSE, size = 5.5, fontface = "bold", color = "black", hjust = 0.5
  ) +
  
  # 标注大类 ANOVA P 值
  annotate(
    "text", 
    x = length(level_order) + 0.6, 
    y = y_min, 
    label = p_val_display, 
    size = 4.8, fontface = "bold.italic", color = "black", hjust = 0, vjust = 1
  ) +
  
  # 标注 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max * 0.6, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = -0.8, size = 4.5, fontface = "italic", color = "black"
  ) +
  
  coord_flip(clip = "off") + 
  expand_limits(y = letter_pos - 0.2) + 
  theme_bw(base_size = 18) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|TOTAL|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 50, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

print(p)

ggsave("/Users/min/Desktop/NEE/fractions/Method_ANOVA_Total_Only.png", 
       plot = p, width = 11, height = 12, dpi = 300, bg = "transparent")













##########this is the final plot###########


library(ggplot2)
library(ggdist)
library(dplyr)
library(colorspace)
library(rstatix)
library(multcompView)

# --- 1. 数据读取与清洗 ---
df <- read.csv("/Users/min/Desktop/NEE/SImeta/20205SIKOoutli0625.csv")

df_clean <- df %>%
  group_by(Method) %>%
  mutate(
    IQR_val = IQR(yi, na.rm = TRUE),
    Q1 = quantile(yi, 0.25, na.rm = TRUE),
    Q3 = quantile(yi, 0.75, na.rm = TRUE),
    lower = Q1 - 1.5 * IQR_val,
    upper = Q3 + 1.5 * IQR_val
  ) %>%
  filter(yi >= lower & yi <= upper) %>%
  ungroup() %>%
  mutate(Method = ifelse(Method == "Acid", "Acid solution", Method))

# --- 2. 构建汇总组 ---
df_chemical <- df_clean %>% filter(Method %in% c("Acid solution", "KMnO4")) %>% mutate(Method = "Total Chemical")
df_physical <- df_clean %>% filter(Method %in% c("Density", "Aggregate", "POCMAOC")) %>% mutate(Method = "Total Physical")
df_all      <- df_clean %>% mutate(Method = "All Methods")

df_final <- bind_rows(df_clean, df_chemical, df_physical, df_all)

level_order <- c(
  "Combined", "KMnO4", "Acid solution", "Total Chemical", 
  "POCMAOC", "Aggregate", "Density", "Total Physical", "All Methods"
)
df_final$Method <- factor(df_final$Method, levels = level_order)

# --- 3. 统计分析 (Kruskal-Wallis + Dunn's Bonferroni) ---
group_fractions <- c("Combined", "KMnO4", "Acid solution", "POCMAOC", "Aggregate", "Density")
group_totals    <- c("Total Chemical", "Total Physical", "All Methods")

# 统一使用 Dunn's + Bonferroni 校正 (最严格)
get_dunn_letters <- function(data, target_methods) {
  sub_data <- data %>% filter(Method %in% target_methods)
  dunn <- sub_data %>% dunn_test(yi ~ Method, p.adjust.method = "bonferroni")
  p_vec <- dunn$p.adj
  names(p_vec) <- paste0(dunn$group1, "-", dunn$group2)
  res <- multcompLetters(p_vec)
  return(data.frame(Method = names(res$Letters), letters = res$Letters))
}

cld_fractions <- get_dunn_letters(df_final, group_fractions)
cld_totals    <- get_dunn_letters(df_final, group_totals)
cld_all       <- bind_rows(cld_fractions, cld_totals)

# --- 核心修改：将大类检验改为 Kruskal-Wallis ---
df_totals_only <- df_final %>% filter(Method %in% group_totals)
kw_totals <- df_totals_only %>% kruskal_test(yi ~ Method)

p_val_display <- paste0(
  "Kruskal-Wallis (Totals):\nP ", p_format(kw_totals$p, digits = 4)
)

# --- 4. 统计汇总 ---
stats_summary <- df_final %>%
  group_by(Method) %>%
  get_summary_stats(yi, type = "common") %>%
  left_join(cld_all, by = "Method")

# --- 5. 绘图参数 ---
method_colors <- c(
  "All Methods"    = "#444444", 
  "Total Chemical" = "#009E73", "Acid solution" = "#26D1A6", "KMnO4" = "#80E7D0",
  "Total Physical" = "#E69F00", "Density" = "#FFC233", "Aggregate" = "#FFE599", "POCMAOC" = "#F0E442",
  "Combined"       = "#D55E00"
)

y_min <- min(df_final$yi, na.rm = TRUE)
y_max <- max(df_final$yi, na.rm = TRUE)
y_range <- y_max - y_min
letter_pos <- y_min - (y_range * 0.18) 

# --- 6. 绘图 ---
p <- ggplot(df_final, aes(x = Method, y = yi, fill = Method, color = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) +
  geom_point(size = 1.2, alpha = 0.15, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  
  scale_fill_manual(values = method_colors) +
  scale_color_manual(values = method_colors) +
  scale_x_discrete(labels = c(
    "KMnO4" = expression(KMnO[4]),
    "POCMAOC" = "POC&MAOC",
    "Total Chemical" = "CHEMICAL (Total)",
    "Total Physical" = "PHYSICAL (Total)",
    "All Methods" = "TOTAL"
  )) +
  
  # 标注字母
  geom_text(
    data = stats_summary,
    aes(x = Method, y = letter_pos, label = letters),
    inherit.aes = FALSE, size = 5.5, fontface = "bold", color = "black", hjust = 0.6
  ) +
  
  # 标注大类 Kruskal-Wallis P 值
  annotate(
    "text", 
    x = length(level_order)+0.2, 
    y = y_min-0.7, 
    label = p_val_display, 
    size = 4.8, fontface = "bold.italic", color = "black", hjust = 0, vjust = -0.1
  ) +
  
  # 标注 n=
  geom_text(
    data = stats_summary,
    aes(x = Method, y = y_max * 0.6, 
        label = paste0("n=", format(n, big.mark = ",", trim = TRUE))),
    inherit.aes = FALSE, hjust = -0.5, size = 5, fontface = "italic", color = "black"
  ) +
  
  
  coord_flip(clip = "off") + 
  expand_limits(y = letter_pos - 0.2) + 
  theme_bw(base_size = 20) +
  labs(x = "", y = "Lability Index (LI)") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      color = "black",
      face = ifelse(grepl("Total|TOTAL|CHEMICAL|PHYSICAL", levels(df_final$Method)), "bold", "plain")
    ),
    panel.grid = element_blank(),
    plot.margin = margin(10, 60, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

print(p)

ggsave("/Users/min/Desktop/NEE/fractions/Method_Kruskal_Total_Only.png", 
       plot = p, width = 9, height = 10, dpi = 300, bg = "transparent")

