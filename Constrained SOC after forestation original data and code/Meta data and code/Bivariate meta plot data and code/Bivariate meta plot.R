
# ==========================================
# 1. 加载库
# ==========================================
library(tidyverse)
library(ggrepel)

# ==========================================
# 2. 数据加载与清洗
# ==========================================
df_raw <- read.csv("F:/wang/two_D/plot.csv", check.names = FALSE)
raw_data <- read.csv("F:/wang/two_D/20206SI.csv")

# 从 df_raw 中提取 30+cm 的样本量 N 值
sample_n_data <- df_raw %>%
  filter(facet == "30+cm") %>%
  select(Landuse, group, N) %>%
  pivot_wider(names_from = group, values_from = N) %>%
  rename(N_SOC = N_SOC, N_Sensitivity = N_Sensitivity)

# 清洗统计汇总数据
df_clean <- df_raw %>% 
  filter(facet == "30+cm") %>% 
  mutate(Landuse = str_to_title(trimws(as.character(Landuse)))) %>%
  mutate(Landuse = recode(Landuse, 
                          "Sandy" = "Desert & Semi-desert",
                          "Bareland" = "Bare Land",
                          "Natural" = "Natural Forest")) %>%
  pivot_wider(
    id_cols = c(facet, Landuse), 
    names_from = group, 
    values_from = c(OR, Lower, Upper)
  ) %>%
  rename(
    OR_SOC = OR_N_SOC,
    OR_Sensitivity = OR_N_Sensitivity,
    Lower_SOC = Lower_N_SOC,
    Upper_SOC = Upper_N_SOC,
    Lower_Sensitivity = Lower_N_Sensitivity,
    Upper_Sensitivity = Upper_N_Sensitivity
  ) %>%
  left_join(
    sample_n_data %>% 
      mutate(Landuse = recode(Landuse, 
                              "Sandy" = "Desert & Semi-desert",
                              "Bareland" = "Bare Land",
                              "Natural" = "Natural Forest")), 
    by = "Landuse"
  ) %>%
  mutate(
    label_full = paste0(Landuse, "\n", 
                        "(n_SOC=", N_SOC, ")\n", 
                        "(n_LI=", N_Sensitivity, ")")
  )

# 处理原始散点数据 (筛选 subsoil 并执行 200% 阈值过滤)
subsoil_clean <- raw_data %>% 
  filter(Soil.layer == "subsoil") %>% 
  mutate(Landuse = str_to_title(trimws(as.character(Landuse)))) %>%
  mutate(Landuse = recode(Landuse, 
                          "Sandy" = "Desert & Semi-desert",
                          "Bareland" = "Bare Land",
                          "Natural" = "Natural Forest")) %>%
  filter(LI_yi <= 200, SOC_yi <= 200)

# ==========================================
# 3. 定义颜色方案
# ==========================================
landuse_colors <- c(
  "Desert & Semi-desert" = "#CD8500",
  "Cropland"             = "#668B8B",
  "Natural Forest"       = "#228B22",
  "Grassland"            = "#008fd5",
  "Bare Land"            = "#D2691E",
  "Abandoned"            = "#8073ac"
)

# ==========================================
# 4. 绘制统一主图
# ==========================================
final_plot <- ggplot() +
  # 1. 底层散点
  geom_point(data = subsoil_clean, 
             aes(x = SOC_yi, y = LI_yi, color = Landuse), 
             alpha = 0.12, size = 3, show.legend = FALSE) + 
  # 2. 水平误差棒
  geom_errorbarh(data = df_clean, 
                 aes(y = OR_Sensitivity, xmin = Lower_SOC, xmax = Upper_SOC, color = Landuse), 
                 height = 0, linewidth = 1.8) + 
  # 3. 垂直误差棒
  geom_errorbar(data = df_clean, 
                aes(x = OR_SOC, ymin = Lower_Sensitivity, ymax = Upper_Sensitivity, color = Landuse), 
                width = 0, linewidth = 1.8) + 
  # 4. 中心均点
  geom_point(data = df_clean, 
             aes(x = OR_SOC, y = OR_Sensitivity, fill = Landuse), 
             size = 9, shape = 21, color = "black", stroke = 1.5) +
  # 5. 颜色映射
  scale_color_manual(values = landuse_colors) +
  scale_fill_manual(values = landuse_colors) +
  # 6. 辅助线
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1.2) +
  # 7. 优化后的标注 (防遮挡核心)
  geom_text_repel(
    data = df_clean,
    aes(x = OR_SOC, y = OR_Sensitivity, label = label_full),color = 'black',
    size = 9,
    lineheight = 0.85,
    # 排斥逻辑设置
    box.padding = 3.5,        # 增加标签间距
    point.padding = 2.5,      # 增加标签与点的间距
    min.segment.length = 0,   # 始终显示引线
    segment.size = 0.8,
    segment.color = "grey40",
    force = 60,               # 加强排斥力度
    force_pull = 0.1,         # 减弱点对标签的吸引力
    direction = "both",       # 允许向四周散开
    max.iter = 100000,        # 增加计算次数以寻找最优位置
    show.legend = FALSE
  ) +
  # 8. 标题与标签
  labs(
    x = expression(bold(Delta * " SOC (%)")),
    y = expression(bold(Delta * " LI (%)")),
    title = "Subsoil Effects (30+ cm)"
  ) +
  # 9. 主题设置
  theme_bw(base_size = 32) + 
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 30),
    axis.text = element_text(color = "black", face = "bold", size = 30),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 32),
    legend.position = "none")

# ==========================================
# 5. 输出与保存
# ==========================================
print(final_plot)

ggsave("F:/wang/two_D/Subsoil_SOC_LI_Plot_Adjusted.png", 
       final_plot, width = 14, height =10, dpi = 600)





























# ==========================================
# 1. 加载库
# ==========================================
library(tidyverse)
library(patchwork)
library(ggrepel)

# ==========================================
# 2. 数据加载与清洗
# ==========================================
df_raw <- read.csv("F:/wang/two_D/plot.csv", check.names = FALSE)
raw_data <- read.csv("F:/wang/two_D/20206SI.csv")

# A. 提取样本量
sample_n_data <- df_raw %>%
  filter(facet == "0-30cm") %>%
  select(Landuse, group, N) %>%
  pivot_wider(names_from = group, values_from = N)

# B. 清洗汇总数据
df_clean <- df_raw %>% 
  filter(facet == "0-30cm") %>%
  mutate(Landuse = str_to_title(trimws(as.character(Landuse)))) %>%
  mutate(Landuse = recode(Landuse, 
                          "Sandy" = "Desert & Semi-desert",
                          "Bareland" = "Bare Land",
                          "Natural" = "Natural Forest")) %>%
  pivot_wider(
    id_cols = c(facet, Landuse), 
    names_from = group, 
    values_from = c(OR, Lower, Upper)
  ) %>%
  rename(
    OR_SOC = OR_N_SOC, OR_Sensitivity = OR_N_Sensitivity,
    Lower_SOC = Lower_N_SOC, Upper_SOC = Upper_N_SOC,
    Lower_Sensitivity = Lower_N_Sensitivity, Upper_Sensitivity = Upper_N_Sensitivity
  ) %>%
  left_join(
    sample_n_data %>% 
      mutate(Landuse = recode(Landuse, 
                              "Sandy" = "Desert & Semi-desert",
                              "Bareland" = "Bare Land",
                              "Natural" = "Natural Forest")), 
    by = "Landuse"
  ) %>%
  mutate(
    label_full = paste0(Landuse, "\n", 
                        "(n_SOC=", N_SOC, ")\n", 
                        "(n_LI=", N_Sensitivity, ")"),
    label_n_only = paste0("(n_SOC=", N_SOC, ")\n", 
                          "(n_LI=", N_Sensitivity, ")")
  )

# C. 清洗原始散点
topsoil_clean <- raw_data %>% 
  filter(Soil.layer == "topsoil") %>%
  mutate(Landuse = str_to_title(trimws(as.character(Landuse)))) %>%
  mutate(Landuse = recode(Landuse, 
                          "Sandy" = "Desert & Semi-desert",
                          "Bareland" = "Bare Land",
                          "Natural" = "Natural Forest"))

df_main  <- df_clean %>% filter(Landuse != "Desert & Semi-desert")
df_sandy <- df_clean %>% filter(Landuse == "Desert & Semi-desert")

topsoil_main  <- topsoil_clean %>% 
  filter(Landuse != "Desert & Semi-desert") %>%
  filter(SOC_yi <= 200, LI_yi <= 200)

topsoil_sandy <- topsoil_clean %>% 
  filter(Landuse == "Desert & Semi-desert")

# ==========================================
# 3. 定义颜色方案
# ==========================================
landuse_colors <- c(
  "Desert & Semi-desert" = "#CD8500",
  "Cropland"             = "#668B8B",
  "Natural Forest"       = "#228B22",
  "Grassland"            = "#008fd5",
  "Bare Land"            = "#D2691E",
  "Abandoned"            = "#8073ac"
)

# ==========================================
# 4. 绘制主图 (p_main) - 优化了标注偏移
# ==========================================
p_main <- ggplot() +
  geom_point(data = topsoil_main, 
             aes(x = SOC_yi, y = LI_yi, color = Landuse), 
             alpha = 0.1, size = 3, show.legend = FALSE) + 
  geom_errorbarh(data = df_main, 
                 aes(y = OR_Sensitivity, xmin = Lower_SOC, xmax = Upper_SOC, color = Landuse), 
                 height = 0, linewidth = 1.8) + 
  geom_errorbar(data = df_main, 
                aes(x = OR_SOC, ymin = Lower_Sensitivity, ymax = Upper_Sensitivity, color = Landuse), 
                width = 0, linewidth = 1.8) + 
  geom_point(data = df_main, 
             aes(x = OR_SOC, y = OR_Sensitivity, fill = Landuse), 
             size = 9, shape = 21, color = "black", stroke = 1.5) +
  scale_color_manual(values = landuse_colors) +
  scale_fill_manual(values = landuse_colors) +
  labs(
    x = expression(bold(Delta * " SOC (%)")),
    y = expression(bold(Delta * " LI (%)")),
    title = "Topsoil Effects (0-30cm)"
  ) +
  theme_bw(base_size = 32) + 
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 30),
    axis.text = element_text(color = "black", face = "bold", size = 30),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 32),
    legend.position = "none"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1.2) +
  # 改进的标注策略：针对 Cropland 强制向下偏移，避开右上角子图
  geom_text_repel(
    data = df_main,
    aes(x = OR_SOC, y = OR_Sensitivity, label = label_full),color = 'black',
    size = 9,
    lineheight = 0.85,
    box.padding = 2.5,         
    point.padding = 1.0,       
    min.segment.length = 0,    
    segment.size = 0.8,
    segment.color = "grey40",
    force = 60,                # 增大排斥力
    # 针对不同地类手动设置微调方向
    nudge_x = ifelse(df_main$Landuse == "Cropland", -40, -10), 
    nudge_y = ifelse(df_main$Landuse == "Cropland", -60, -20), 
    direction = "both",
    max.iter = 100000,
    show.legend = FALSE
  )

# ==========================================
# 5. 绘制子图 (p_inset)
# ==========================================
p_inset <- ggplot() +
  geom_point(data = topsoil_sandy, aes(x = SOC_yi, y = LI_yi), 
             color = "#CD8500", alpha = 0.4, size = 1.5) +
  geom_errorbarh(data = df_sandy, aes(y = OR_Sensitivity, xmin = Lower_SOC, xmax = Upper_SOC), 
                 color = "#CD8500", height = 0, linewidth = 1.2) +
  geom_errorbar(data = df_sandy, aes(x = OR_SOC, ymin = Lower_Sensitivity, ymax = Upper_Sensitivity), 
                color = "#CD8500", width = 0, linewidth = 1.2) +
  geom_point(data = df_sandy, aes(x = OR_SOC, y = OR_Sensitivity), 
             fill = "#CD8500", size = 6, shape = 21, color = "black", stroke = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_text_repel(
    data = df_sandy,
    aes(x = OR_SOC, y = OR_Sensitivity, label = label_n_only),
    size = 7.5, color = "black", 
    lineheight = 0.8,
    nudge_x = 120, nudge_y = 50,
    min.segment.length = 0,
    segment.color = "grey30"
  ) +
  coord_cartesian(xlim = c(0, 600), ylim = c(-50, 150)) + 
  labs(title = "Desert & Semi-desert") +
  theme_bw(base_size = 18) +
  theme(
    axis.title = element_blank(), 
    axis.text = element_text( size = 19, color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = "black", fill = "white", linewidth = 1)
  )

# ==========================================
# 6. 组合并输出 - 调整子图范围以防挡住 Cropland
# ==========================================
final_plot <- p_main + 
  inset_element(p_inset, 
                left = 0.62,   # 增加左侧边界，使子图更靠右
                bottom = 0.62, # 增加底部边界，使子图更靠上
                right = 0.99, 
                top = 0.99)

print(final_plot)

# 保存
ggsave("F:/wang/two_D/Topsoil_SOC_LI_Plot_v3.png", final_plot, width = 14, height = 10, dpi = 600)







































































































library(ggplot2)
library(ggpubr)

# 1. 强制设定土层顺序
raw_data$Soil.layer <- factor(raw_data$Soil.layer, levels = c("topsoil", "subsoil"))

# 2. 绘图
p <-ggplot(raw_data, aes(x = SOC_yi, y = LI_yi, color = Soil.layer)) +
  geom_point(alpha = 0.2, size = 2) +                                
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +                 
  # size = 6 放大相关系数文字
  stat_cor(method = "pearson", 
           label.y.npc = 0.9, 
           color = "black", 
           size = 6,
           p.accuracy = 0.01,
           r.accuracy = 0.01) + 
  # 使用 labeller 将首字母大写：topsoil -> Topsoil
  facet_wrap(~Soil.layer, labeller = as_labeller(c(topsoil = "Topsoil", subsoil = "Subsoil"))) +                               
  scale_color_manual(values = c("topsoil" = "#0D47A1", "subsoil" = "#4A0505")) +
  labs(title = "",
       x = "SOC (%)",
       y = "LI (%)") +
  theme_classic() + 
  theme(
    # 整体放大字体
    text = element_text(size = 16),                      # 基础字体大小
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # 标题放大居中
    axis.title = element_text(size = 16, face = "bold"), # 坐标轴标题
    axis.text = element_text(size = 14, color = "black"),# 坐标轴刻度
    strip.text = element_text(size = 16, face = "bold"), # 分面标题(Topsoil/Subsoil)
    strip.background = element_blank(),           
    legend.position = "none",                     
    axis.line = element_line(color = "black", linewidth = 0.8),    
    panel.background = element_blank()            
  )
p
# 保存图片
ggsave("F:/wang/two_D/SOCandLIcorrelation plot.png",p, width = 10, height =5, dpi = 600)


