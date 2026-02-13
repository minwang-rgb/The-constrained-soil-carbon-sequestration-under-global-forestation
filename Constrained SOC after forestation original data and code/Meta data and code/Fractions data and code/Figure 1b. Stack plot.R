library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
library(forcats)
library(RColorBrewer)

setwd("/Users/min/Desktop/NEE/")

# 1. 读取与初步数据清洗
data <- read_csv("2025S1223KOoutli.csv") %>%
  clean_names()

# 2. 预处理数据
plot_data_full <- data %>%
  filter(fractions != "SOC" & 
           !is.na(method) & method != "" & 
           !is.na(extractions) & extractions != "" &
           !grepl("^others$", fractions, ignore.case = TRUE)) %>%
  group_by(method, extractions) %>%
  summarise(n = sum(n()), .groups = 'drop')

# --- 核心逻辑：从小到大排列 (升序) ---
p1_data <- plot_data_full %>%
  # 计算每个 method 的总量，用于全局排序
  group_by(method) %>%
  mutate(method_total = sum(n)) %>%
  ungroup() %>%
  # 修改点：去掉 desc()，改为默认的升序排列
  # 1. method_total: 总量小的 Method 排在左侧
  # 2. n: 同一个 Method 内部数量小的 Extraction 排在左侧
  arrange(method_total, n) %>%
  # 锁定当前的升序排列顺序
  mutate(group_id = fct_inorder(paste0(method, " - ", extractions)))

# --- 颜色生成逻辑：同一 Method 使用相近色系 ---
methods_ordered <- unique(p1_data$method)
n_methods <- length(methods_ordered)

# 基础色板
base_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#73448C", "#882255")



if(n_methods > length(base_palette)) {
  base_colors <- colorRampPalette(base_palette)(n_methods)
} else {
  base_colors <- base_palette[1:n_methods]
}

custom_colors <- setNames(character(), character())

for(i in seq_along(methods_ordered)) {
  m <- methods_ordered[i]
  m_data <- p1_data %>% filter(method == m)
  n_ext <- nrow(m_data)
  
  # 生成渐变色：从小到大，颜色由浅入深（或者保持一致）
  # 这里调整为从浅色到主色的渐变，以符合从小到大的视觉感
  ext_colors <- rev(colorRampPalette(c(base_colors[i], "#F8F9FA"))(n_ext + 1)[1:n_ext])
  names(ext_colors) <- m_data$group_id
  custom_colors <- c(custom_colors, ext_colors)
}

# --- 绘图 A: 概览条形图 (p1) --- 
p1 <- ggplot(p1_data, aes(y = "Total", x = n, fill = group_id)) + 
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.4, width = 0.5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "", 
       x = "Sample Count",
       y = "",
       fill = "METHOD - EXTRACTION") + 
  theme_classic() +
  theme(
    axis.line = element_line(color = "#2B2D42", linewidth = 1.2),
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 28, face = "bold", color = "#2B2D42"),
    axis.title = element_text(size = 28, face = "bold", color = "#2B2D42"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "#2B2D42"),
    legend.title = element_text(size = 28, face = "bold"),
    legend.text = element_text(size = 28),
    legend.position = "bottom",
    legend.ncol = 3
  )

# --- 绘图 B: 饼图 (p2) 保持不变 ---
p2 <- ggplot(pie_counts <- plot_data_full %>% 
               group_by(extractions) %>% 
               summarise(n = sum(n)) %>% 
               mutate(label = paste0(extractions, "\n", round(n/sum(n)*100, 1), "%")), 
             aes(x = "", y = n, fill = extractions)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = colorRampPalette(base_palette)(nrow(pie_counts))) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 8, fontface = "bold", color = "white") + 
  labs(title = "", fill = "EXTRACTIONS") +
  theme_void() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "#2B2D42"),
    legend.position = "bottom"
  )

# 保存与打印
dir.create("/Users/min/Desktop/NEE/fractions/", showWarnings = FALSE)
print(p1)
print(p2)

ggsave(filename = "/Users/min/Desktop/NEE/fractions/total_stackbar.png", 
       plot = p1, width = 20, height = 6, bg = "white", dpi = 300)
ggsave(filename = "/Users/min/Desktop/NEE/fractions/pie.png", 
       plot = p2, width = 8, height = 8, bg = "white", dpi = 300)



