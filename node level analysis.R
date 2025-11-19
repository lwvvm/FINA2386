library(igraph)
library(dplyr)
library(gridExtra)
library(grid)

cat("--- 脚本开始运行 ---\n")

# --- 辅助函数：用于将长表格分页打印到PDF ---
# 这个函数是解决PDF溢出问题的关键
# 参数:
#   data_frame: 要打印的数据框
#   title: 这一系列页面的主标题
#   rows_per_page: 每页最多显示的行数
print_paginated_table <- function(data_frame, title, rows_per_page = 25) {
  # 如果数据框为空，则不执行任何操作
  if (nrow(data_frame) == 0) {
    return()
  }
  
  # 计算总页数
  num_pages <- ceiling(nrow(data_frame) / rows_per_page)
  
  # 循环创建每一页
  for (i in 1:num_pages) {
    # 计算当前页的起始行和结束行
    start_row <- (i - 1) * rows_per_page + 1
    end_row <- min(i * rows_per_page, nrow(data_frame))
    
    # 提取当前页的数据子集
    data_chunk <- data_frame[start_row:end_row, ]
    
    # 创建包含页码的标题
    page_title <- paste0(title, " (第 ", i, " 页 / 共 ", num_pages, " 页)")
    
    # 开始一个新的PDF页面
    grid.newpage()
    
    # 创建标题和表格的图形对象 (grob)
    title_grob <- textGrob(page_title, gp = gpar(fontsize = 16, fontface = "bold"))
    table_grob <- tableGrob(data_chunk, rows = NULL)
    
    # 在新页面上排列标题和表格
    grid.arrange(title_grob, table_grob, ncol = 1, 
                 heights = unit.c(unit(4, "lines"), unit(1, "npc") - unit(4, "lines")))
  }
}

# --- 1. 定义文件路径 ---
input_csv_path <- "/Users/zhoumanqi/Documents/data_set.csv" 
output_dir <- "/Users/zhoumanqi/Documents"
pdf_file_path <- file.path(output_dir, "network_analysis_results_paginated.pdf")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(paste("已创建输出目录:", output_dir, "\n"))
}

# --- 2. 加载数据并动态处理节点名 ---
cat(paste("正在从 '", input_csv_path, "' 加载数据...\n", sep=""))
edge_list_original <- read.csv(input_csv_path, header = FALSE, col.names = c("mode1", "mode2"), stringsAsFactors = FALSE)
edge_list_processed <- edge_list_original

cat("正在动态处理节点名称以确保二分图结构...\n")
edge_list_processed$mode1 <- paste("M1_", edge_list_processed$mode1, sep = "")
edge_list_processed$mode2 <- paste("M2_", edge_list_processed$mode2, sep = "")
cat("--- 节点名称处理完成。---\n\n")

# --- 3. 创建二分图并计算度中心性 ---
g_bipartite <- graph_from_data_frame(edge_list_processed, directed = FALSE)
V(g_bipartite)$type <- startsWith(V(g_bipartite)$name, "M2_")

all_degrees <- degree(g_bipartite)
nodes_mode1_v <- V(g_bipartite)[V(g_bipartite)$type == FALSE]
nodes_mode2_v <- V(g_bipartite)[V(g_bipartite)$type == TRUE]

degree_centrality_mode1 <- degree(g_bipartite, v = nodes_mode1_v) / length(nodes_mode2_v)
degree_centrality_mode2 <- degree(g_bipartite, v = nodes_mode2_v) / length(nodes_mode1_v)

results_degree <- data.frame(
  Node = V(g_bipartite)$name,
  Type = ifelse(V(g_bipartite)$type, "Mode2", "Mode1"),
  Degree = all_degrees,
  stringsAsFactors = FALSE
)
results_degree$NormalizedDegreeCentrality <- 0
results_degree$NormalizedDegreeCentrality[match(names(degree_centrality_mode1), results_degree$Node)] <- degree_centrality_mode1
results_degree$NormalizedDegreeCentrality[match(names(degree_centrality_mode2), results_degree$Node)] <- degree_centrality_mode2

cat("--- 原始二分图中心性计算完成。---\n")

# --- 4. 进行网络投影 ---
projections <- bipartite_projection(g_bipartite)
g_mode1 <- projections$proj1
g_mode2 <- projections$proj2
cat("--- 网络投影完成。---\n")

# --- 5. 准备所有分析结果以供输出 ---

# 创建一个列表来存储所有要输出到PDF的表格和标题
pdf_outputs <- list()

# **修改点**: 将Mode 1和Mode 2分开排序
# 按度(Degree)排序
bipartite_m1_by_degree <- results_degree %>% 
  filter(Type == "Mode1") %>% 
  select(Node, Degree, NormalizedDegreeCentrality) %>% 
  arrange(desc(Degree))
bipartite_m2_by_degree <- results_degree %>% 
  filter(Type == "Mode2") %>% 
  select(Node, Degree, NormalizedDegreeCentrality) %>% 
  arrange(desc(Degree))

# 按归一化度中心性(NormalizedDegreeCentrality)排序
bipartite_m1_by_norm <- results_degree %>% 
  filter(Type == "Mode1") %>% 
  select(Node, Degree, NormalizedDegreeCentrality) %>% 
  arrange(desc(NormalizedDegreeCentrality))
bipartite_m2_by_norm <- results_degree %>% 
  filter(Type == "Mode2") %>% 
  select(Node, Degree, NormalizedDegreeCentrality) %>% 
  arrange(desc(NormalizedDegreeCentrality))

# 将分开排序后的结果添加到输出列表
pdf_outputs <- append(pdf_outputs, list(
  list(title = "原始二分图 (Mode 1): 按度(Degree)降序排列", data = bipartite_m1_by_degree),
  list(title = "原始二分图 (Mode 2): 按度(Degree)降序排列", data = bipartite_m2_by_degree),
  list(title = "原始二分图 (Mode 1): 按归一化度中心性降序排列", data = bipartite_m1_by_norm),
  list(title = "原始二分图 (Mode 2): 按归一化度中心性降序排列", data = bipartite_m2_by_norm)
))

# 分析Mode 1的投影
if (vcount(g_mode1) > 0 && ecount(g_mode1) > 0) {
  results_mode1_proj <- data.frame(
    Node = V(g_mode1)$name,
    Degree = degree(g_mode1),
    Betweenness = betweenness(g_mode1, normalized = TRUE),
    Closeness = closeness(g_mode1, normalized = TRUE)
  )
  pdf_outputs <- append(pdf_outputs, list(
    list(title = "Mode 1 投影网络: 按度(Degree)降序排列", data = results_mode1_proj %>% select(Node, Degree) %>% arrange(desc(Degree))),
    list(title = "Mode 1 投影网络: 按介数中心性(Betweenness)降序排列", data = results_mode1_proj %>% select(Node, Betweenness) %>% arrange(desc(Betweenness))),
    list(title = "Mode 1 投影网络: 按接近中心性(Closeness)降序排列", data = results_mode1_proj %>% select(Node, Closeness) %>% arrange(desc(Closeness)))
  ))
  cat("--- Mode 1 投影网络分析完成。---\n")
}

# 分析Mode 2的投影
if (vcount(g_mode2) > 0 && ecount(g_mode2) > 0) {
  results_mode2_proj <- data.frame(
    Node = V(g_mode2)$name,
    Degree = degree(g_mode2),
    Betweenness = betweenness(g_mode2, normalized = TRUE),
    Closeness = closeness(g_mode2, normalized = TRUE)
  )
  pdf_outputs <- append(pdf_outputs, list(
    list(title = "Mode 2 投影网络: 按度(Degree)降序排列", data = results_mode2_proj %>% select(Node, Degree) %>% arrange(desc(Degree))),
    list(title = "Mode 2 投影网络: 按介数中心性(Betweenness)降序排列", data = results_mode2_proj %>% select(Node, Betweenness) %>% arrange(desc(Betweenness))),
    list(title = "Mode 2 投影网络: 按接近中心性(Closeness)降序排列", data = results_mode2_proj %>% select(Node, Closeness) %>% arrange(desc(Closeness)))
  ))
  cat("--- Mode 2 投影网络分析完成。---\n")
}

# --- 6. 生成可分页的PDF报告 ---
cat(paste("\n正在生成可分页的PDF文件到:", pdf_file_path, "\n"))

# 定义每页显示的行数，您可以根据需要调整这个值
ROWS_PER_PAGE <- 25

# 开启PDF设备
pdf(pdf_file_path, width = 11, height = 8.5)

# 遍历所有准备好的表格和标题
for (item in pdf_outputs) {
  # 复制数据框以避免修改原始列表中的数据
  final_data <- item$data
  
  # 清理节点名前缀
  final_data$Node <- gsub("^M1_|^M2_", "", final_data$Node)
  
  # 对数值列进行四舍五入以美化输出
  final_data <- final_data %>% mutate_if(is.numeric, round, 4)
  
  # **修改点**: 使用我们自定义的分页函数来打印表格
  print_paginated_table(final_data, item$title, rows_per_page = ROWS_PER_PAGE)
}

# 关闭PDF设备，保存文件
dev.off()

cat(paste("--- 成功！结果已保存到", pdf_file_path, "---\n"))