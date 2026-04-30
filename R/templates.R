#' 1. 获取模板分类（用于 UI 菜单的分组展示）
#' @keywords internal
get_template_categories <- function() {
  list(
    "通用选项 (General)" = c(
      "None (默认自由发挥)"
    ),
    "热图系列 (Heatmaps)" = c(
      "单细胞相关性热图",
      "带聚类树的高级热图",
      "环形热图 (Circular Heatmap)"
    ),
    "分布与组学 (Omics)" = c(
      "高阶云雨图 (Raincloud Plot)",
      "PCA 3D 散点带置信椭圆",
      "Nature级复杂三元图"
    )
  )
}

#' 2. 获取模板对应的真实代码（充当你的秘籍数据库）
#' @keywords internal
get_template_code <- function(template_name) {
  
  # 这里集中存放你所有的定制化代码
  # 键名必须和上面 get_template_categories 里的名称一模一样！
  codes <- list(
    "None (默认自由发挥)" = "",
    
    "单细胞相关性热图" = "
      # 请使用 pheatmap 包绘制相关性热图
      # 去除默认的网格线，配色使用 colorRampPalette(c('navy', 'white', 'firebrick3'))(50)
    ",
    
    "高阶云雨图 (Raincloud Plot)" = "
      # 必须包含三个元素：半小提琴图、箱线图、散点抖动
      # 可以参考使用 ggdist 包的 stat_halfeye 或者 gghalves 包
      # 坐标轴翻转采用 coord_flip()
    ",
    
    "PCA 3D 散点带置信椭圆" = "
      # 这是一个 3D PCA 的伪代码逻辑，如果用户数据适合 2D 就做 2D
      # 需要用 stat_ellipse(type = 'norm', level = 0.95) 添加置信椭圆
      # 主题采用 theme_bw() 并去除 panel.grid
    "
    # ... 在这里尽情添加你的 100 个模板吧 ...
  )
  
  # 如果找不到对应的代码，返回空字符串
  if (is.null(codes[[template_name]])) return("")
  return(codes[[template_name]])
}