# 从 GitHub 安装 RAIPlot
devtools::install_github("mlaotou/RAIPlot")
library(RAIPlot)
configure_ai(
  provider = "custom",
  api_url = "http://localhost:9015/v1/chat/completions",
  model = "minimax-m2.5-free",
  api_key = "milaotou"
)
# 加载数据
data(mtcars)
data(iris)


# 测试获取配置
config <- get_ai_config()
print(config)

# 手动测试 API
library(httr)
response <- POST(
  config$api_url,
  add_headers(Authorization = paste("Bearer", config$api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = config$model,
    messages = list(
      list(role = "user", content = "Hello")
    )
  )
)
httr::status_code(response)
content <- httr::content(response, as = "parsed")
content$choices[[1]]$message$content
# ============
# rm(list=ls())  # 清除环境变量  
# # 设置工作路径
library(this.path);setwd(dirname(this.path()));getwd()
# 
devtools::load_all()
ai_plotter_addin()
