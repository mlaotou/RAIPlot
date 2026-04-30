# ==========================================
# PART 1: 核心配置与状态管理
# ==========================================

# 1. 定义包级独立环境（替代原本混乱的 options 和多重环境）
.RAIPlot_env <- new.env(parent = emptyenv())

# 2. 定义 NULL 合并操作符（非常实用）
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' 配置 AI 接口
#' @export
configure_ai <- function(provider = "openai", 
                         api_key = NULL, 
                         api_url = NULL,
                         model = NULL, 
                         config = list()) {
  
  provider <- tolower(provider)
  
  # 如果没有提供 API key，尝试从系统环境变量获取
  if (is.null(api_key)) {
    env_key <- Sys.getenv(paste0(toupper(provider), "_API_KEY"))
    if (env_key != "") api_key <- env_key
  }
  
  # 构建基础配置对象
  ai_config <- list(
    provider = provider,
    api_key = api_key,
    api_url = api_url %||% get_default_api_url(provider),
    model = model,
    config = config
  )
  
  # 存入插件的独立环境
  assign("raiplot_ai_config", ai_config, envir = .RAIPlot_env)
  message(sprintf("✅ AI 配置已更新: Provider = '%s', Model = '%s'", 
                  provider, model %||% "default"))
  
  invisible(ai_config)
}

#' 获取当前 AI 配置（带安全兜底机制）
#' @export
get_ai_config <- function() {
  # 坚不可摧的默认配置字典
  default_config <- list(
    provider = "openai",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    api_url = "https://api.openai.com/v1/chat/completions",
    model = "gpt-3.5-turbo",
    config = list(temperature = 0.3, max_tokens = 1500)
  )
  
  # 如果用户从未配置过，直接返回默认
  if (!exists("raiplot_ai_config", envir = .RAIPlot_env)) {
    return(default_config)
  }
  
  # 获取用户配置
  user_config <- get("raiplot_ai_config", envir = .RAIPlot_env)
  
  # 核心修复点：将用户配置覆盖到默认配置上。
  # 这样即使用户漏填了某个参数（比如 API URL），也会被默认值自动补齐
  merged_config <- utils::modifyList(default_config, user_config)
  
  return(merged_config)
}

#' 内部辅助函数：获取默认 URL
#' @keywords internal
get_default_api_url <- function(provider) {
  urls <- list(
    openai = "https://api.openai.com/v1/chat/completions",
    anthropic = "https://api.anthropic.com/v1/messages",
    ollama = "http://localhost:11434/api/generate",
    localai = "http://localhost:8080/v1/chat/completions",
    llamacpp = "http://localhost:8000/v1/chat/completions",
    custom = NULL
  )
  return(urls[[provider]])
}

#' 内部辅助函数：系统 Prompt
#' @keywords internal
get_default_system_prompt <- function() {
  paste(
    "You are an expert R data visualization specialist.",
    "You create beautiful, clean, and effective ggplot2 visualizations.",
    "Always follow best practices for data visualization.",
    "Your code must be syntactically correct and executable.",
    sep = " "
  )
}

# ==========================================
# PART 2: 路由分发器
# ==========================================

#' 通用 API 调用入口
#' @export
call_custom_ai <- function(prompt, api_config = NULL, config_override = list()) {
  
  # 1. 获取配置（自动享受 get_ai_config 的兜底保护）
  if (is.null(api_config)) {
    api_config <- get_ai_config()
  }
  
  # 2. 合并临时覆盖的参数
  if (!is.list(config_override)) config_override <- list()
  if (length(config_override) > 0) {
    api_config$config <- utils::modifyList(api_config$config, config_override)
  }
  
  # 3. 终极防御：如果由于不可抗力 provider 依然为空，抛出清晰的错误而不是崩溃
  provider <- api_config$provider
  if (is.null(provider) || length(provider) == 0) {
    stop("❌ 错误: AI Provider 未指定或为空。请先运行 configure_ai()。")
  }
  
  # 4. 根据 Provider 路由到具体的处理函数
  response <- switch(provider,
                     openai = call_openai_api(prompt, api_config),
                     anthropic = call_anthropic_api(prompt, api_config),
                     ollama = call_ollama_api(prompt, api_config),
                     localai = call_local_ai_api(prompt, api_config),
                     llamacpp = call_local_ai_api(prompt, api_config),
                     custom = call_custom_endpoint(prompt, api_config),
                     stop("❌ 不支持的 AI 提供商: ", provider)
  )
  
  return(response)
}

# ==========================================
# PART 3: 具体 API 请求实现
# ==========================================

#' 调用 OpenAI 兼容接口
#' @keywords internal
call_openai_api <- function(prompt, api_config) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("需要安装 'httr' 包")
  
  # 强制检查 API Key
  if (is.null(api_config$api_key) || api_config$api_key == "") {
    stop("❌ OpenAI 需要 API Key，请通过 Sys.setenv(OPENAI_API_KEY='...') 或 configure_ai() 设置。")
  }
  
  sys_prompt <- api_config$config$system_prompt %||% get_default_system_prompt()
  
  response <- httr::POST(
    url = api_config$api_url,
    httr::add_headers(Authorization = paste("Bearer", api_config$api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = api_config$model,
      messages = list(
        list(role = "system", content = sys_prompt),
        list(role = "user", content = prompt)
      ),
      temperature = api_config$config$temperature %||% 0.3,
      max_tokens = api_config$config$max_tokens %||% 1500
    )
  )
  
  if (httr::status_code(response) != 200) {
    error_msg <- httr::content(response, as = "text")
    stop("API 错误 (HTTP ", httr::status_code(response), "): ", error_msg)
  }
  
  result <- httr::content(response, as = "parsed")
  return(result$choices[[1]]$message$content)
}

#' 调用完全自定义的本地大模型接口 (例如 LM Studio, vLLM 等)
#' @keywords internal
call_custom_endpoint <- function(prompt, api_config) {
  if (is.null(api_config$api_url) || api_config$api_url == "") {
    stop("❌ Provider 设置为 'custom' 时，必须提供 api_url。")
  }
  
  # 构建 Header（如果有 API key 则带上，无则不带）
  req_headers <- httr::content_type_json()
  if (!is.null(api_config$api_key) && api_config$api_key != "") {
    req_headers <- c(req_headers, httr::add_headers(Authorization = paste("Bearer", api_config$api_key)))
  }
  
  response <- tryCatch({
    httr::POST(
      url = api_config$api_url,
      req_headers,
      encode = "json",
      body = list(
        model = api_config$model,
        messages = list(
          list(role = "system", content = get_default_system_prompt()),
          list(role = "user", content = prompt)
        ),
        temperature = api_config$config$temperature %||% 0.3
      ),
      httr::timeout(60) # 设置合理的超时防卡死
    )
  }, error = function(e) {
    stop("❌ 无法连接到自定义节点: ", e$message)
  })
  
  if (httr::status_code(response) == 200) {
    result <- httr::content(response, as = "parsed")
    # 尝试兼容标准 OpenAI 格式返回体
    if (!is.null(result$choices[[1]]$message$content)) {
      return(result$choices[[1]]$message$content)
    }
  }
  
  stop("自定义节点返回错误或无法解析的内容，状态码: ", httr::status_code(response))
}

# 注：如果你还需要 Ollama 或 Anthropic 的接口，可以仿照上面的格式继续添加 
# call_ollama_api 和 call_anthropic_api