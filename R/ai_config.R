#' AI Configuration Manager
#'
#' Manage custom AI API configurations for different AI providers
#'
#' @param provider Name of AI provider (e.g., "openai", "custom")
#' @param api_key API key for the provider
#' @param api_url API endpoint URL
#' @param model Model name to use
#' @param config List of additional configuration parameters
#'
#' @return List with configuration or invisibly stores configuration
#' @export
#'
#' @examples
#' \dontrun{
#'   # Configure OpenAI (default)
#'   configure_ai(
#'     provider = "openai",
#'     api_key = "sk-your-key",
#'     model = "gpt-3.5-turbo"
#'   )
#'
#'   # Configure custom provider (e.g., Ollama, local LLM)
#'   configure_ai(
#'     provider = "ollama",
#'     api_url = "http://localhost:11434/api/generate",
#'     model = "llama2",
#'     config = list(temperature = 0.3)
#'   )
#'
#'   # Configure Claude (Anthropic)
#'   configure_ai(
#'     provider = "anthropic",
#'     api_key = "sk-ant-your-key",
#'     model = "claude-3-opus"
#'   )
#' }
nconfigure_ai <- function(provider = "openai", api_key = NULL, api_url = NULL,
                          model = NULL, config = list()) {
                          ai_config <- list(
                            provider = provider,
                            api_key = api_key %||% Sys.getenv(paste0(toupper(provider), \"_API_KEY\")),
                            api_url = api_url,
                            model = model,
config = config\
)
if (is.null(api_url)) {
ai_config$api_url <- get_default_api_url(provider)
}
validate_ai_config(ai_config)
options(raiplot.ai_config = ai_config)
message(\"AI configuration set for '\", provider, \"'\")
invisible(ai_config)
}
@return List with current AI configuration\n#' @export\nget_ai_config <- function() {
config <- getOption(\"raiplot.ai_config\")\n\n  if (is.null(config)) {\n    # Return default OpenAI configuration\n    config <- list(\n      provider = \"openai\",\n      api_key = Sys.getenv(\"OPENAI_API_KEY\"),\n      api_url = \"https://api.openai.com/v1/chat/completions\",\n      model = \"gpt-3.5-turbo\",\n      config = list()\n    )\n    options(raiplot.ai_config = config)\n  }\n\n  return(config)\n}\n\n#' Get default API URL for known providers\n#'\n#' @param provider Name of AI provider\n#' @return API URL for the provider\n#' @keywords internal\nget_default_api_url <- function(provider) {\n  urls <- list(\n    openai = \"https://api.openai.com/v1/chat/completions\",\n    anthropic = \"https://api.anthropic.com/v1/messages\",\n    ollama = \"http://localhost:11434/api/generate\",\n    localai = \"http://localhost:8080/v1/chat/completions\",\n    llamacpp = \"http://localhost:8000/v1/chat/completions\"\n  )\n\n  return(urls[[tolower(provider)]])\n}\n\n#' Validate AI configuration\n#'\n#' @param config Configuration list to validate\n#' @keywords internal\nvalidate_ai_config <- function(config) {\n  if (is.null(config$provider)) {\n    stop(\"Provider must be specified\")\n  }\n\n  if (is.null(config$model)) {\n    stop(\"Model must be specified\")\n  }\n\n  # Check API key for cloud providers\n  cloud_providers <- c(\"openai\", \"anthropic\", \"cohere\", \"huggingface\")\n  if (tolower(config$provider) %in% cloud_providers && is.null(config$api_key)) {\n    stop(\"API key required for \", config$provider)\n  }\n\n  invisible(TRUE)\n}\n\n#' Call custom AI API\n#'\n#' Make a request to custom AI API with flexible configuration\n#'\n#' @param prompt The prompt to send to AI\n#' @param api_config Custom API configuration (uses global config if NULL)\n#' @param config_override Override specific configuration options\n#'\n#' @return Character string with AI response\n#' @keywords internal\ncall_custom_ai <- function(prompt, api_config = NULL, config_override = list()) {\n\n  if (is.null(api_config)) {\n    api_config <- get_ai_config()\n  }\n\n  # Merge configurations\n  api_config$config <- modifyList(api_config$config, config_override)\n\n  # Route to appropriate provider handler\n  provider <- tolower(api_config$provider)\n\n  response <- switch(provider,\n    openai = call_openai_api(prompt, api_config),\n    anthropic = call_anthropic_api(prompt, api_config),\n    ollama = call_ollama_api(prompt, api_config),\n    localai = call_local_ai_api(prompt, api_config),\n    llamacpp = call_local_ai_api(prompt, api_config),\n    stop(\"Unsupported provider: \", provider)\n  )\n\n  return(response)\n}\n\n#' Call OpenAI API\n#'\n#' @keywords internal\ncall_openai_api <- function(prompt, api_config) {\n  if (!requireNamespace(\"httr\", quietly = TRUE)) {\n    stop(\"Package 'httr' is required\")\n  }\n\n  response <- httr::POST(\n    api_config$api_url,\n    httr::add_headers(Authorization = paste(\"Bearer\", api_config$api_key)),\n    httr::content_type_json(),\n    encode = \"json\",\n    body = list(\n      model = api_config$model,\n      messages = list(\n        list(role = \"system\", content = api_config$config$system_prompt %||% get_default_system_prompt()),\n        list(role = \"user\", content = prompt)\n      ),\n      temperature = api_config$config$temperature %||% 0.3,\n      max_tokens = api_config$config$max_tokens %||% 1500\n    )\n  )\n\n  if (httr::status_code(response) != 200) {\n    error_msg <- httr::content(response, as = \"text\")\n    stop(\"API error (\", httr::status_code(response), \"): \", error_msg)\n  }\n\n  result <- httr::content(response, as = \"parsed\")\n  return(result$choices[[1]]$message$content)\n}\n\n#' Call Anthropic Claude API\n#'\n#' @keywords internal\ncall_anthropic_api <- function(prompt, api_config) {\n  if (!requireNamespace(\"httr\", quietly = TRUE)) {\n    stop(\"Package 'httr' is required\")\n  }\n\n  response <- httr::POST(\n    api_config$api_url,\n    httr::add_headers(\n      \"x-api-key\" = api_config$api_key,\n      \"anthropic-version\" = \"2023-06-01\"\n    ),\n    httr::content_type_json(),\n    encode = \"json\",\n    body = list(\n      model = api_config$model,\n      max_tokens = api_config$config$max_tokens %||% 1500,\n      system = api_config$config$system_prompt %||% get_default_system_prompt(),\n      messages = list(\n        list(role = \"user\", content = prompt)\n      )\n    )\n  )\n\n  if (httr::status_code(response) != 200) {\n    error_msg <- httr::content(response, as = \"text\")\n    stop(\"API error (\", httr::status_code(response), \"): \", error_msg)\n  }\n\n  result <- httr::content(response, as = \"parsed\")\n  return(result$content[[1]]$text)\n}\n\n#' Call Ollama API (local)\n#'\n#' @keywords internal\ncall_ollama_api <- function(prompt, api_config) {\n  if (!requireNamespace(\"httr\", quietly = TRUE)) {\n    stop(\"Package 'httr' is required\")\n  }\n\n  response <- httr::POST(\n    api_config$api_url,\n    httr::content_type_json(),\n    encode = \"json\",\n    body = list(\n      model = api_config$model,\n      prompt = prompt,\n      stream = FALSE\n    )\n  )\n\n  if (httr::status_code(response) != 200) {\n    error_msg <- httr::content(response, as = \"text\")\n    stop(\"API error (\", httr::status_code(response), \"): \", error_msg)\n  }\n\n  result <- httr::content(response, as = \"parsed\")\n  return(result$response)\n}\n\n#' Call Local AI API (LocalAI, llama.cpp, etc.)\n#'\n#' @keywords internal\ncall_local_ai_api <- function(prompt, api_config) {\n  if (!requireNamespace(\"httr\", quietly = TRUE)) {\n    stop(\"Package 'httr' is required\")\n  }\n\n  response <- httr::POST(\n    api_config$api_url,\n    httr::content_type_json(),\n    encode = \"json\",\n    body = list(\n      model = api_config$model,\n      messages = list(\n        list(role = \"system\", content = api_config$config$system_prompt %||% get_default_system_prompt()),\n        list(role = \"user\", content = prompt)\n      ),\n      temperature = api_config$config$temperature %||% 0.3,\n      max_tokens = api_config$config$max_tokens %||% 1500\n    )\n  )\n\n  if (httr::status_code(response) != 200) {\n    error_msg <- httr::content(response, as = \"text\")\n    stop(\"API error (\", httr::status_code(response), \"): \", error_msg)\n  }\n\n  result <- httr::content(response, as = \"parsed\")\n  if (\"choices\" %in% names(result)) {\n    return(result$choices[[1]]$message$content)\n  } else if (\"response\" %in% names(result)) {\n    return(result$response)\n  } else {\n    stop(\"Unexpected API response format\")\n  }\n}\n\n#' Get default system prompt for plot generation\n#'\n#' @keywords internal\nget_default_system_prompt <- function() {\n  paste(\n    \"You are an expert R data visualization specialist.\",\n    \"You create beautiful, clean, and effective ggplot2 visualizations.\",\n    \"Always follow best practices for data visualization.\",\n    \"Your code must be syntactically correct and executable.\",\n    sep = \" \"\n  )\n}\n\n#' Utility: NULL coalescing operator\n#'\n#' @keywords internal\n`%||%` <- function(x, y) {\n  if (is.null(x)) y else x\n}"
