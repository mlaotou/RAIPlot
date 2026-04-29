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

# Define NULL coalescing operator FIRST
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Initialize package environment
.RAIPlot_env <- new.env(parent = emptyenv())

configure_ai <- function(provider = "openai", api_key = NULL, api_url = NULL,
                         model = NULL, config = list()) {
  
  # Get API key from environment if not provided
  if (is.null(api_key)) {
    env_key <- Sys.getenv(paste0(toupper(provider), "_API_KEY"))
    if (env_key != "") api_key <- env_key
  }
  
  ai_config <- list(
    provider = provider,
    api_key = api_key,
    api_url = api_url,
    model = model,
    config = config
  )
  
  # Set default API URL if not provided
  if (is.null(api_url)) {
    ai_config$api_url <- get_default_api_url(provider)
  }
  
  # Don't validate custom providers immediately (they may not need API keys)
  if (tolower(provider) != "custom") {
    validate_ai_config(ai_config)
  }
  
  options(raiplot.ai_config = ai_config)
  message(paste0("AI configuration set for '", provider, "'"))
  invisible(ai_config)
}

#' @return List with current AI configuration
#' @export
get_ai_config <- function() {
  config <- getOption("raiplot.ai_config")
  
  if (is.null(config)) {
    # Return default OpenAI configuration
    config <- list(
      provider = "openai",
      api_key = Sys.getenv("OPENAI_API_KEY"),
      api_url = "https://api.openai.com/v1/chat/completions",
      model = "gpt-3.5-turbo",
      config = list()
    )
    options(raiplot.ai_config = config)
  }
  
  return(config)
}

#' Get default API URL for known providers
#'
#' @param provider Name of AI provider
#' @return API URL for the provider
#' @keywords internal
get_default_api_url <- function(provider) {
  urls <- list(
    openai = "https://api.openai.com/v1/chat/completions",
    anthropic = "https://api.anthropic.com/v1/messages",
    ollama = "http://localhost:11434/api/generate",
    localai = "http://localhost:8080/v1/chat/completions",
    llamacpp = "http://localhost:8000/v1/chat/completions",
    custom = NULL  # Custom provider has no default URL
  )
  
  return(urls[[tolower(provider)]])
}

#' Validate AI configuration
#'
#' @param config Configuration list to validate
#' @keywords internal
validate_ai_config <- function(config) {
  if (is.null(config$provider)) {
    stop("Provider must be specified")
  }
  
  if (is.null(config$model)) {
    stop("Model must be specified")
  }
  
  # Check API key for cloud providers
  cloud_providers <- c("openai", "anthropic", "cohere", "huggingface")
  if (tolower(config$provider) %in% cloud_providers) {
    if (is.null(config$api_key) || config$api_key == "") {
      stop("API key required for ", config$provider)
    }
  }
  
  invisible(TRUE)
}

#' Call custom AI API
#'
#' Make a request to custom AI API with flexible configuration
#'
#' @param prompt The prompt to send to AI
#' @param api_config Custom API configuration (uses global config if NULL)
#' @param config_override Override specific configuration options
#'
#' @return Character string with AI response
#' @keywords internal
call_custom_ai <- function(prompt, api_config = NULL, config_override = list()) {
  
  if (is.null(api_config)) {
    api_config <- get_ai_config()
  }
  
  # Merge configurations
  if (!is.list(config_override)) config_override <- list()
  api_config$config <- modifyList(api_config$config, config_override)
  
  # Route to appropriate provider handler
  provider <- tolower(api_config$provider)
  
  response <- switch(provider,
                     openai = call_openai_api(prompt, api_config),
                     anthropic = call_anthropic_api(prompt, api_config),
                     ollama = call_ollama_api(prompt, api_config),
                     localai = call_local_ai_api(prompt, api_config),
                     llamacpp = call_local_ai_api(prompt, api_config),
                     custom = call_custom_endpoint(prompt, api_config),
                     stop("Unsupported provider: ", provider)
  )
  
  return(response)
}

#' Call OpenAI API
#'
#' @keywords internal
call_openai_api <- function(prompt, api_config) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required")
  }
  
  response <- httr::POST(
    api_config$api_url,
    httr::add_headers(Authorization = paste("Bearer", api_config$api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = api_config$model,
      messages = list(
        list(role = "system", content = api_config$config$system_prompt %||% get_default_system_prompt()),
        list(role = "user", content = prompt)
      ),
      temperature = api_config$config$temperature %||% 0.3,
      max_tokens = api_config$config$max_tokens %||% 1500
    )
  )
  
  if (httr::status_code(response) != 200) {
    error_msg <- httr::content(response, as = "text")
    stop("API error (", httr::status_code(response), "): ", error_msg)
  }
  
  result <- httr::content(response, as = "parsed")
  return(result$choices[[1]]$message$content)
}

#' Call Anthropic Claude API
#'
#' @keywords internal
call_anthropic_api <- function(prompt, api_config) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required")
  }
  
  response <- httr::POST(
    api_config$api_url,
    httr::add_headers(
      "x-api-key" = api_config$api_key,
      "anthropic-version" = "2023-06-01"
    ),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = api_config$model,
      max_tokens = api_config$config$max_tokens %||% 1500,
      system = api_config$config$system_prompt %||% get_default_system_prompt(),
      messages = list(
        list(role = "user", content = prompt)
      )
    )
  )
  
  if (httr::status_code(response) != 200) {
    error_msg <- httr::content(response, as = "text")
    stop("API error (", httr::status_code(response), "): ", error_msg)
  }
  
  result <- httr::content(response, as = "parsed")
  return(result$content[[1]]$text)
}

#' Call Ollama API (local)
#'
#' @keywords internal
call_ollama_api <- function(prompt, api_config) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required")
  }
  
  response <- httr::POST(
    api_config$api_url,
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = api_config$model,
      prompt = prompt,
      stream = FALSE
    )
  )
  
  if (httr::status_code(response) != 200) {
    error_msg <- httr::content(response, as = "text")
    stop("API error (", httr::status_code(response), "): ", error_msg)
  }
  
  result <- httr::content(response, as = "parsed")
  return(result$response)
}

#' Call Local AI API (LocalAI, llama.cpp, etc.)
#'
#' @keywords internal
call_local_ai_api <- function(prompt, api_config) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required")
  }
  
  response <- httr::POST(
    api_config$api_url,
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = api_config$model,
      messages = list(
        list(role = "system", content = api_config$config$system_prompt %||% get_default_system_prompt()),
        list(role = "user", content = prompt)
      ),
      temperature = api_config$config$temperature %||% 0.3,
      max_tokens = api_config$config$max_tokens %||% 1500
    )
  )
  
  if (httr::status_code(response) != 200) {
    error_msg <- httr::content(response, as = "text")
    stop("API error (", httr::status_code(response), "): ", error_msg)
  }
  
  result <- httr::content(response, as = "parsed")
  if ("choices" %in% names(result)) {
    return(result$choices[[1]]$message$content)
  } else if ("response" %in% names(result)) {
    return(result$response)
  } else {
    stop("Unexpected API response format")
  }
}

#' Call custom generic endpoint
#'
#' @keywords internal
call_custom_endpoint <- function(prompt, api_config) {
  if (is.null(api_config$api_url)) {
    stop("api_url must be specified for custom provider")
  }
  
  # Try OpenAI-compatible format first
  response <- tryCatch({
    httr::POST(
      api_config$api_url,
      httr::add_headers(Authorization = paste("Bearer", api_config$api_key %||% "")),
      httr::content_type_json(),
      encode = "json",
      body = list(
        model = api_config$model,
        messages = list(
          list(role = "user", content = prompt)
        )
      ),
      httr::timeout(60)
    )
  }, error = function(e) {
    stop("Failed to connect to custom endpoint: ", e$message)
  })
  
  if (httr::status_code(response) == 200) {
    result <- httr::content(response, as = "parsed")
    if (!is.null(result$choices[[1]]$message$content)) {
      return(result$choices[[1]]$message$content)
    }
  }
  
  stop("Custom endpoint returned status ", httr::status_code(response))
}

#' Get default system prompt for plot generation
#'
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