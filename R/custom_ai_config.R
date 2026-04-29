#' Custom AI Configuration for RAIPlot
#'
#' Configure RAIPlot to use custom AI models and endpoints
#'
#' @param provider Character string specifying the AI provider
#'   Options: "openai", "custom"
#' @param api_key API key for authentication
#' @param api_url Custom API endpoint URL (for custom providers)
#' @param model Model name to use
#'
#' @return List containing the configuration (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Use OpenAI (default)
#'   configure_ai(
#'     provider = "openai",
#'     api_key = Sys.getenv("OPENAI_API_KEY"),
#'     model = "gpt-4"
#'   )
#'
#'   # Use custom LLM endpoint (e.g., local Ollama, Azure, LM Studio)
#'   configure_ai(
#'     provider = "custom",
#'     api_url = "http://localhost:11434/api/generate",
#'     model = "llama2",
#'     api_key = ""  # Not needed for local services
#'   )
#' }
configure_ai <- function(provider = "openai",
                         api_key = NULL,
                         api_url = NULL,
                         model = "gpt-3.5-turbo") {

  # Validate provider
  valid_providers <- c("openai", "custom")
  if (!provider %in% valid_providers) {
    stop("Provider must be one of: ", paste(valid_providers, collapse = ", "))
  }

  # Set up configuration based on provider
  config <- list(
    provider = provider,
    model = model,
    api_key = api_key,
    api_url = api_url
  )

  # Store in package environment
  assign("raiplot_ai_config", config, envir = .RAIPlot_env)

  message("RAIPlot AI configuration updated:")
  message("  Provider: ", provider)
  message("  Model: ", model)
  if (!is.null(api_url)) {
    message("  API URL: ", api_url)
  }

  invisible(config)
}

#' Get current AI configuration
#'
#' @return List containing the current AI configuration
#' @export
#'
#' @examples
#' \dontrun{
#'   config <- get_ai_config()
#'   print(config)
#' }
get_ai_config <- function() {
  if (!exists("raiplot_ai_config", envir = .RAIPlot_env)) {
    # Return default OpenAI config
    return(list(
      provider = "openai",
      model = "gpt-3.5-turbo",
      api_key = Sys.getenv("OPENAI_API_KEY"),
      api_url = "https://api.openai.com/v1/chat/completions"
    ))
  }

  get("raiplot_ai_config", envir = .RAIPlot_env)
}

#' Reset AI configuration to default (OpenAI)
#'
#' @export
reset_ai_config <- function() {
  configure_ai(
    provider = "openai",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    model = "gpt-3.5-turbo"
  )
  message("AI configuration reset to default (OpenAI)")
}

#' Call custom AI provider
#'
#' @param prompt The prompt to send to the AI
#' @param config AI configuration (uses get_ai_config() if NULL)
#'
#' @return Character string containing the AI response
#' @keywords internal
call_custom_ai <- function(prompt, config = NULL) {

  if (is.null(config)) {
    config <- get_ai_config()
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required.")
  }

  if (config$provider == "openai") {
    return(call_openai_api(prompt, config))
  } else if (config$provider == "custom") {
    return(call_custom_endpoint(prompt, config))
  } else {
    stop("Unknown provider: ", config$provider)
  }
}

#' Call OpenAI API
#'
#' @param prompt The prompt
#' @param config Configuration list
#'
#' @return Generated code
#' @keywords internal
call_openai_api <- function(prompt, config) {

  response <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", config$api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = config$model,
      messages = list(
        list(
          role = "system",
          content = paste(
            "You are an expert R data visualization specialist.",
            "You create beautiful, clean, and effective ggplot2 visualizations.",
            "Always follow best practices for data visualization.",
            "Your code must be syntactically correct and executable."
          )
        ),
        list(role = "user", content = prompt)
      ),
      temperature = 0.3,
      max_tokens = 1500
    )
  )

  if (httr::status_code(response) == 200) {
    result <- httr::content(response, as = "parsed")
    return(result$choices[[1]]$message$content)
  } else {
    error_msg <- httr::content(response, as = "text")
    stop("OpenAI API failed: ", httr::status_code(response), " - ", error_msg)
  }
}

#' Call custom AI endpoint
#'
#' @param prompt The prompt
#' @param config Configuration list
#'
#' @return Generated code
#' @keywords internal
call_custom_endpoint <- function(prompt, config) {

  if (is.null(config$api_url)) {
    stop("api_url must be specified for custom provider")
  }

  # Build request headers
  headers <- httr::add_headers(`Content-Type` = "application/json")

  if (!is.null(config$api_key) && config$api_key != "") {
    headers <- httr::add_headers(
      headers,
      Authorization = paste("Bearer", config$api_key)
    )
  }

  # Try different custom endpoint formats
  # Format 1: OpenAI-compatible endpoint
  tryCatch(
    {
      response <- httr::POST(
        config$api_url,
        headers,
        encode = "json",
        body = list(
          model = config$model,
          messages = list(
            list(role = "system", content = "You are an R visualization expert."),
            list(role = "user", content = prompt)
          ),
          temperature = 0.3,
          max_tokens = 1500
        ),
        timeout(60)
      )

      if (httr::status_code(response) == 200) {
        result <- httr::content(response, as = "parsed")
        if (!is.null(result$choices[[1]]$message$content)) {
          return(result$choices[[1]]$message$content)
        }
      }
    },
    error = function(e) {
      message("Note: ", e$message)
    }
  )

  # Format 2: Ollama-style endpoint
  tryCatch(
    {
      response <- httr::POST(
        config$api_url,
        headers,
        encode = "json",
        body = list(
          model = config$model,
          prompt = prompt,
          stream = FALSE
        ),
        timeout(60)
      )

      if (httr::status_code(response) == 200) {
        result <- httr::content(response, as = "parsed")
        if (!is.null(result$response)) {
          return(result$response)
        }
      }
    },
    error = function(e) {
      message("Note: ", e$message)
    }
  )

  stop("Could not communicate with custom AI endpoint at: ", config$api_url)
}
