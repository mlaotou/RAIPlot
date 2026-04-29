#' Call OpenAI API to generate plotting code
#'
#' @param data_name Name of the data frame in the environment
#' @param description User's plot description in English or Chinese
#' @param api_key OpenAI API key
#' @param model Model to use (default: gpt-3.5-turbo)
#'
#' @return Character string containing generated R ggplot2 code
#' @export
#'
#' @examples
#' \dontrun{
#'   code <- call_openai_for_plot(
#'     data_name = "mtcars",
#'     description = "Show relationship between mpg and wt, colored by cyl",
#'     api_key = Sys.getenv("OPENAI_API_KEY")
#'   )
#'   cat(code)
#' }
call_openai_for_plot <- function(data_name, description, api_key,
                                  model = "gpt-3.5-turbo") {

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Please install it first.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it first.")
  }

  # Build the prompt
  prompt <- build_plot_prompt(data_name, description)

  # Call OpenAI API
  response <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = model,
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

  # Parse response
  if (httr::status_code(response) == 200) {
    result <- httr::content(response, as = "parsed")
    generated_code <- result$choices[[1]]$message$content
    return(generated_code)
  } else {
    error_msg <- httr::content(response, as = "text")
    stop("API call failed with status ", httr::status_code(response),
         ": ", error_msg)
  }
}

#' Build the prompt for OpenAI
#'
#' @param data_name Name of the data frame
#' @param description User's description
#'
#' @return Character string containing the complete prompt
#' @keywords internal
build_plot_prompt <- function(data_name, description) {
  paste0(
    "Generate ggplot2 code to create a plot based on the following:\n\n",
    "Data frame name: '", data_name, "'\n",
    "Plot description: ", description, "\n\n",
    "Requirements:\n",
    "1. Return ONLY the R code, no explanations or markdown\n",
    "2. Use library(ggplot2) at the beginning if needed\n",
    "3. The code must be executable and work with the data frame named '",
    data_name, "'\n",
    "4. Use theme_minimal() or another appropriate theme\n",
    "5. Add meaningful title, x label, y label\n",
    "6. Handle missing values appropriately\n",
    "7. Use a professional color palette\n",
    "8. Make the plot publication-ready\n",
    "9. Print the plot at the end with print(p) or just p\n",
    "\n",
    "The generated code should look like:\n",
    "library(ggplot2)\n",
    "p <- ggplot(...) +\n",
    "  geom_*(...) +\n",
    "  labs(...) +\n",
    "  theme_minimal()\n",
    "print(p)"
  )
}

#' Get OpenAI API key from environment
#'
#' @return Character string containing the API key
#' @keywords internal
get_openai_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (key == "") {
    stop("OPENAI_API_KEY not found in environment variables. ",
         "Please set it using Sys.setenv(OPENAI_API_KEY = 'your-key')")
  }
  return(key)
}