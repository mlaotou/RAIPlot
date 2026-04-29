#' Print RAIPlot welcome message
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Check if OpenAI API key is set
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    message(
      "RAIPlot loaded successfully!\n\n",
      "To use the AI plotting features, you need to set your OpenAI API key:\n",
      "  Sys.setenv(OPENAI_API_KEY = 'your-api-key')\n\n",
      "Or add it to your .Renviron file:\n",
      "  OPENAI_API_KEY=your-api-key\n\n",
      "Get your API key from: https://platform.openai.com/account/api-keys"
    )
  }
}

#' Print package information
#'
#' @export
raiplot_info <- function() {
  cat("RAIPlot - AI-Powered Adaptive Plot Generator\n")
  cat("Version:", as.character(packageVersion("RAIPlot")), "\n")
  cat("Repository: https://github.com/mlaotou/RAIPlot\n")
  cat("\nUsage:\n")
  cat("1. Load data into your R environment\n")
  cat("2. Open RStudio Addins menu\n")
  cat("3. Click 'AI Plot Generator'\n")
  cat("4. Describe the plot you want\n")
  cat("5. Click 'Generate Code'\n")
  cat("\nAvailable functions:\n")
  cat("  - ai_plotter_addin(): Launch the Addin\n")
  cat("  - call_openai_for_plot(): Call OpenAI API directly\n")
  cat("  - safe_execute_plot(): Execute plotting code\n")
  cat("  - get_data_info(): Get info about a data frame\n")
  cat("  - list_available_data(): List all available data frames\n")
}