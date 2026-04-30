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

#' Safely execute generated R plotting code
#'
#' @param code Character string containing R code
#' @param data_env Environment containing the data (default: .GlobalEnv)
#' @param verbose If TRUE, print the code being executed
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @export
safe_execute_plot <- function(code, data_env = .GlobalEnv, verbose = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it first.")
  }
  
  tryCatch(
    {
      if (verbose) {
        message("Executing code:\n", code)
      }
      
      # 解析生成的代码
      expr <- parse(text = code)
      
      # ⚠️ 核心修复：用 suppressWarnings 屏蔽执行过程中的无害警告（比如包版本提示）
      # 这样就不会打断正常的画图流程
      result <- suppressWarnings(eval(expr, envir = data_env))
      
      # 尝试打印图像
      if (inherits(result, "gg")) {
        print(result)
      } else if (is.null(result)) {
        # 如果代码里自己带了 print(p) 就会是 null，没关系，也是成功的
      } else {
        message("Code executed successfully but did not return a ggplot object.")
      }
      
      # 一定要明确返回 TRUE
      return(TRUE)
    },
    error = function(e) {
      # 只有遇到真正的代码报错，才返回 FALSE
      message("Error executing code: ", e$message)
      return(FALSE)
    }
    # 删掉了原来那个罪魁祸首 warning = function(w) {...} 拦截器！
  )
}

#' Get information about a data frame in the environment
#'
#' @param data_name Name of the data frame as character string
#'
#' @return List containing data information or NULL if not found
#' @export
#'
#' @examples
#' get_data_info("mtcars")
get_data_info <- function(data_name) {
  tryCatch(
    {
      data <- get(data_name, envir = .GlobalEnv)
      
      if (!is.data.frame(data)) {
        message("Warning: '", data_name, "' is not a data frame.")
        return(NULL)
      }
      
      # Analyze columns
      col_types <- sapply(data, function(x) {
        if (is.numeric(x)) "numeric"
        else if (is.factor(x) || is.character(x)) "categorical"
        else class(x)[1]
      })
      
      info <- list(
        name = data_name,
        nrows = nrow(data),
        ncols = ncol(data),
        columns = names(data),
        types = col_types,
        numeric_cols = names(data)[col_types == "numeric"],
        categorical_cols = names(data)[col_types == "categorical"]
      )
      
      return(info)
    },
    error = function(e) {
      message("Error getting data info: ", e$message)
      return(NULL)
    }
  )
}

#' List all available data frames in the environment
#'
#' @return Character vector of data frame names
#' @export
#'
#' @examples
#' list_available_data()
list_available_data <- function() {
  all_objects <- ls(envir = .GlobalEnv)
  data_frames <- Filter(
    function(x) is.data.frame(get(x, envir = .GlobalEnv)),
    all_objects
  )
  return(data_frames)
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