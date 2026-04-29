#' Safely execute generated R plotting code
#'
#' @param code Character string containing R code
#' @param data_env Environment containing the data (default: .GlobalEnv)
#' @param verbose If TRUE, print the code being executed
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#'   code <- "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
#'   safe_execute_plot(code)
#' }
safe_execute_plot <- function(code, data_env = .GlobalEnv, verbose = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it first.")
  }

  tryCatch(
    {
      if (verbose) {
        message("Executing code:\n", code)
      }

      # Parse and evaluate the code
      expr <- parse(text = code)
      result <- eval(expr, envir = data_env)

      # If result is a ggplot object, display it
      if (inherits(result, "gg")) {
        print(result)
        return(TRUE)
      } else if (is.null(result)) {
        # Code was executed but didn't return a plot
        # (likely used print() inside)
        return(TRUE)
      } else {
        message("Code executed successfully but did not return a ggplot object.")
        return(TRUE)
      }
    },
    error = function(e) {
      message("Error executing code: ", e$message)
      return(FALSE)
    },
    warning = function(w) {
      message("Warning during execution: ", w$message)
    }
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

#' Validate R plotting code syntax
#'
#' @param code Character string containing R code
#'
#' @return Logical TRUE if valid, FALSE otherwise
#' @keywords internal
validate_code_syntax <- function(code) {
  tryCatch(
    {
      parse(text = code)
      return(TRUE)
    },
    error = function(e) {
      message("Syntax error in code: ", e$message)
      return(FALSE)
    }
  )
}