#' AI Plot Generator Addin
#'
#' RStudio Addin that generates ggplot2 code using AI API
#' based on user description and data characteristics.
#'
#' @return NULL (invisibly). Opens a Shiny gadget for interaction.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Launch from RStudio Addins menu
#'   ai_plotter_addin()
#' }
ai_plotter_addin <- function() {

  # Check required packages
  required_packages <- c("shiny", "miniUI", "ggplot2", "httr", "jsonlite")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }

  # Get available data frames
  available_data <- list_available_data()

  if (length(available_data) == 0) {
    shiny::showNotification(
      "No data frames found in environment. Please load some data first.",
      type = "error"
    )
    return(invisible(NULL))
  }

  # Get current AI config
  ai_config <- tryCatch(
    {
      get_ai_config()
    },
    error = function(e) {
      list(provider = "openai", model = "gpt-3.5-turbo", api_url = NULL, api_key = NULL)
    }
  )

  # Build UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "🤖 AI Plot Generator",
      left = miniUI::miniTitleBarButton("cancel", "Cancel", primary = FALSE),
      right = miniUI::miniTitleBarButton("done", "Close", primary = TRUE)
    ),

    miniUI::miniContentPanel(
      # Layout: Left side for input, right side for code preview
      tags$div(
        style = "display: flex; height: 100%;",

        # LEFT PANEL: Input configuration
        tags$div(
          style = "flex: 0 0 45%; padding: 15px; overflow-y: auto; border-right: 1px solid #ddd;",

          # Data selection
          tags$h4("📊 Data Configuration"),
          shiny::selectInput(
            inputId = "data_select",
            label = "Select Data Frame:",
            choices = available_data,
            selected = available_data[1]
          ),
          shiny::verbatimTextOutput("data_info"),

          tags$br(),

          # Plot description
          tags$h4("✍️ Describe Your Plot"),
          shiny::textAreaInput(
            inputId = "plot_description",
            label = "What kind of plot do you want?",
            value = "Show the relationship between the main numeric columns",
            rows = 6,
            placeholder = "Examples:\n- Scatter plot of X vs Y, colored by category\n- Box plot comparing groups\n- Time series with trend line"
          ),

          tags$br(),

          # API configuration info
          tags$h4("🔑 AI Configuration"),
          tags$p(style = "font-size: 12px; color: #666;",
            paste0("Provider: ", ai_config$provider, " | Model: ", ai_config$model)
          ),
          if (!is.null(ai_config$api_url)) {
            tags$p(style = "font-size: 11px; color: #888;",
              paste0("API: ", ai_config$api_url)
            )
          },

          tags$br(),

          # Action buttons
          shiny::actionButton(
            inputId = "generate_btn",
            label = "Generate Code",
            class = "btn-primary btn-block"
          ),
          tags$br(),

          shiny::checkboxInput(
            inputId = "auto_execute",
            label = "Auto-execute and display plot",
            value = TRUE
          ),
          shiny::checkboxInput(
            inputId = "copy_to_clipboard",
            label = "Copy code to clipboard",
            value = TRUE
          )
        ),

        # RIGHT PANEL: Code preview and results
        tags$div(
          style = "flex: 1; padding: 15px; overflow-y: auto; background-color: #f5f5f5;",

          tags$h4("💻 Generated Code"),
          shiny::verbatimTextOutput("generated_code"),

          tags$br(),

          tags$h4("📝 Status / Errors"),
          shiny::verbatimTextOutput("status_log")
        )
      )
    )
  )

  # Server logic
  server <- function(input, output, session) {

    # Display data information
    output$data_info <- shiny::renderText({
      data_name <- input$data_select
      info <- get_data_info(data_name)

      if (!is.null(info)) {
        paste0(
          "Rows: ", info$nrows, " | ",
          "Columns: ", info$ncols, "\n\n",
          "Numeric: ", paste(info$numeric_cols, collapse = ", "), "\n",
          "Categorical: ", paste(info$categorical_cols, collapse = ", ")
        )
      } else {
        "Unable to load data information"
      }
    })

    # Generate code when button clicked
    shiny::observeEvent(input$generate_btn, {

      shiny::req(input$plot_description, cancelOutput = TRUE)
      shiny::req(input$data_select, cancelOutput = TRUE)

      # Show progress
      output$status_log <- shiny::renderText("⏳ Generating code... Please wait.")

      # Get AI config
      ai_config <- tryCatch(
        {
          get_ai_config()
        },
        error = function(e) {
          stop("Please configure AI first using configure_ai()")
        }
      )

      # Build prompt
      description <- if (is.null(input$plot_description) || input$plot_description == "") {
        "Create a simple scatter plot"
      } else {
        input$plot_description
      }

      prompt <- paste0(
        "Generate ggplot2 code to create a plot based on the following:\n\n",
        "Data frame name: '", input$data_select, "'\n",
        "Plot description: ", description, "\n\n",
        "Requirements:\n",
        "1. Return ONLY the R code, no explanations or markdown\n",
        "2. Use library(ggplot2) at the beginning if needed\n",
        "3. The code must be executable and work with the data frame named '",
        input$data_select, "'\n",
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

      # Call AI API based on provider
      tryCatch(
        {
          if (ai_config$provider == "custom") {
            code <- RAIPlot:::call_custom_ai(prompt, ai_config)
          } else {
            # OpenAI
            api_key <- if (is.null(ai_config$api_key) || ai_config$api_key == "") {
              Sys.getenv("OPENAI_API_KEY")
            } else {
              ai_config$api_key
            }
            if (api_key == "") {
              stop("No API key. Set with Sys.setenv(OPENAI_API_KEY='your-key')")
            }
            code <- call_openai_for_plot(
              data_name = input$data_select,
              description = input$plot_description,
              api_key = api_key,
              model = ai_config$model
            )
          }

          # Display generated code
          output$generated_code <- shiny::renderText(code)
          output$status_log <- shiny::renderText("✅ Code generated successfully!\n\nReview the code, then click 'Close' to exit.")

          # Auto-execute if checked
          if (input$auto_execute) {
            success <- safe_execute_plot(code, .GlobalEnv, verbose = FALSE)
            if (success) {
              output$status_log <- shiny::renderText(
                "✅ Code generated and executed successfully!\n\nPlot is displayed in your RStudio Plots pane."
              )
            } else {
              output$status_log <- shiny::renderText(
                "⚠️ Code generated but execution failed.\n\nCheck the code for errors and try adjusting your description."
              )
            }
          }

          # Copy to clipboard if checked
          if (input$copy_to_clipboard && requireNamespace("clipr", quietly = TRUE)) {
            tryCatch(
              {
                clipr::write_clip(code)
                output$status_log <- shiny::renderText(
                  paste0(
                    output$status_log(),
                    "\n📋 Code copied to clipboard!"
                  )
                )
              },
              error = function(e) {
                message("Could not copy to clipboard: ", e$message)
              }
            )
          }
        },
        error = function(e) {
          output$status_log <- shiny::renderText(
            paste0("❌ Error: ", e$message)
          )
          output$generated_code <- shiny::renderText("")
        }
      )
    })

    # Cancel button
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })

    # Done button
    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(NULL))
    })
  }

  # Run gadget
  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}