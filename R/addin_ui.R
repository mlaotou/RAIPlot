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
  
  # ✨ 修复报错的核心：明确告诉包环境，tags 到底是谁的
  tags <- shiny::tags
  
  # Get available data frames
  available_data <- list_available_data()
  
  if (length(available_data) == 0) {
    shiny::showNotification(
      "No data frames found in environment. Please load some data first.",
      type = "error"
    )
    return(invisible(NULL))
  }
  
  # Get current AI config safely (现在这里绝对安全，自带默认兜底)
  ai_config <- get_ai_config()
  
  # Build UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "🤖 AI Plot Generator",
      left = miniUI::miniTitleBarButton("cancel", "Cancel", primary = FALSE),
      right = miniUI::miniTitleBarButton("done", "Close", primary = TRUE)
    ),
    
    miniUI::miniContentPanel(
      tags$div(
        style = "display: flex; height: 100%;",
        
        # LEFT PANEL: Input configuration
        tags$div(
          style = "flex: 0 0 45%; padding: 15px; overflow-y: auto; border-right: 1px solid #ddd;",
          
          tags$h4("📊 Data Configuration"),
          shiny::selectInput(
            inputId = "data_select",
            label = "Select Data Frame(s):",
            choices = available_data,
            selected = available_data[1],
            multiple = TRUE,        # ✨ 关键1：开启多选！
            selectize = TRUE        # 开启友好的搜索框
          ),
          shiny::verbatimTextOutput("data_info"),
          
          tags$br(),
          
          tags$h4("✨ Plot Template"),
          shiny::selectInput(
            inputId = "plot_template",
            label = "Select a reference style:",
            choices = get_template_categories(),  # ✨ 关键2：传入嵌套列表，自动生成分组！
            selected = "None (默认自由发挥)"
          ),
          
          tags$br(),
          
          tags$h4("✍️ Describe Your Plot"),
          shiny::textAreaInput(
            inputId = "plot_description",
            label = "What kind of plot do you want?",
            value = "Show the relationship between the main numeric columns",
            rows = 6,
            placeholder = "Examples:\n- Scatter plot of X vs Y, colored by category\n- Box plot comparing groups\n- Time series with trend line"
          ),
          
          tags$br(),
          
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
          
          # ✨ 新增：直接在插件里预览图片的画板！
          tags$h4("📈 Plot Preview"),
          shiny::plotOutput("plot_preview", height = "350px"),
          
          tags$br(),
          
          tags$h4("📝 Status / Errors"),
          shiny::verbatimTextOutput("status_log")
        )
      )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    output$data_info <- shiny::renderText({
      data_names <- input$data_select
      if (is.null(data_names) || length(data_names) == 0) return("Please select at least one data frame.")
      
      # 遍历获取所有选中的数据集信息
      info_strings <- lapply(data_names, function(d_name) {
        info <- get_data_info(d_name)
        if (!is.null(info)) {
          # 如果列太多，只展示前 5 个，防止界面爆炸
          num_cols <- if(length(info$numeric_cols) > 5) paste0(paste(info$numeric_cols[1:5], collapse = ", "), "...") else paste(info$numeric_cols, collapse = ", ")
          cat_cols <- if(length(info$categorical_cols) > 5) paste0(paste(info$categorical_cols[1:5], collapse = ", "), "...") else paste(info$categorical_cols, collapse = ", ")
          
          paste0(
            "📦 [", d_name, "] (", info$nrows, " rows, ", info$ncols, " cols)\n",
            "   Numeric: ", num_cols, "\n",
            "   Categorical: ", cat_cols
          )
        } else {
          paste0("📦 [", d_name, "] Unable to load.")
        }
      })
      
      # 把多个数据集的信息拼在一起
      paste(info_strings, collapse = "\n\n")
    })
    
    shiny::observeEvent(input$generate_btn, {
      
      shiny::req(input$plot_description, cancelOutput = TRUE)
      shiny::req(input$data_select, cancelOutput = TRUE)
      
      output$status_log <- shiny::renderText("⏳ Generating code... Please wait.")
      
      description <- if (is.null(input$plot_description) || input$plot_description == "") {
        "Create a simple scatter plot"
      } else {
        input$plot_description
      }
      
      # ==========================================
      # 准备开始构建 Prompt
      # ==========================================
      
      # ✨ 魔法1：获取私家模板代码 (注意：这是在准备变量，不是在 paste0 里面)
      selected_template_code <- get_template_code(input$plot_template)
      reference_instruction <- ""
      
      if (selected_template_code != "") {
        reference_instruction <- paste0(
          "\n--- CRITICAL REQUIREMENT: REFERENCE STYLE ---\n",
          "You MUST use the following custom code logic, packages, and themes as the blueprint for your plot:\n",
          "```R\n", selected_template_code, "\n```\n",
          "Adapt the variable names in this template to match the user's data frames.\n--------------------------------------------\n\n"
        )
      }
      
      # ✨ 魔法2：把多数据集拼接成 "df1", "df2" 的形式
      df_names_str <- paste(input$data_select, collapse = "', '")
      
      # 🏆 终极版 Prompt：利用上面准备好的变量，正式拼接字符串！
      prompt <- paste0(
        "Generate R plotting code based on the following request:\n\n",
        "Available Data Frames: '", df_names_str, "'\n",
        "User Request: ", description, "\n\n",
        
        reference_instruction, # 插入你的秘籍
        
        "Requirements:\n",
        "1. Return ONLY the R code, no explanations or markdown.\n",
        "2. Use library(ggplot2) and other necessary packages at the beginning.\n",
        "3. The code must be executable and work with the provided data frames: '", df_names_str, "'. Merge them if necessary.\n",
        "4. Use theme_minimal() or another appropriate clean theme.\n",
        "5. Add meaningful title, x label, and y label.\n",
        "6. Handle missing values appropriately.\n",
        "7. Use a professional color palette (e.g., viridis, RColorBrewer, ggsci).\n",
        "8. Make the plot publication-ready.\n",
        "9. Print the plot at the end with print(p) or just p.\n",
        "10. DO NOT output any cleanup code, DO NOT use rm(), and absolutely DO NOT use detach().\n",
        "\n",
        "The generated code should look like:\n",
        "library(ggplot2)\n",
        "p <- ggplot(...) +\n",
        "  geom_*(...) +\n",
        "  labs(...) +\n",
        "  theme_minimal()\n",
        "print(p)"
      )
      
      # ==========================================
      # 核心魔法：所有繁杂的 API 判断全部浓缩为这一句话
      # ==========================================
      tryCatch(
        {
          # 1. 呼叫后端路由获取代码
          code <- call_custom_ai(prompt)
          
          # 2. 渲染 UI
          output$generated_code <- shiny::renderText(code)
          output$status_log <- shiny::renderText("✅ Code generated successfully!\n\nReview the code, then click 'Close' to exit.")
          
          # 3. 自动执行并实时预览图形
          if (input$auto_execute) {
            output$plot_preview <- shiny::renderPlot({
              # 屏蔽无害警告，直接在全局环境中执行 AI 生成的代码并渲染
              suppressWarnings(eval(parse(text = code), envir = .GlobalEnv))
            })
            
            output$status_log <- shiny::renderText(
              "✅ 代码生成并执行成功！\n\n图片已在上方实时预览。"
            )
          } else {
            # 如果用户取消勾选自动执行，就清空画板
            output$plot_preview <- shiny::renderPlot({ NULL })
          }
          
          # 4. 复制剪贴板
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
          # 如果后端的 call_custom_ai() 遇到任何问题，错误信息会直接抛给界面
          output$status_log <- shiny::renderText(
            paste0("❌ Error: ", e$message)
          )
          output$generated_code <- shiny::renderText("")
        }
      )
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
    
    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(NULL))
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}