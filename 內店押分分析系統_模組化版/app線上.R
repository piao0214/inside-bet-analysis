options(shiny.maxRequestSize = 100 * 1024^2)

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
library(scales)
library(purrr)
library(stringr)
library(readr)
library(httr2)

source(file.path("R", "01_utils.R"), encoding = "UTF-8")
source(file.path("R", "02_excel.R"), encoding = "UTF-8")
source(file.path("R", "03_forecast.R"), encoding = "UTF-8")
source(file.path("R", "04_ollama.R"), encoding = "UTF-8")
source(file.path("R", "05_plots.R"), encoding = "UTF-8")
source(file.path("R", "06_ui.R"), encoding = "UTF-8")

ui <- build_ui()

server <- function(input, output, session) {
  ai_result <- reactiveVal(
    paste0(
      "尚未產生分析。\n",
      "請先上傳 Excel、按「讀取資料」、",
      "選擇店家與遊戲及年月，再按「產生 AI 分析」。"
    )
  )
  
  ollama_status <- reactiveVal("尚未檢查 Ollama 狀態")
  
  # ------------------------------------------------------
  # Ollama 模型刷新
  # ------------------------------------------------------
  refresh_models_now <- function() {
    ollama_status("檢查中...")
    
    tryCatch({
      models <- unname(get_ollama_models(base_url = input$ollama_url))
      models <- models[!is.na(models) & nzchar(models)]
      
      default_model <- if ("qwen3:4b" %in% models) {
        "qwen3:4b"
      } else if ("gpt-oss:20b-cloud" %in% models) {
        "gpt-oss:20b-cloud"
      } else if (length(models) > 0) {
        models[1]
      } else {
        character(0)
      }
      
      updateSelectInput(
        session,
        "ollama_model",
        choices = unname(models),
        selected = unname(default_model)
      )
      
      if (length(models) == 0) {
        ollama_status("已連上 Ollama，但目前沒有模型")
      } else {
        ollama_status(
          paste0("已連上 Ollama，找到模型：", paste(models, collapse = ", "))
        )
      }
    }, error = function(e) {
      updateSelectInput(
        session,
        "ollama_model",
        choices = character(0),
        selected = character(0)
      )
      ollama_status(paste("連線失敗：", conditionMessage(e)))
    })
  }
  
  observe({
    refresh_models_now()
  })
  
  observeEvent(input$refresh_models, {
    refresh_models_now()
  })
  
  output$ollama_status <- renderText({
    ollama_status()
  })
  
  # ------------------------------------------------------
  # Excel 讀取
  # ------------------------------------------------------
  parsed_data <- eventReactive(input$load, {
    req(input$file)
    parse_excel_file(input$file$datapath)
  })
  
  observeEvent(parsed_data(), {
    pd <- parsed_data()
    
    store_choices <- unname(c(
      "--",
      sort(unique(na.omit(pd$store_summary$店家)))
    ))
    
    game_choices <- unname(c(
      "--",
      sort(unique(na.omit(pd$machine_detail$遊戲)))
    ))
    
    updateSelectInput(
      session,
      "store",
      choices = store_choices,
      selected = "--"
    )
    
    updateSelectInput(
      session,
      "game",
      choices = game_choices,
      selected = "--"
    )
  })
  
  # ------------------------------------------------------
  # 年月區間
  # ------------------------------------------------------
  valid_period <- reactive({
    validate(
      need(
        input$end_year > input$start_year ||
          (
            input$end_year == input$start_year &&
              input$end_month >= input$start_month
          ),
        "結束年月不能早於起始年月"
      )
    )
    TRUE
  })
  
  month_seq_df <- reactive({
    pd <- parsed_data()
    pd$month_info %>%
      select(月份, 年, 月) %>%
      distinct() %>%
      arrange(年, 月)
  })
  
  selected_months <- reactive({
    valid_period()
    
    month_seq_df() %>%
      filter(
        (
          年 > input$start_year |
            (年 == input$start_year & 月 >= input$start_month)
        ) &
          (
            年 < input$end_year |
              (年 == input$end_year & 月 <= input$end_month)
          )
      ) %>%
      arrange(年, 月)
  })
  
  # ------------------------------------------------------
  # 依店家 / 遊戲建立分析序列
  # ------------------------------------------------------
  target_series <- reactive({
    req(input$store, input$game)
    
    period_text <- period_text_fn(
      input$start_year,
      input$start_month,
      input$end_year,
      input$end_month
    )
    
    build_target_series(
      pd = parsed_data(),
      months_df = selected_months(),
      store_sel = input$store,
      game_sel = input$game,
      period_text = period_text
    )
  })
  
  # ------------------------------------------------------
  # 測試模型：最後兩期留作實際值比較
  # ------------------------------------------------------
  forecast_result <- reactive({
    make_backtest_forecast(target_series())
  })
  
  # ------------------------------------------------------
  # 真正兩期預測：使用全部已知資料重新建模
  # ------------------------------------------------------
  future_forecast_result <- reactive({
    make_future_forecast(target_series())
  })
  
  # ------------------------------------------------------
  # 1. 兩期預測
  # ------------------------------------------------------
  output$future_return_forecast_plot <- renderPlot({
    tryCatch({
      plot_two_period_forecast(future_forecast_result())
    }, error = function(e) {
      plot_error_message("兩期預測圖錯誤：", e)
    })
  })
  
  # ------------------------------------------------------
  # 2. LOG RETURN 測試
  # ------------------------------------------------------
  output$forecast_plot <- renderPlot({
    tryCatch({
      plot_log_return_test(forecast_result())
    }, error = function(e) {
      plot_error_message("LOG RETURN 測試圖錯誤：", e)
    })
  })
  
  # ------------------------------------------------------
  # 3. RETURN 測試
  # ------------------------------------------------------
  output$return_forecast_plot <- renderPlot({
    tryCatch({
      plot_return_test(forecast_result())
    }, error = function(e) {
      plot_error_message("RETURN 測試圖錯誤：", e)
    })
  })
  
  # ------------------------------------------------------
  # 4. AI 分析
  # ------------------------------------------------------
  observeEvent(
    input$analyze_ai,
    {
      req(input$ollama_model)
      
      validate(
        need(
          nzchar(input$ollama_model),
          "請先刷新模型並選擇一個 Ollama 模型"
        )
      )
      
      ai_result("分析中，請稍候...")
      
      tryCatch({
        withProgress(message = "Ollama 分析中...", value = 0, {
          fr <- forecast_result()
          incProgress(0.35)
          
          prompt <- build_ai_prompt(
            target_name = fr$target_name,
            period_text = fr$period_text,
            month_labels = fr$month_labels,
            ts_data = fr$ts_data,
            train_log_re = fr$train_log_re,
            actual_test_log_re = fr$actual_test_log_re,
            fc_mean = fr$fc_mean,
            fc_lower85 = fr$fc_lower85,
            fc_upper85 = fr$fc_upper85,
            fc_lower95 = fr$fc_lower95,
            fc_upper95 = fr$fc_upper95,
            model_order = fr$model_order
          )
          
          incProgress(0.70)
          
          result_text <- call_ollama_chat(
            prompt = prompt,
            model = input$ollama_model,
            base_url = input$ollama_url
          )
          
          incProgress(1)
          ai_result(result_text)
        })
      }, error = function(e) {
        ai_result(paste("AI 分析失敗：", conditionMessage(e)))
      })
    },
    ignoreInit = TRUE
  )
  
  output$ai_text <- renderText({
    ai_result()
  })
}

shinyApp(ui, server)
