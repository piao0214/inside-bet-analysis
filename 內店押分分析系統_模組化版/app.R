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
  
  # ------------------------------------------------------
  # Ollama：本機 / Connect Cloud 自動判斷
  # ------------------------------------------------------
  default_ollama_url <- OLLAMA_LOCAL_URL
  
  initial_backend <- detect_ollama_backend(default_ollama_url)
  ollama_backend <- reactiveVal(initial_backend)
  ollama_models <- reactiveVal(character(0))
  ollama_status <- reactiveVal("")
  
  choose_default_local_model <- function(models) {
    if (OLLAMA_LOCAL_DEFAULT_MODEL %in% models) {
      return(OLLAMA_LOCAL_DEFAULT_MODEL)
    }
    
    if ("qwen3:4b" %in% models) {
      return("qwen3:4b")
    }
    
    if (length(models) > 0) {
      return(models[1])
    }
    
    character(0)
  }
  
  load_local_models <- function(url = default_ollama_url) {
    tryCatch({
      models <- unname(get_ollama_models(base_url = url))
      models <- models[!is.na(models) & nzchar(models)]
      ollama_models(models)
      
      if (length(models) == 0) {
        ollama_status("已連上本機 Ollama，但目前沒有模型")
      } else {
        ollama_status(
          paste0(
            "目前使用：本機 Ollama｜找到模型：",
            paste(models, collapse = ", ")
          )
        )
      }
      
      invisible(models)
    }, error = function(e) {
      ollama_models(character(0))
      ollama_status(paste("本機 Ollama 連線失敗：", conditionMessage(e)))
      invisible(character(0))
    })
  }
  
  # 啟動時初始化 AI 來源。
  if (initial_backend == "local") {
    load_local_models(default_ollama_url)
  } else if (initial_backend == "cloud") {
    ollama_status(
      paste0(
        "目前使用：Ollama Cloud｜模型：",
        OLLAMA_CLOUD_DEFAULT_MODEL
      )
    )
  } else {
    ollama_status(
      paste0(
        "目前沒有可使用的 Ollama。",
        "本機請先啟動 Ollama；",
        "Connect Cloud 請設定 OLLAMA_API_KEY。"
      )
    )
  }
  
  # 動態 AI 控制介面：本機才顯示 URL / 模型選單；
  # Connect Cloud 則固定使用 Ollama Cloud。
  output$ollama_controls <- renderUI({
    backend <- ollama_backend()
    
    if (backend == "local") {
      models <- ollama_models()
      selected_model <- choose_default_local_model(models)
      
      return(
        tagList(
          tags$strong("AI 來源：本機 Ollama"),
          textInput(
            "ollama_url",
            "Ollama URL",
            value = default_ollama_url
          ),
          actionButton("refresh_models", "刷新模型"),
          verbatimTextOutput("ollama_status"),
          selectInput(
            "ollama_model",
            "Ollama 模型",
            choices = unname(models),
            selected = unname(selected_model)
          )
        )
      )
    }
    
    if (backend == "cloud") {
      return(
        tagList(
          tags$strong("AI 來源：Ollama Cloud"),
          helpText(
            paste0("模型：", OLLAMA_CLOUD_DEFAULT_MODEL)
          ),
          helpText(
            "Connect Cloud 會透過 OLLAMA_API_KEY 直接連線 Ollama Cloud。"
          ),
          verbatimTextOutput("ollama_status")
        )
      )
    }
    
    tagList(
      tags$strong("Ollama 尚未設定"),
      helpText(
        "本機請先啟動 Ollama；Connect Cloud 請在 Variables 設定 OLLAMA_API_KEY。"
      ),
      actionButton("redetect_ollama", "重新偵測 Ollama"),
      verbatimTextOutput("ollama_status")
    )
  })
  
  output$ollama_status <- renderText({
    ollama_status()
  })
  
  # 本機手動刷新模型。
  observeEvent(
    input$refresh_models,
    {
      req(ollama_backend() == "local")
      
      url <- if (!is.null(input$ollama_url) && nzchar(input$ollama_url)) {
        input$ollama_url
      } else {
        default_ollama_url
      }
      
      load_local_models(url)
    },
    ignoreInit = TRUE
  )
  
  # 若 Shiny 啟動時 Ollama 尚未開啟，可啟動 Ollama 後重新偵測。
  observeEvent(
    input$redetect_ollama,
    {
      backend <- detect_ollama_backend(default_ollama_url)
      ollama_backend(backend)
      
      if (backend == "local") {
        load_local_models(default_ollama_url)
      } else if (backend == "cloud") {
        ollama_status(
          paste0(
            "目前使用：Ollama Cloud｜模型：",
            OLLAMA_CLOUD_DEFAULT_MODEL
          )
        )
      } else {
        ollama_status(
          paste0(
            "仍找不到可使用的 Ollama。",
            "本機請確認 Ollama 已啟動；",
            "Connect Cloud 請確認 OLLAMA_API_KEY 已設定。"
          )
        )
      }
    },
    ignoreInit = TRUE
  )
  
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
      backend <- ollama_backend()
      
      validate(
        need(
          backend != "none",
          paste0(
            "目前沒有可使用的 Ollama。",
            "本機請先啟動 Ollama；",
            "Connect Cloud 請設定 OLLAMA_API_KEY。"
          )
        )
      )
      
      if (backend == "local") {
        validate(
          need(
            !is.null(input$ollama_model) && nzchar(input$ollama_model),
            "請先刷新模型並選擇一個 Ollama 模型"
          )
        )
      }
      
      ai_result("分析中，請稍候...")
      
      tryCatch({
        progress_message <- if (backend == "cloud") {
          "Ollama Cloud 分析中..."
        } else {
          "本機 Ollama 分析中..."
        }
        
        withProgress(message = progress_message, value = 0, {
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
          
          if (backend == "local") {
            local_url <- if (!is.null(input$ollama_url) && nzchar(input$ollama_url)) {
              input$ollama_url
            } else {
              default_ollama_url
            }
            
            result_text <- call_ai_chat(
              prompt = prompt,
              backend = "local",
              local_url = local_url,
              local_model = input$ollama_model
            )
          } else {
            result_text <- call_ai_chat(
              prompt = prompt,
              backend = "cloud",
              cloud_model = OLLAMA_CLOUD_DEFAULT_MODEL
            )
          }
          
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