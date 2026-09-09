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


# ------------------------------------------------------
# 兩期預測分類工具
# 判定規則：
#   85% CI 完全高於 0 -> 上漲
#   85% CI 包含 0     -> 正常
#   85% CI 完全低於 0 -> 下跌
# ------------------------------------------------------
classify_85ci_direction <- function(lower85, upper85) {
  dplyr::case_when(
    lower85 > 0 ~ "上漲",
    upper85 < 0 ~ "下跌",
    TRUE ~ "正常"
  )
}

next_two_month_labels <- function(months_df) {
  shiny::validate(
    shiny::need(nrow(months_df) > 0, "所選年月區間沒有資料")
  )

  last_year <- as.integer(tail(months_df$年, 1))
  last_month <- as.integer(tail(months_df$月, 1))

  base_month_index <- last_year * 12 + (last_month - 1)
  future_index <- base_month_index + 1:2

  paste0(
    future_index %/% 12,
    ".",
    future_index %% 12 + 1
  )
}

forecast_direction_for_items <- function(
    pd,
    months_df,
    period_text,
    items,
    type = c("game", "store")
) {
  type <- match.arg(type)

  purrr::map_dfr(items, function(item) {
    tryCatch({
      ts_obj <- if (type == "store") {
        build_target_series(
          pd = pd,
          months_df = months_df,
          store_sel = item,
          game_sel = "--",
          period_text = period_text
        )
      } else {
        build_target_series(
          pd = pd,
          months_df = months_df,
          store_sel = "--",
          game_sel = item,
          period_text = period_text
        )
      }

      fr <- make_future_forecast(ts_obj)

      tibble::tibble(
        項目 = item,
        期別 = seq_along(fr$future_month_labels),
        預測月份 = fr$future_month_labels,
        預測RETURN = fr$future_mean_return,
        `85%下界` = fr$future_lower85_return,
        `85%上界` = fr$future_upper85_return,
        趨勢 = classify_85ci_direction(
          fr$future_lower85_return,
          fr$future_upper85_return
        ),
        錯誤 = NA_character_
      )
    }, error = function(e) {
      tibble::tibble(
        項目 = item,
        期別 = NA_integer_,
        預測月份 = NA_character_,
        預測RETURN = NA_real_,
        `85%下界` = NA_real_,
        `85%上界` = NA_real_,
        趨勢 = "資料不足",
        錯誤 = conditionMessage(e)
      )
    })
  })
}

make_direction_summary_table <- function(detail_df, future_labels) {
  directions <- c("上漲", "正常", "下跌")

  collapse_items <- function(direction, period_no) {
    x <- detail_df$項目[
      detail_df$趨勢 == direction &
        detail_df$期別 == period_no
    ]

    x <- sort(unique(x[!is.na(x) & nzchar(x)]))

    if (length(x) == 0) {
      return("—")
    }

    paste(x, collapse = "、")
  }

  out <- tibble::tibble(
    趨勢 = directions,
    第一期 = vapply(
      directions,
      collapse_items,
      character(1),
      period_no = 1
    ),
    第二期 = vapply(
      directions,
      collapse_items,
      character(1),
      period_no = 2
    ),
    分析 = c(
      "85% CI 下界 > 0：整段皆高於 0",
      "85% CI 包含 0：方向不夠明確",
      "85% CI 上界 < 0：整段皆低於 0"
    )
  )

  names(out)[2:3] <- paste0(future_labels, " 預測")
  out
}

make_unclassified_note <- function(detail_df) {
  bad <- detail_df |>
    dplyr::filter(is.na(期別)) |>
    dplyr::distinct(項目)

  if (nrow(bad) == 0) {
    return("")
  }

  paste0(
    "未列入分類（資料不足或建模失敗）：",
    paste(bad$項目, collapse = "、")
  )
}

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
  # 4. 預測分類：全部遊戲 / 全部店家批次兩期預測
  #
  # 遊戲表：每個遊戲以「全部店家」資料預測
  # 店家表：每個店家以「全部遊戲」資料預測
  #
  # 判定使用兩期預測圖相同的 85% CI：
  #   lower85 > 0 -> 上漲
  #   lower85 <= 0 <= upper85 -> 正常
  #   upper85 < 0 -> 下跌
  # ------------------------------------------------------
  direction_result <- reactive({
    pd <- parsed_data()
    months_df <- selected_months()

    validate(
      need(nrow(months_df) > 0, "所選年月區間沒有資料")
    )

    period_text <- period_text_fn(
      input$start_year,
      input$start_month,
      input$end_year,
      input$end_month
    )

    month_keys <- months_df$月份

    game_items <- pd$machine_detail |>
      filter(
        月份 %in% month_keys,
        !is.na(遊戲),
        nzchar(trimws(遊戲)),
        !is.na(押分)
      ) |>
      distinct(遊戲) |>
      arrange(遊戲) |>
      pull(遊戲)

    store_items <- pd$store_summary |>
      filter(
        月份 %in% month_keys,
        !is.na(店家),
        nzchar(trimws(店家)),
        !is.na(平均押分)
      ) |>
      distinct(店家) |>
      arrange(店家) |>
      pull(店家)

    validate(
      need(length(game_items) > 0, "所選年月區間沒有可預測的遊戲"),
      need(length(store_items) > 0, "所選年月區間沒有可預測的店家")
    )

    list(
      future_labels = next_two_month_labels(months_df),
      game_detail = forecast_direction_for_items(
        pd = pd,
        months_df = months_df,
        period_text = period_text,
        items = game_items,
        type = "game"
      ),
      store_detail = forecast_direction_for_items(
        pd = pd,
        months_df = months_df,
        period_text = period_text,
        items = store_items,
        type = "store"
      )
    )
  })

  output$game_direction_table <- renderTable({
    dr <- direction_result()

    make_direction_summary_table(
      dr$game_detail,
      dr$future_labels
    )
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "m",
  width = "100%",
  rownames = FALSE
  )

  output$store_direction_table <- renderTable({
    dr <- direction_result()

    make_direction_summary_table(
      dr$store_detail,
      dr$future_labels
    )
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "m",
  width = "100%",
  rownames = FALSE
  )

  output$game_direction_note <- renderUI({
    note <- make_unclassified_note(
      direction_result()$game_detail
    )

    if (!nzchar(note)) {
      return(NULL)
    }

    tags$p(
      style = "color:#777; margin-top:8px;",
      note
    )
  })

  output$store_direction_note <- renderUI({
    note <- make_unclassified_note(
      direction_result()$store_detail
    )

    if (!nzchar(note)) {
      return(NULL)
    }

    tags$p(
      style = "color:#777; margin-top:8px;",
      note
    )
  })

  # ------------------------------------------------------
  # 5. AI 分析
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
