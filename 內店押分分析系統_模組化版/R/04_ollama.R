# ========================================================
# Ollama API
# 本機：localhost:11434 -> gpt-oss:20b-cloud
# Connect Cloud：https://ollama.com/api -> gpt-oss:20b
# ========================================================

OLLAMA_LOCAL_URL <- "http://localhost:11434"
OLLAMA_CLOUD_URL <- "https://ollama.com"
OLLAMA_LOCAL_DEFAULT_MODEL <- "gpt-oss:20b-cloud"
OLLAMA_CLOUD_DEFAULT_MODEL <- "gpt-oss:20b"

clean_ollama_url <- function(x) {
  sub("/+$", "", trimws(as.character(x)))
}

is_connect_cloud <- function() {
  identical(
    tolower(Sys.getenv("R_CONFIG_ACTIVE", unset = "")),
    "connect_cloud"
  )
}

ollama_local_available <- function(base_url = OLLAMA_LOCAL_URL) {
  base_url <- clean_ollama_url(base_url)
  
  tryCatch({
    resp <- httr2::request(paste0(base_url, "/api/tags")) |>
      httr2::req_timeout(3) |>
      httr2::req_perform()
    
    status <- httr2::resp_status(resp)
    isTRUE(status >= 200 && status < 300)
  }, error = function(e) {
    FALSE
  })
}

# --------------------------------------------------------
# 自動判斷 AI 來源
# local：本機 Ollama
# cloud：Ollama Cloud API
# none：目前無可用 AI
# --------------------------------------------------------
detect_ollama_backend <- function(local_url = OLLAMA_LOCAL_URL) {
  # Posit Connect Cloud 不應嘗試 localhost，直接走 Ollama Cloud。
  if (is_connect_cloud()) {
    if (nzchar(Sys.getenv("OLLAMA_API_KEY", unset = ""))) {
      return("cloud")
    }
    return("none")
  }
  
  # 本機優先走已啟動的 Ollama。
  if (ollama_local_available(local_url)) {
    return("local")
  }
  
  # 本機若沒開 Ollama，但有設定 API Key，也可直接走雲端。
  if (nzchar(Sys.getenv("OLLAMA_API_KEY", unset = ""))) {
    return("cloud")
  }
  
  "none"
}

make_ollama_request <- function(url, api_key = NULL) {
  req <- httr2::request(url)
  
  if (!is.null(api_key) && nzchar(api_key)) {
    req <- req |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_key)
      )
  }
  
  req
}

extract_ollama_model_names <- function(obj) {
  if (is.null(obj$models) || length(obj$models) == 0) {
    return(character(0))
  }
  
  if (is.data.frame(obj$models)) {
    if ("name" %in% names(obj$models)) {
      return(unname(as.character(obj$models$name)))
    }
    return(character(0))
  }
  
  if (is.list(obj$models)) {
    model_names <- vapply(
      obj$models,
      function(x) {
        if (is.list(x) && !is.null(x$name)) {
          as.character(x$name)
        } else if (is.list(x) && !is.null(x$model)) {
          as.character(x$model)
        } else {
          NA_character_
        }
      },
      character(1)
    )
    
    return(unname(model_names[!is.na(model_names) & nzchar(model_names)]))
  }
  
  character(0)
}

# --------------------------------------------------------
# 取得模型清單
# 本機：不需 API Key
# 雲端：需 OLLAMA_API_KEY
# --------------------------------------------------------
get_ollama_models <- function(
    base_url = OLLAMA_LOCAL_URL,
    api_key = NULL
) {
  base_url <- clean_ollama_url(base_url)
  
  resp <- make_ollama_request(
    paste0(base_url, "/api/tags"),
    api_key = api_key
  ) |>
    httr2::req_timeout(10) |>
    httr2::req_perform()
  
  obj <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  extract_ollama_model_names(obj)
}

extract_ollama_text <- function(obj) {
  if (!is.null(obj$message) && is.list(obj$message) && !is.null(obj$message$content)) {
    return(as.character(obj$message$content))
  }
  
  if (!is.null(obj$message) && is.data.frame(obj$message) && "content" %in% names(obj$message)) {
    return(as.character(obj$message$content[1]))
  }
  
  if (!is.null(obj$response)) {
    return(as.character(obj$response))
  }
  
  stop("Ollama 沒有回傳可用內容")
}

# --------------------------------------------------------
# 通用 Ollama Chat 呼叫
# base_url 可為 localhost 或 https://ollama.com
# --------------------------------------------------------
call_ollama_chat <- function(
    prompt,
    model,
    base_url = OLLAMA_LOCAL_URL,
    api_key = NULL,
    system_prompt = paste(
      "你是資料分析助理，請用繁體中文回答。",
      "請只根據我提供的數據分析，不要虛構外部原因，不要空泛。",
      "請依序輸出：趨勢摘要、波動解讀、預測解讀、風險提醒、管理建議。"
    )
) {
  base_url <- clean_ollama_url(base_url)
  
  resp <- make_ollama_request(
    paste0(base_url, "/api/chat"),
    api_key = api_key
  ) |>
    httr2::req_timeout(120) |>
    httr2::req_body_json(
      list(
        model = model,
        stream = FALSE,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = prompt)
        )
      )
    ) |>
    httr2::req_perform()
  
  obj <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  extract_ollama_text(obj)
}

# --------------------------------------------------------
# 統一入口
# 本機：gpt-oss:20b-cloud
# Connect Cloud：gpt-oss:20b + OLLAMA_API_KEY
# --------------------------------------------------------
call_ai_chat <- function(
    prompt,
    backend = c("auto", "local", "cloud"),
    local_url = OLLAMA_LOCAL_URL,
    local_model = OLLAMA_LOCAL_DEFAULT_MODEL,
    cloud_model = OLLAMA_CLOUD_DEFAULT_MODEL,
    api_key = Sys.getenv("OLLAMA_API_KEY", unset = "")
) {
  backend <- match.arg(backend)
  
  if (backend == "auto") {
    backend <- detect_ollama_backend(local_url)
  }
  
  if (backend == "local") {
    if (is.null(local_model) || length(local_model) == 0 || !nzchar(local_model)) {
      stop("尚未選擇本機 Ollama 模型")
    }
    
    return(
      call_ollama_chat(
        prompt = prompt,
        model = local_model,
        base_url = local_url
      )
    )
  }
  
  if (backend == "cloud") {
    if (!nzchar(api_key)) {
      stop("找不到 OLLAMA_API_KEY，請先在 Posit Connect Cloud 的 Variables 中設定")
    }
    
    return(
      call_ollama_chat(
        prompt = prompt,
        model = cloud_model,
        base_url = OLLAMA_CLOUD_URL,
        api_key = api_key
      )
    )
  }
  
  stop(
    paste0(
      "目前沒有可使用的 Ollama。\n",
      "本機：請先啟動 Ollama。\n",
      "Connect Cloud：請設定 OLLAMA_API_KEY。"
    )
  )
}

# ========================================================
# AI Prompt
# ========================================================

build_ai_prompt <- function(
    target_name,
    period_text,
    month_labels,
    ts_data,
    train_log_re,
    actual_test_log_re,
    fc_mean,
    fc_lower85,
    fc_upper85,
    fc_lower95,
    fc_upper95,
    model_order
) {
  paste0(
    "請根據以下資料，用繁體中文分析。\n\n",
    "規則：\n",
    "- 只能依據提供資料解讀\n",
    "- 不要猜節慶、行銷、客群、競品等外部原因\n",
    "- 若無法明確判斷，請直接說不確定\n",
    "- 內容請具體、精簡，控制在 250 到 450 字\n\n",
    "分析對象：", target_name, "\n",
    "分析區間：", period_text, "\n",
    "月份序列：", paste(month_labels, collapse = ", "), "\n",
    "平均押分序列：", safe_num_text(ts_data, 2), "\n",
    "平均押分摘要：", series_summary_text(ts_data), "\n",
    "Training Log Return：", safe_num_text(train_log_re, 4), "\n",
    "Actual Test Log Return：", safe_num_text(actual_test_log_re, 4), "\n",
    "ARIMA 階數：(p,d,q)=(", paste(model_order, collapse = ","), ")\n",
    "預測均值：", safe_num_text(fc_mean, 4), "\n",
    "85% 下界：", safe_num_text(fc_lower85, 4), "\n",
    "85% 上界：", safe_num_text(fc_upper85, 4), "\n",
    "95% 下界：", safe_num_text(fc_lower95, 4), "\n",
    "95% 上界：", safe_num_text(fc_upper95, 4), "\n\n",
    "請依序輸出：\n",
    "1. 趨勢摘要\n",
    "2. 波動解讀\n",
    "3. 預測解讀\n",
    "4. 風險提醒\n",
    "5. 一句管理建議"
  )
}