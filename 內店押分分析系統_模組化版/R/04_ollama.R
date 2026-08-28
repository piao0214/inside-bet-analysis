# ========================================================
# Ollama API
# ========================================================

get_ollama_models <- function(base_url = "http://localhost:11434") {
  resp <- httr2::request(paste0(base_url, "/api/tags")) |>
    httr2::req_timeout(10) |>
    httr2::req_perform()

  obj <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (is.null(obj$models) || length(obj$models) == 0) return(character(0))

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
        if (is.list(x) && !is.null(x$name)) as.character(x$name) else NA_character_
      },
      character(1)
    )
    return(unname(model_names[!is.na(model_names)]))
  }

  character(0)
}

call_ollama_chat <- function(
    prompt,
    model,
    base_url = "http://localhost:11434",
    system_prompt = paste(
      "你是資料分析助理，請用繁體中文回答。",
      "請只根據我提供的數據分析，不要虛構外部原因，不要空泛。",
      "請依序輸出：趨勢摘要、波動解讀、預測解讀、風險提醒、管理建議。"
    )
) {
  resp <- httr2::request(paste0(base_url, "/api/chat")) |>
    httr2::req_timeout(120) |>
    httr2::req_body_json(list(
      model = model,
      stream = FALSE,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = prompt)
      )
    )) |>
    httr2::req_perform()

  obj <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  if (!is.null(obj$message) && is.list(obj$message) && !is.null(obj$message$content)) {
    return(as.character(obj$message$content))
  }

  if (!is.null(obj$message) && is.data.frame(obj$message) && "content" %in% names(obj$message)) {
    return(as.character(obj$message$content[1]))
  }

  if (!is.null(obj$response)) return(as.character(obj$response))

  stop("Ollama 沒有回傳可用內容")
}

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
