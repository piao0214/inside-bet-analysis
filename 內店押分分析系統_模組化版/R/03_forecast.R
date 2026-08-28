# ========================================================
# ARIMA / LOG RETURN / RETURN 預測
# ========================================================

fit_arima_forecast <- function(x, h = 2, levels = c(85, 95), label = "") {
  x <- as.numeric(x)

  model <- tryCatch({
    if (!is.finite(sd(x)) || sd(x) < 1e-10) {
      forecast::Arima(
        x,
        order = c(0, 0, 0),
        include.mean = TRUE
      )
    } else {
      forecast::auto.arima(
        x,
        test = "adf",
        seasonal = FALSE,
        allowdrift = FALSE,
        allowmean = TRUE,
        start.p = 0,
        start.q = 0
      )
    }
  }, error = function(e) {
    stop(paste0(label, "ARIMA 建模失敗：", conditionMessage(e)))
  })

  fc <- tryCatch({
    forecast::forecast(model, h = h, level = levels)
  }, error = function(e) {
    stop(paste0(label, "預測失敗：", conditionMessage(e)))
  })

  lower_mat <- as.matrix(fc$lower)
  upper_mat <- as.matrix(fc$upper)

  shiny::validate(
    shiny::need(
      ncol(lower_mat) >= 2 && ncol(upper_mat) >= 2,
      paste0(label, "未成功產生 85% 與 95% 預測區間")
    )
  )

  list(
    model = model,
    order = forecast::arimaorder(model),
    mean = as.numeric(fc$mean),
    lower85 = as.numeric(lower_mat[, 1]),
    upper85 = as.numeric(upper_mat[, 1]),
    lower95 = as.numeric(lower_mat[, 2]),
    upper95 = as.numeric(upper_mat[, 2])
  )
}

make_backtest_forecast <- function(ts_obj) {
  df <- ts_obj$df
  ts_data <- as.numeric(df$平均押分)
  month_labels <- paste0(df$年, ".", df$月)
  n <- length(ts_data)

  shiny::validate(
    shiny::need(
      n >= 6,
      "至少需要 6 期資料，才能做 LOG RETURN 預測（最後兩期做測試）"
    ),
    shiny::need(all(is.finite(ts_data)), "平均押分含有缺值或非數值"),
    shiny::need(all(ts_data > 0), "平均押分含有 0 或負值，無法計算 LOG RETURN")
  )

  train_raw <- ts_data[1:(n - 2)]
  train_log_re <- diff(log(train_raw))

  shiny::validate(
    shiny::need(length(train_log_re) >= 3, "Training 的 LOG RETURN 期數不足，無法建模"),
    shiny::need(all(is.finite(train_log_re)), "Training LOG RETURN 含有非數值")
  )

  actual_test_log_re <- c(
    log(ts_data[n - 1] / ts_data[n - 2]),
    log(ts_data[n] / ts_data[n - 1])
  )

  shiny::validate(
    shiny::need(
      all(is.finite(actual_test_log_re)),
      "測試資料的 LOG RETURN 含有非數值"
    )
  )

  fit <- fit_arima_forecast(train_log_re, h = 2, levels = c(85, 95))

  list(
    target_name = ts_obj$target_name,
    period_text = ts_obj$period_text,
    df = df,
    ts_data = ts_data,
    month_labels = month_labels,
    n = n,
    train_log_re = train_log_re,
    actual_test_log_re = actual_test_log_re,
    model = fit$model,
    model_order = fit$order,
    fc_mean = fit$mean,
    fc_lower85 = fit$lower85,
    fc_upper85 = fit$upper85,
    fc_lower95 = fit$lower95,
    fc_upper95 = fit$upper95,
    train_return = exp(train_log_re) - 1,
    actual_test_return = exp(actual_test_log_re) - 1,
    fc_mean_return = exp(fit$mean) - 1,
    fc_lower85_return = exp(fit$lower85) - 1,
    fc_upper85_return = exp(fit$upper85) - 1,
    fc_lower95_return = exp(fit$lower95) - 1,
    fc_upper95_return = exp(fit$upper95) - 1
  )
}

make_future_forecast <- function(ts_obj) {
  df <- ts_obj$df
  ts_data <- as.numeric(df$平均押分)
  month_labels <- paste0(df$年, ".", df$月)
  n <- length(ts_data)

  shiny::validate(
    shiny::need(n >= 6, "至少需要 6 期資料，才能做兩期預測"),
    shiny::need(all(is.finite(ts_data)), "平均押分含有缺值或非數值"),
    shiny::need(all(ts_data > 0), "平均押分含有 0 或負值，無法計算 LOG RETURN")
  )

  full_log_re <- diff(log(ts_data))

  shiny::validate(
    shiny::need(length(full_log_re) >= 3, "LOG RETURN 期數不足，無法建模"),
    shiny::need(all(is.finite(full_log_re)), "LOG RETURN 含有非數值")
  )

  fit <- fit_arima_forecast(
    full_log_re,
    h = 2,
    levels = c(85, 95),
    label = "兩期預測："
  )

  last_year <- as.integer(tail(df$年, 1))
  last_month <- as.integer(tail(df$月, 1))
  base_month_index <- last_year * 12 + (last_month - 1)
  future_index <- base_month_index + 1:2
  future_year <- future_index %/% 12
  future_month <- future_index %% 12 + 1
  future_month_labels <- paste0(future_year, ".", future_month)

  list(
    target_name = ts_obj$target_name,
    period_text = ts_obj$period_text,
    df = df,
    ts_data = ts_data,
    n = n,
    month_labels = month_labels,
    full_log_re = full_log_re,
    historical_return = exp(full_log_re) - 1,
    future_model = fit$model,
    future_model_order = fit$order,
    future_month_labels = future_month_labels,
    future_fc_mean_log = fit$mean,
    future_mean_return = exp(fit$mean) - 1,
    future_lower85_return = exp(fit$lower85) - 1,
    future_upper85_return = exp(fit$upper85) - 1,
    future_lower95_return = exp(fit$lower95) - 1,
    future_upper95_return = exp(fit$upper95) - 1
  )
}
