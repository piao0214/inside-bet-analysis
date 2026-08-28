# ========================================================
# 圖表函數
# ========================================================

plot_log_return_test <- function(fr) {
  df_plot <- data.frame(
    time = c(2:(fr$n - 2), (fr$n - 1):fr$n),
    value = c(fr$train_log_re, fr$actual_test_log_re),
    type = c(
      rep("Training Log Return", length(fr$train_log_re)),
      rep("Actual Test Log Return", 2)
    )
  )

  df_fc <- data.frame(
    time = (fr$n - 1):fr$n,
    mean = fr$fc_mean,
    lower85 = fr$fc_lower85,
    upper85 = fr$fc_upper85,
    lower95 = fr$fc_lower95,
    upper95 = fr$fc_upper95
  )

  y_min <- min(c(df_plot$value, df_fc$lower95), na.rm = TRUE)
  y_max <- max(c(df_plot$value, df_fc$upper95), na.rm = TRUE)
  pad <- 0.05 * (y_max - y_min)
  if (!is.finite(pad) || pad == 0) pad <- 0.05

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df_fc,
      ggplot2::aes(x = time, ymin = lower95, ymax = upper95, group = 1),
      fill = "red", alpha = 0.10
    ) +
    ggplot2::geom_ribbon(
      data = df_fc,
      ggplot2::aes(x = time, ymin = lower85, ymax = upper85, group = 1),
      fill = "red", alpha = 0.22
    ) +
    ggplot2::geom_line(
      data = df_plot,
      ggplot2::aes(x = time, y = value, color = type, group = 1),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_plot,
      ggplot2::aes(x = time, y = value, color = type),
      size = 2
    ) +
    ggplot2::geom_line(
      data = df_fc,
      ggplot2::aes(x = time, y = mean, group = 1),
      color = "red", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_fc,
      ggplot2::aes(x = time, y = mean),
      color = "red", shape = 17, size = 3
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::scale_x_continuous(
      breaks = 2:fr$n,
      labels = fr$month_labels[2:fr$n],
      minor_breaks = NULL
    ) +
    ggplot2::scale_color_manual(values = c(
      "Training Log Return" = "black",
      "Actual Test Log Return" = "blue"
    )) +
    ggplot2::coord_cartesian(ylim = c(y_min - pad, y_max + pad)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste(fr$target_name, "LOG RETURN 測試：預測 vs 實際"),
      subtitle = paste0(
        "區間：", fr$period_text,
        "｜最後兩期做測試｜深紅 = 85% CI｜淺紅 = 95% CI"
      ),
      x = "年份.月份",
      y = "LOG RETURN"
    )
}

plot_return_test <- function(fr) {
  df_actual <- data.frame(
    time = 2:fr$n,
    value = c(fr$train_return, fr$actual_test_return),
    type = c(
      rep("Training Return", length(fr$train_return)),
      rep("Actual Test Return", length(fr$actual_test_return))
    )
  )

  df_fc <- data.frame(
    time = (fr$n - 1):fr$n,
    mean = fr$fc_mean_return,
    lower85 = fr$fc_lower85_return,
    upper85 = fr$fc_upper85_return,
    lower95 = fr$fc_lower95_return,
    upper95 = fr$fc_upper95_return
  )

  y_min <- min(c(df_actual$value, df_fc$lower95), na.rm = TRUE)
  y_max <- max(c(df_actual$value, df_fc$upper95), na.rm = TRUE)
  pad <- 0.05 * (y_max - y_min)
  if (!is.finite(pad) || pad == 0) pad <- 0.05

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df_fc,
      ggplot2::aes(x = time, ymin = lower95, ymax = upper95, group = 1),
      fill = "red", alpha = 0.10
    ) +
    ggplot2::geom_ribbon(
      data = df_fc,
      ggplot2::aes(x = time, ymin = lower85, ymax = upper85, group = 1),
      fill = "red", alpha = 0.22
    ) +
    ggplot2::geom_line(
      data = df_actual,
      ggplot2::aes(x = time, y = value, color = type, group = 1),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_actual,
      ggplot2::aes(x = time, y = value, color = type),
      size = 2
    ) +
    ggplot2::geom_line(
      data = df_fc,
      ggplot2::aes(x = time, y = mean, group = 1),
      color = "red", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_fc,
      ggplot2::aes(x = time, y = mean),
      color = "red", shape = 17, size = 3
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::scale_x_continuous(
      breaks = 2:fr$n,
      labels = fr$month_labels[2:fr$n],
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::scale_color_manual(values = c(
      "Training Return" = "black",
      "Actual Test Return" = "blue"
    )) +
    ggplot2::coord_cartesian(ylim = c(y_min - pad, y_max + pad)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste(fr$target_name, "RETURN 測試：預測 vs 實際"),
      subtitle = paste0(
        "區間：", fr$period_text,
        "｜ARIMA 以 LOG RETURN 建模後轉成一般 RETURN",
        "｜最後兩期做測試｜深紅 = 85% CI｜淺紅 = 95% CI"
      ),
      x = "年份.月份",
      y = "RETURN"
    )
}

plot_two_period_forecast <- function(fr) {
  df_history <- data.frame(
    time = 2:fr$n,
    value = fr$historical_return
  )

  df_future <- data.frame(
    time = (fr$n + 1):(fr$n + 2),
    mean = fr$future_mean_return,
    lower85 = fr$future_lower85_return,
    upper85 = fr$future_upper85_return,
    lower95 = fr$future_lower95_return,
    upper95 = fr$future_upper95_return
  )

  y_min <- min(c(df_history$value, df_future$lower95, 0), na.rm = TRUE)
  y_max <- max(c(df_history$value, df_future$upper95, 0), na.rm = TRUE)
  pad <- 0.05 * (y_max - y_min)
  if (!is.finite(pad) || pad == 0) pad <- 0.05

  x_breaks <- c(2:fr$n, fr$n + 1, fr$n + 2)
  x_labels <- c(fr$month_labels[2:fr$n], fr$future_month_labels)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df_future,
      ggplot2::aes(x = time, ymin = lower95, ymax = upper95, group = 1),
      fill = "red", alpha = 0.10
    ) +
    ggplot2::geom_ribbon(
      data = df_future,
      ggplot2::aes(x = time, ymin = lower85, ymax = upper85, group = 1),
      fill = "red", alpha = 0.22
    ) +
    ggplot2::geom_line(
      data = df_history,
      ggplot2::aes(x = time, y = value, group = 1),
      color = "black", linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_history,
      ggplot2::aes(x = time, y = value),
      color = "black", size = 2
    ) +
    ggplot2::geom_line(
      data = df_future,
      ggplot2::aes(x = time, y = mean, group = 1),
      color = "red", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_future,
      ggplot2::aes(x = time, y = mean),
      color = "red", shape = 17, size = 3.5
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::coord_cartesian(ylim = c(y_min - pad, y_max + pad)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste(fr$target_name, "兩期 RETURN 預測"),
      subtitle = paste0(
        "歷史資料使用至 ", tail(fr$month_labels, 1),
        "｜使用全部資料重新配適 ARIMA(",
        paste(fr$future_model_order, collapse = ","), ")",
        "｜紅色三角形 = 未來兩期 RETURN 預測",
        "｜深紅 = 85% CI｜淺紅 = 95% CI"
      ),
      x = "年份.月份",
      y = "RETURN"
    )
}
