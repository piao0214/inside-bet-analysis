# ========================================================
# 共用工具函數
# ========================================================

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

normalize_game_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_replace(x, "[-_[:space:]]*[0-9]+$", "")
  x <- stringr::str_replace(x, "[-_[:space:]]+$", "")
  x
}

safe_num_text <- function(x, digits = 4) {
  x <- as.numeric(x)
  if (length(x) == 0) return("無")
  paste(round(x, digits), collapse = ", ")
}

series_summary_text <- function(x) {
  x <- as.numeric(x)
  paste0(
    "平均=", round(mean(x, na.rm = TRUE), 2),
    "；標準差=", round(sd(x, na.rm = TRUE), 2),
    "；最小=", round(min(x, na.rm = TRUE), 2),
    "；最大=", round(max(x, na.rm = TRUE), 2)
  )
}

fill_na_with_mean <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

period_text_fn <- function(start_year, start_month, end_year, end_month) {
  paste0(start_year, ".", start_month, " ~ ", end_year, ".", end_month)
}

target_text_fn <- function(store, game) {
  if (store != "--" && game == "--") {
    paste0("店家：", store)
  } else if (store == "--" && game != "--") {
    paste0("遊戲：", game, "（全部店家）")
  } else if (store != "--" && game != "--") {
    paste0("店家：", store, "｜遊戲：", game)
  } else {
    "全部店家｜全部遊戲"
  }
}

plot_error_message <- function(prefix, e) {
  plot.new()
  text(0.5, 0.5, paste(prefix, conditionMessage(e)), cex = 1)
}
