# ========================================================
# Excel 讀取與資料整理
# ========================================================

split_sheet_to_shop <- function(df) {
  is_shop_row <- !is.na(df[[1]]) &
    apply(df[, -1, drop = FALSE], 1, function(x) all(is.na(x)))

  shop_id <- cumsum(is_shop_row)
  df2 <- df %>% mutate(shop_id = shop_id)

  shop_names <- df2 %>%
    filter(is_shop_row) %>%
    select(shop_id, shop = ...1)

  df_data <- df2 %>%
    filter(
      !is_shop_row,
      !...1 %in% c("機台編號", "遊戲名稱"),
      !is.na(...2)
    )

  shop_list <- df_data %>% group_split(shop_id)
  names(shop_list) <- shop_names$shop
  lapply(shop_list, function(d) d %>% select(-shop_id))
}

parse_excel_file <- function(file_path) {
  all_sheets <- readxl::excel_sheets(file_path)

  target_months <- all_sheets[
    stringr::str_detect(all_sheets, "^[0-9]+\\.[0-9]+月報表$")
  ]

  shiny::validate(
    shiny::need(
      length(target_months) > 0,
      "Excel 裡沒有找到符合格式的「X.Y月報表」工作表"
    )
  )

  month_info <- tibble::tibble(月份 = target_months) %>%
    tidyr::extract(
      月份,
      into = c("年", "月"),
      regex = "^([0-9]+)\\.([0-9]+)月報表$",
      convert = TRUE,
      remove = FALSE
    ) %>%
    arrange(年, 月)

  target_months <- month_info$月份

  df_list <- lapply(target_months, function(s) {
    readxl::read_excel(file_path, sheet = s, col_names = FALSE)
  })
  names(df_list) <- target_months
  df_list <- lapply(df_list, split_sheet_to_shop)

  store_summary_df <- purrr::map_dfr(names(df_list), function(month) {
    purrr::map_dfr(names(df_list[[month]]), function(store) {
      d <- df_list[[month]][[store]]

      summary_row <- d %>%
        filter(...2 %in% c("加總", "總計", "合計")) %>%
        slice(1)

      if (nrow(summary_row) == 0) {
        return(tibble::tibble(
          月份 = month,
          店家 = store,
          機台數 = NA_real_,
          總押分 = NA_real_,
          總開分 = NA_real_,
          總淨值 = NA_real_
        ))
      }

      t1 <- safe_num(summary_row$...3[1])
      t2 <- safe_num(summary_row$...6[1])
      t3 <- safe_num(summary_row$...12[1])

      machine_count <- d %>%
        filter(!(...2 %in% c("加總", "總計", "合計"))) %>%
        filter(!is.na(safe_num(...3))) %>%
        nrow()

      tibble::tibble(
        月份 = month,
        店家 = store,
        機台數 = machine_count,
        總押分 = t1,
        總開分 = t2,
        總淨值 = t3
      )
    })
  }) %>%
    tidyr::extract(
      月份,
      into = c("年", "月"),
      regex = "^([0-9]+)\\.([0-9]+)月報表$",
      convert = TRUE,
      remove = FALSE
    ) %>%
    arrange(店家, 年, 月) %>%
    group_by(店家) %>%
    mutate(
      期數 = row_number(),
      平均押分 = ifelse(
        機台數 > 0,
        round(總押分 / 機台數, 2),
        NA_real_
      )
    ) %>%
    ungroup()

  machine_detail_df <- purrr::map_dfr(names(df_list), function(month) {
    purrr::map_dfr(names(df_list[[month]]), function(store) {
      d <- df_list[[month]][[store]]

      d %>%
        filter(!(...2 %in% c("加總", "總計", "合計"))) %>%
        transmute(
          月份 = month,
          店家 = as.character(store),
          機台編號 = as.character(...1),
          遊戲原始名稱 = as.character(...2),
          遊戲 = normalize_game_name(...2),
          押分 = safe_num(...3),
          開分 = safe_num(...6),
          淨值 = safe_num(...12)
        ) %>%
        filter(!is.na(遊戲), nzchar(trimws(遊戲)))
    })
  }) %>%
    tidyr::extract(
      月份,
      into = c("年", "月"),
      regex = "^([0-9]+)\\.([0-9]+)月報表$",
      convert = TRUE,
      remove = FALSE
    ) %>%
    arrange(店家, 遊戲, 年, 月, 機台編號)

  list(
    store_summary = store_summary_df,
    machine_detail = machine_detail_df,
    month_info = month_info
  )
}

build_target_series <- function(pd, months_df, store_sel, game_sel, period_text) {
  shiny::validate(
    shiny::need(nrow(months_df) > 0, "所選年月區間沒有資料")
  )

  target_name <- target_text_fn(store_sel, game_sel)

  if (store_sel != "--" && game_sel == "--") {
    df <- pd$store_summary %>%
      filter(店家 == store_sel) %>%
      select(
        月份, 年, 月, 店家, 機台數,
        總押分, 總開分, 總淨值, 平均押分
      ) %>%
      right_join(months_df, by = c("月份", "年", "月")) %>%
      arrange(年, 月)

    shiny::validate(
      shiny::need(
        any(!is.na(df$平均押分)),
        "此店家在所選區間沒有可用的平均押分資料"
      )
    )

    df <- df %>%
      mutate(
        平均押分 = fill_na_with_mean(平均押分),
        期數_plot = row_number()
      )

    return(list(
      df = df,
      target_name = target_name,
      period_text = period_text,
      mode = "store_only"
    ))
  }

  md <- pd$machine_detail
  if (store_sel != "--") md <- md %>% filter(店家 == store_sel)
  if (game_sel != "--") md <- md %>% filter(遊戲 == game_sel)

  shiny::validate(
    shiny::need(nrow(md) > 0, "目前選取的店家與遊戲沒有符合資料")
  )

  agg_df <- md %>%
    group_by(月份, 年, 月) %>%
    summarise(
      機台數 = sum(!is.na(押分)),
      總押分 = ifelse(
        all(is.na(押分)),
        NA_real_,
        sum(押分, na.rm = TRUE)
      ),
      平均押分 = ifelse(
        all(is.na(押分)),
        NA_real_,
        mean(押分, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    right_join(months_df, by = c("月份", "年", "月")) %>%
    arrange(年, 月)

  shiny::validate(
    shiny::need(
      any(!is.na(agg_df$平均押分)),
      "此條件在所選區間沒有可用資料"
    )
  )

  agg_df <- agg_df %>%
    mutate(
      平均押分 = ifelse(is.nan(平均押分), NA_real_, 平均押分),
      平均押分 = fill_na_with_mean(平均押分),
      期數_plot = row_number()
    )

  list(
    df = agg_df,
    target_name = target_name,
    period_text = period_text,
    mode = "game_based"
  )
}
