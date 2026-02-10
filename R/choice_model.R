build_choice_data <- function(signals_df) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  df <- signals_df
  df$signal_raw <- safe_scale(df$momentum) - safe_scale(df$tail_score) - safe_scale(df$spread_est)
  df$signal_raw[is.na(df$signal_raw)] <- 0

  df <- dplyr::group_by(df, date)
  df <- dplyr::mutate(
    df,
    task = dplyr::cur_group_id(),
    x = pmax(0, signal_raw),
    p = 1 + pmax(0, spread_est)
  )
  df <- dplyr::ungroup(df)

  df$id <- 1
  df$alt <- as.integer(factor(df$ticker))

  df
}

weights_from_signals <- function(signals_df, max_weight = 0.2) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  df <- signals_df
  df$signal <- safe_scale(df$momentum) - safe_scale(df$tail_score) - safe_scale(df$spread_est)
  df$signal[is.na(df$signal)] <- 0

  df <- dplyr::group_by(df, date)
  df <- dplyr::mutate(
    df,
    w = pmax(0, signal),
    w = ifelse(sum(w) == 0, 1 / dplyr::n(), w / sum(w)),
    w = pmin(w, max_weight)
  )
  df <- dplyr::mutate(df, w = w / sum(w))
  df <- dplyr::ungroup(df)

  df[, c("date", "ticker", "w")]
}

fit_choice_model <- function(choice_df, max_weight = 0.2, use_echoice2 = TRUE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  if (!use_echoice2 || !requireNamespace("echoice2", quietly = TRUE)) {
    return(weights_from_signals(choice_df, max_weight = max_weight))
  }

  est <- tryCatch(
    echoice2::vd_est_vdm(choice_df, R = 50, keep = 10, cores = 1),
    error = function(e) NULL
  )

  if (is.null(est)) {
    return(weights_from_signals(choice_df, max_weight = max_weight))
  }

  dem <- tryCatch(echoice2::vd_dem_vdm(choice_df, est, cores = 1), error = function(e) NULL)
  if (is.null(dem)) {
    return(weights_from_signals(choice_df, max_weight = max_weight))
  }

  dem_sum <- tryCatch(echoice2::vd_dem_summarise(dem), error = function(e) NULL)
  if (is.null(dem_sum)) {
    return(weights_from_signals(choice_df, max_weight = max_weight))
  }

  demand_col <- intersect(names(dem_sum), c("mean", "E_x", "x", "demand", "mu"))
  demand_col <- if (length(demand_col) > 0) demand_col[1] else NULL

  if (is.null(demand_col)) {
    return(weights_from_signals(choice_df, max_weight = max_weight))
  }

  dem_sum <- dplyr::select(dem_sum, task, alt, !!demand_col)
  names(dem_sum)[3] <- "demand"

  map_alt <- unique(choice_df[, c("alt", "ticker")])
  map_task <- unique(choice_df[, c("task", "date")])

  wdf <- dplyr::left_join(dem_sum, map_alt, by = "alt")
  wdf <- dplyr::left_join(wdf, map_task, by = "task")

  wdf <- dplyr::group_by(wdf, date)
  wdf <- dplyr::mutate(
    wdf,
    w = pmax(0, demand),
    w = ifelse(sum(w) == 0, 1 / dplyr::n(), w / sum(w)),
    w = pmin(w, max_weight)
  )
  wdf <- dplyr::mutate(wdf, w = w / sum(w))
  wdf <- dplyr::ungroup(wdf)

  wdf[, c("date", "ticker", "w")]
}
