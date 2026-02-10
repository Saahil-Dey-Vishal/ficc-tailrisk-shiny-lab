optimize_portfolio_weights <- function(returns_xts, base_weights_df, max_weight = 0.2, use_pkg = TRUE, blend = 0.5) {
  tickers <- colnames(returns_xts)

  opt_weights <- rep(1 / length(tickers), length(tickers))
  names(opt_weights) <- tickers

  if (use_pkg && requireNamespace("portfolio.optimization", quietly = TRUE)) {
    scenario <- as.matrix(returns_xts)

    opt_weights <- tryCatch({
      model <- portfolio.optimization::portfolio.model(scenario)
      model <- portfolio.optimization::objective(model, "expected.shortfall")
      model <- portfolio.optimization::alpha(model, 0.05)
      model <- portfolio.optimization::upper.bound(model, max_weight)
      model <- portfolio.optimization::lower.bound(model, 0)
      model <- portfolio.optimization::optimal.portfolio(model)
      w <- portfolio.optimization::weights(model)
      as.numeric(w)
    }, error = function(e) opt_weights)

    names(opt_weights) <- tickers
  }

  opt_weights <- pmin(opt_weights, max_weight)
  opt_weights <- opt_weights / sum(opt_weights)

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  dates <- sort(unique(base_weights_df$date))

  opt_df <- data.frame(
    date = rep(dates, each = length(tickers)),
    ticker = rep(tickers, times = length(dates)),
    w_opt = rep(opt_weights, times = length(dates))
  )

  merged <- dplyr::left_join(base_weights_df, opt_df, by = c("date", "ticker"))
  merged$w_opt[is.na(merged$w_opt)] <- 0

  merged$w <- blend * merged$w + (1 - blend) * merged$w_opt
  merged <- dplyr::group_by(merged, date)
  merged <- dplyr::mutate(merged, w = w / sum(w))
  merged <- dplyr::ungroup(merged)

  merged[, c("date", "ticker", "w")]
}
