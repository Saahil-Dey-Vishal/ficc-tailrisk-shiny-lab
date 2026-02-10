load_universe <- function(path = "config/universe.csv") {
  if (!file.exists(path)) stop("Universe file not found: ", path)
  read.csv(path, stringsAsFactors = FALSE)
}

compute_momentum <- function(returns_xts, window = 13) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required for momentum.")
  }

  roll_fun <- function(x) prod(1 + x, na.rm = TRUE) - 1
  zoo::rollapply(returns_xts, width = window, FUN = roll_fun, align = "right", fill = NA)
}

run_pipeline <- function(start,
                         end,
                         max_weight = 0.2,
                         drawdown_limit = 0.08,
                         tail_window = 52,
                         momentum_window = 13,
                         n_factors = 3,
                         use_echoice2 = TRUE,
                         use_opt = TRUE,
                         use_regimes = TRUE,
                         regime_rates_ticker = "TLT",
                         regime_credit_ticker = "HYG",
                         regime_ig_ticker = "LQD",
                         regime_rate_threshold = 0,
                         regime_credit_threshold = 0,
                         risk_off_ticker = "BIL") {
  if (!requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'dplyr' and 'tidyr' are required.")
  }

  universe <- load_universe()
  tickers <- universe$ticker

  data <- fetch_weekly_ohlc(tickers = tickers, from = start, to = end)
  prices <- data$prices
  ohlc_list <- data$ohlc

  returns <- compute_weekly_returns(prices)
  momentum_xts <- compute_momentum(returns, window = momentum_window)

  tail_df <- compute_tail_risk_scores(returns, window = tail_window)
  spread_df <- compute_liquidity_spread(ohlc_list)

  hofa <- compute_hofa_factors(returns, n_factors = n_factors)

  returns_long <- tibble::tibble(
    date = as.Date(xts::index(returns)),
    !!!as.data.frame(returns)
  )
  returns_long <- tidyr::pivot_longer(returns_long, -date, names_to = "ticker", values_to = "ret_w")

  momentum_long <- tibble::tibble(
    date = as.Date(xts::index(momentum_xts)),
    !!!as.data.frame(momentum_xts)
  )
  momentum_long <- tidyr::pivot_longer(momentum_long, -date, names_to = "ticker", values_to = "momentum")

  signals <- dplyr::left_join(returns_long, momentum_long, by = c("date", "ticker"))
  signals <- dplyr::left_join(signals, tail_df[, c("date", "ticker", "tail_score")], by = c("date", "ticker"))
  signals <- dplyr::left_join(signals, spread_df, by = c("date", "ticker"))

  signals <- dplyr::mutate(
    signals,
    momentum = tidyr::replace_na(momentum, 0),
    tail_score = tidyr::replace_na(tail_score, 0),
    spread_est = tidyr::replace_na(spread_est, 0)
  )

  choice_data <- build_choice_data(signals)
  weights_raw <- fit_choice_model(choice_data, max_weight = max_weight, use_echoice2 = use_echoice2)
  weights_raw <- optimize_portfolio_weights(returns, weights_raw, max_weight = max_weight, use_pkg = use_opt, blend = 0.6)

  regimes <- compute_regimes(
    momentum_xts,
    rates_ticker = regime_rates_ticker,
    credit_ticker = regime_credit_ticker,
    ig_ticker = regime_ig_ticker,
    rate_threshold = regime_rate_threshold,
    credit_threshold = regime_credit_threshold
  )

  if (use_regimes) {
    weights <- apply_regime_filters(weights_raw, regimes, risk_off_ticker = risk_off_ticker)
  } else {
    weights <- weights_raw
  }

  backtest <- simulate_portfolio(returns, weights, drawdown_limit = drawdown_limit, risk_off_ticker = risk_off_ticker)

  list(
    universe = universe,
    prices = prices,
    returns = returns,
    signals = signals,
    spreads = spread_df,
    tail = tail_df,
    hofa = hofa,
    regimes = regimes,
    weights_raw = weights_raw,
    weights = weights,
    backtest = backtest
  )
}
