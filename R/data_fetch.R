fetch_weekly_ohlc <- function(tickers, from, to) {
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Package 'quantmod' is required for Yahoo Finance data.")
  }
  if (!requireNamespace("xts", quietly = TRUE)) {
    stop("Package 'xts' is required for weekly aggregation.")
  }

  ohlc_list <- list()
  adj_list <- list()

  for (tk in tickers) {
    xt <- tryCatch(
      quantmod::getSymbols(tk, src = "yahoo", from = from, to = to, auto.assign = FALSE, warnings = FALSE),
      error = function(e) NULL
    )

    if (is.null(xt)) next

    weekly_ohlc <- xts::to.weekly(xt, indexAt = "lastof", drop.time = TRUE)

    adj <- quantmod::Ad(xt)
    ep <- xts::endpoints(adj, on = "weeks")
    weekly_adj <- xts::period.apply(adj, INDEX = ep, FUN = function(x) x[NROW(x)])

    colnames(weekly_adj) <- tk
    ohlc_list[[tk]] <- weekly_ohlc
    adj_list[[tk]] <- weekly_adj

    Sys.sleep(0.05)
  }

  if (length(adj_list) == 0) stop("No data returned from Yahoo Finance.")

  prices <- do.call(merge, adj_list)
  list(prices = prices, ohlc = ohlc_list)
}
