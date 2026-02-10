`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

safe_scale <- function(x) {
  if (all(is.na(x))) return(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x - mean(x, na.rm = TRUE))
  (x - mean(x, na.rm = TRUE)) / s
}

as_xts_safe <- function(df, date_col = "date") {
  if (!date_col %in% names(df)) stop("date column missing")
  xts::xts(df[, setdiff(names(df), date_col), drop = FALSE], order.by = df[[date_col]])
}
