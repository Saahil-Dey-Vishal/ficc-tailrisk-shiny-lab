#!/usr/bin/env Rscript

repo_dir <- normalizePath(".", mustWork = TRUE)
message("Project: ", repo_dir)

cran <- "https://cloud.r-project.org"

required <- c(
  "shiny", "bslib", "DT", "ggplot2", "scales",
  "quantmod", "xts", "zoo", "moments",
  "dplyr", "tidyr"
)

optional <- c("bidask", "hofa", "echoice2", "portfolio.optimization", "rsims")

install_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  missing <- setdiff(pkgs, installed)
  if (length(missing) > 0) {
    message("Installing: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = cran)
  }
}

install_or_update <- function(pkgs) {
  available <- tryCatch(available.packages(repos = cran), error = function(e) NULL)
  if (is.null(available)) return(invisible(NULL))

  installed <- utils::installed.packages()
  outdated <- character(0)

  for (pkg in pkgs) {
    if (!pkg %in% rownames(installed)) next
    if (!pkg %in% rownames(available)) next

    local_v <- package_version(installed[pkg, "Version"])
    remote_v <- package_version(available[pkg, "Version"])
    if (local_v < remote_v) {
      outdated <- c(outdated, pkg)
    }
  }

  if (length(outdated) > 0) {
    message("Updating: ", paste(outdated, collapse = ", "))
    install.packages(outdated, repos = cran)
  }
}

install_missing(required)
install_or_update(c("shiny", "bslib"))

for (pkg in required) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

install_missing(optional)

port <- 3838L
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && grepl("^[0-9]+$", args[1])) {
  port <- as.integer(args[1])
}

message("Starting app on http://127.0.0.1:", port)
shiny::runApp(
  appDir = repo_dir,
  host = "127.0.0.1",
  port = port,
  launch.browser = TRUE
)
