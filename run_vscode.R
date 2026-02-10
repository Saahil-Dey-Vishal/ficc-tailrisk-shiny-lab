#!/usr/bin/env Rscript

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
file_match <- args_full[startsWith(args_full, file_arg)]

if (length(file_match) > 0) {
  script_path <- normalizePath(sub(file_arg, "", file_match[1]), mustWork = TRUE)
  repo_dir <- dirname(script_path)
} else {
  repo_dir <- normalizePath(".", mustWork = TRUE)
}

message("Project: ", repo_dir)

cran <- "https://cloud.r-project.org"
options(repos = c(CRAN = cran))

required <- c(
  "shiny", "bslib", "DT", "ggplot2", "scales",
  "quantmod", "xts", "zoo", "moments",
  "dplyr", "tidyr"
)

optional <- c("bidask", "hofa", "echoice2", "portfolio.optimization", "rsims")

configure_project_library <- function() {
  project_lib <- file.path(repo_dir, ".r_libs")
  if (!dir.exists(project_lib)) {
    ok <- dir.create(project_lib, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(project_lib)) {
      stop("Unable to create project library: ", project_lib, call. = FALSE)
    }
  }

  .libPaths(c(project_lib, .libPaths()))
  message("Using library path: ", .libPaths()[1])
}

installed_names <- function() rownames(installed.packages())

install_missing <- function(pkgs, required_pkgs = TRUE) {
  missing <- setdiff(pkgs, installed_names())
  if (length(missing) > 0) {
    message("Installing: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"))
  }

  unresolved <- setdiff(pkgs, installed_names())
  if (length(unresolved) > 0) {
    msg <- paste("Missing packages after install:", paste(unresolved, collapse = ", "))
    if (required_pkgs) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
  }
}

update_if_outdated <- function(pkgs) {
  available <- tryCatch(available.packages(), error = function(e) NULL)
  if (is.null(available)) return(invisible(NULL))

  installed <- utils::installed.packages()
  outdated <- character(0)

  for (pkg in pkgs) {
    if (!pkg %in% rownames(installed)) next
    if (!pkg %in% rownames(available)) next

    local_v <- package_version(installed[pkg, "Version"])
    remote_v <- package_version(available[pkg, "Version"])
    if (local_v < remote_v) outdated <- c(outdated, pkg)
  }

  if (length(outdated) > 0) {
    message("Updating: ", paste(outdated, collapse = ", "))
    install.packages(outdated, dependencies = c("Depends", "Imports", "LinkingTo"))
  }
}

configure_project_library()
install_missing(required, required_pkgs = TRUE)
update_if_outdated(c("shiny", "bslib"))
install_missing(required, required_pkgs = TRUE)
install_missing(optional, required_pkgs = FALSE)

for (pkg in required) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

port <- 3838L
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && grepl("^[0-9]+$", args[1])) {
  port <- as.integer(args[1])
}

message("Starting app on http://127.0.0.1:", port)
setwd(repo_dir)
shiny::runApp(
  appDir = repo_dir,
  host = "127.0.0.1",
  port = port,
  launch.browser = TRUE
)
