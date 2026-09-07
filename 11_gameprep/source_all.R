## ============================================================
## source_all.R — load the tidy layer
## ------------------------------------------------------------
## Until this project earns a DESCRIPTION file and becomes a proper
## package, this is how consumers load it:
##
##   source("~/Desktop/Basketball Analytics/11_gameprep_project/source_all.R")
##   stints <- get_lineup_stints(401857092, league = "wnba")
##
## The R/ and tests/testthat/ layout already matches what a package
## expects, so promoting it later needs no file moves.
##
## Root resolution follows the same convention as the rest of the
## project: GAMEPREP_ROOT wins, else <BBALL_HOME>/11_gameprep_project,
## else the Desktop copy.
## ============================================================

suppressMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(stringr)
})

gameprep_root <- Sys.getenv("GAMEPREP_ROOT", unset = "")
if (!nzchar(gameprep_root)) {
  bball_home <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
  gameprep_root <- file.path(bball_home, "11_gameprep_project")
}

local({
  r_dir <- file.path(gameprep_root, "R")
  if (!dir.exists(r_dir)) {
    stop("Cannot find the tidy layer at ", r_dir,
         "\nSet GAMEPREP_ROOT or BBALL_HOME to point at it.", call. = FALSE)
  }
  for (f in list.files(r_dir, pattern = "\\.R$", full.names = TRUE)) source(f)
})
