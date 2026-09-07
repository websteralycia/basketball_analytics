## ============================================================
## build_profile.R — build and cache one play profile
## ------------------------------------------------------------
## The dashboard's League and Population dropdowns list whatever is
## cached under data/tidy/<league>/profiles/. This is what puts one
## there.
##
##   Rscript scripts/build_profile.R <league> <season> [population]
##
##   Rscript scripts/build_profile.R wnba 2026
##   Rscript scripts/build_profile.R wbb  2026 "Big East"
##
## `population` defaults to "national" -- right for the pro leagues,
## where there are no conferences to split. For college, pass a
## conference name.
##
## Slow: it pulls a full season of play-by-play. Minutes, not seconds.
## ============================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("usage: build_profile.R <league> <season> [population]", call. = FALSE)
}
league     <- args[1]
season     <- as.integer(args[2])
population <- if (length(args) >= 3) args[3] else "national"

GP_ROOT <- Sys.getenv("GAMEPREP_ROOT", unset = "")
if (!nzchar(GP_ROOT)) {
  GP_ROOT <- file.path(Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics"),
                       "11_gameprep_project")
}
source(file.path(GP_ROOT, "source_all.R"))

t0 <- Sys.time()
message(sprintf("Building %s %s '%s' ...", league, season, population))

prof <- build_team_play_profile(league = league, season = season,
                                population = population)

path <- profile_cache_path(population, season, league)
dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(prof, path)

mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
message(sprintf("Wrote %d rows for %d teams -> %s  (%s min)",
                nrow(prof), length(unique(prof$team)), path, mins))
