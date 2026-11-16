# Reproducible R environment (run from the project root, in R 4.5.1)
#
# First time (creates renv.lock from the packages already installed and used by R/*.R):
#   source("setup_renv.R")   # renv::init() restarts R when it finishes
#   source("setup_renv.R")   # run it again to write sessionInfo.txt
#
# Anyone reproducing the analyses:
#   source("setup_renv.R")   # restores the packages recorded in renv.lock

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

if (!file.exists("renv.lock")) {
  renv::init()
} else {
  renv::restore()
  writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
}
