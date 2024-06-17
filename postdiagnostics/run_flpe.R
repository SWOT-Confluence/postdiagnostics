# Libraries
library(RNetCDF)
library(dplyr)
library(rjson)
library(reticulate)

# Program files
source("/app/postdiagnostics/input.R")
source("/app/postdiagnostics/postdiagnostics.R")
source("/app/postdiagnostics/output.R")

# Directories
start <- Sys.time()
input_dir <- file.path("/mnt", "data", "input", fsep=.Platform$file.sep)
flpe_dir <- file.path("/mnt", "data", "flpe", fsep=.Platform$file.sep)
output_dir <- file.path("/mnt", "data", "output", fsep=.Platform$file.sep)

# Command line arguments
args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 5) {
  tolerance <- strtoi(args[1])
  index <- strtoi(args[2]) + 1
  reaches_json <- args[3]
  s3_bucket <- args[4]
  local_bool <- TRUE
} else if (length(args) == 4) {
  tolerance <- strtoi(args[1])
  index <- strtoi(args[2]) + 1
  reaches_json <- args[3]
  s3_bucket <- args[4]
  local_bool <- FALSE
} else if (length(args) == 3) {
  tolerance <- strtoi(args[1])
  index <- strtoi(args[2]) + 1
  reaches_json <- args[3]
  s3_bucket <- "confluence-sos"
  local_bool <- FALSE
} else if (length(args) == 2) {
  tolerance <- strtoi(args[1])
  index <- strtoi(args[2]) + 1
  reaches_json <- "reaches.json"
  s3_bucket <- "confluence-sos"
  local_bool <- FALSE
} else {
  tolerance <- 0.25
  index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
  reaches_json <- "reaches.json"
  s3_bucket <- "confluence-sos"
  local_bool <- FALSE
}

# Run diagnostics
run_flpe_diagnostics(input_dir, flpe_dir, output_dir, reaches_json, index, tolerance, s3_bucket, local_bool)
end <- Sys.time()
print(paste0("Execution time: ", end - start))