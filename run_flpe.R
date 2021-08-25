source("input.R")
source("postdiagnostics.R")
source("output.R")

library(RNetCDF)
library(dplyr)
library(rjson)

# Directories
start <- Sys.time()
input_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                       "diagnostics", "post_data", "input", "input", fsep=.Platform$file.sep)
flpe_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                      "diagnostics", "post_data", "input", "flpe", fsep=.Platform$file.sep)
output_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                        "diagnostics", "post_data", "output", "reach", fsep=.Platform$file.sep)

# Command line arguments
args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 2) {
  reaches_json <- args[2]
  tolerance <- args[1]
} else if (length(args) == 1) {
  reaches_json <- "reaches.json"
  tolerance <- args[1]
} else {
  reaches_json <- "reaches.json"
  tolerance <- 0.25
}

# Run diagnostics
# index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
index <- 14
run_flpe_diagnostics(input_dir, flpe_dir, output_dir, index, tolerance)
end <- Sys.time()
print(paste0("Execution time: ", end - start))