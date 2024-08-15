# Libraries
library(RNetCDF)
library(dplyr)
library(rjson)
library(reticulate)
library(optparse)

# Program files
source("/app/postdiagnostics/input.R")
source("/app/postdiagnostics/postdiagnostics.R")
source("/app/postdiagnostics/output.R")

# Directories
start <- Sys.time()
input_dir <- file.path("/mnt", "data", "input", fsep=.Platform$file.sep)
flpe_dir <- file.path("/mnt", "data", "flpe", fsep=.Platform$file.sep)
moi_dir <- file.path("/mnt", "data", "moi", fsep=.Platform$file.sep)
output_dir <- file.path("/mnt", "data", "output", fsep=.Platform$file.sep)

# Command line arguments
option_list <- list(
  make_option(c("-i", "--index"), type = "integer", default = NULL, help = "Index to run on"),
  make_option(c("-t", "--tolerance"), type = "integer", default = 0.25, help = "Tolerance value for stability check"),
  make_option(c("-b", "--current_bucket"), type = "character", default = "", help = "Bucket key to find the sos"),
  make_option(c("-r", "--reaches_json"), type = "character", default = "reaches.json", help = "Name of reaches.json"),
  make_option(c("-l", "--local_bool"), type = "logical", default = FALSE, help = "Name of reaches.json"),
  make_option(c("-p", "--previous_bucket"), type = "character", default = "confluence-sos", help = "Name of SoS bucket to pull previous results")
)
opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)
index <- opts$index + 1    # Add 1 to AWS 0-based index
tolerance <- opts$tolerance
current_bucket <- opts$current_bucket
reaches_json <- opts$reaches_json
local_bool <- opts$local_bool
previous_bucket <- opts$previous_bucket
print(paste("index: ", index))
print(paste("tolerance: ", tolerance))
print(paste("current_bucket: ", current_bucket))
print(paste("reaches_json: ", reaches_json))
print(paste("local_bool: ", local_bool))
print(paste("previous_bucket: ", previous_bucket))

# Run diagnostics
run_moi_diagnostics(input_dir, flpe_dir, moi_dir, output_dir, index, tolerance,
                    current_bucket, previous_bucket, local_bool)
end <- Sys.time()
print(paste0("Execution time: ", end - start))