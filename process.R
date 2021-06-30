source("input.R")
source("output.R")

#' Retrieves FLPE and SoS discharge data
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#'
#' @return dataframe of FLPE and SoS data
get_discharge_flpe <- function(input_dir) {
  
  args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 3
  reach_files <- get_sos_file_flpe(reaches_json, input_dir, index)
  return(get_data_flpe(reach_files$sos, reach_files$reach_id, input_dir))
}

#' Retrieves Integrator discharge data - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#'
#' @return ?
get_discharge_integrator <- function(input_dir) {
  args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 3
  
  ## TODO get and return integrator data to run diagnostics on
  # integrator_files <- get_reach_files_integrator(reaches_json, input_dir, index)
  # return(get_data_integrator())
}

#' Run diagnostics on FLPE discharge data
#'
#' @param data dataframe of SOS and FLPE data
#'
#' @return ??
run_diagnostics_flpe <- function(data) {
  ## Do Stuff
}

#' Run diagnostics on Integrator discharge data
#'
#' @param data ??
#'
#' @return ??
run_diagnostics_integrator <- function(data) {
  ## Do Stuff
}

input_dir <- "/home/nikki/Documents/confluence/workspace/diagnostics/post_data"    # CHANGE ME

# Run FLPE diagnostics
data <- get_discharge_flpe(input_dir)
# diag_data_flpe <- run_diagnostics_flpe(data)
# write_data_flpe(diag_data)

# Run Integrator diagnostics - not complete (unsure of integrator output)
# data <- get_discharge_integrator(input_dir)
# diag_data_integrator <- run_diagnostics(data)
# write_data_integrator(diag_data)