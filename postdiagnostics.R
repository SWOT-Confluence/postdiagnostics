source("input.R")
source("output.R")

#' Run FLPE diagnostics - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#'
#' @return dataframe of FLPE and SoS data
run_flpe_diagnostics <- function(input_dir) {
  # INPUT  
  # args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 20
  reach_files <- get_sos_file_flpe(reaches_json, input_dir, index)
  data <- get_data_flpe(reach_files$sos, reach_files$reach_id, input_dir)
  
  # PROCESSING
  # diag_data_flpe <- run_diagnostics_flpe(data)
  
  # OUTPUT
  # write_data_flpe(diag_data)
}

#' Run Integrator diagnostics - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#'
#' @return ?
run_integrator_diagnostics <- function(input_dir) {
  # INPUT
  # args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  basin_json <- "basin.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 4
  integrator_file <- get_integrator_file(basin_json, input_dir, index)
  data <- get_data_integrator(integrator_file)
  
  # PROCESSING
  # diag_data_integrator <- run_diagnostics(data)
  
  # OUTPUT
  # write_data_integrator(diag_data)
}

#' Diagnostics on FLPE discharge data
#'
#' @param data dataframe of SOS and FLPE data
#'
#' @return ??
flpe_diagnostics <- function(data) {
  ## Do Stuff
}

#' Diagnostics on Integrator discharge data
#'
#' @param data ??
#'
#' @return ??
integrator_diagnostics <- function(data) {
  ## Do Stuff
}